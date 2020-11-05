#devtools::install_github("timjmiller/wham", dependencies=TRUE)
library(wham)
source("demonstrations/gavin/wham_mse_functions.R")
source("demonstrations/tim/wham_retro_functions.R")
source("demonstrations/gavin/base_input.R")

#Retro type definitions
#size of retro: ~0.5
#Catch/F:  2.75 times higher than reported. Change 10 years from end of base period
# results in Mohn's rho approx 0.50 for SSB, -0.34 for F, 0.33 for Recruitment
#fit SCAA model to estimate retro.

wham_like_asap = function(nsim = 1){

  input = get_base_input()

  na = input$na #number of ages
  nf=input$nf #number of fleets (we only need 1)
  ni=input$ni #number of indices 
  #input$recruit_model = 3 #Beverton-Holt
    
  #a50 = 5 and slope = 1 for logistic selectivity for each fleet and index
  #sel.list=list(model=rep("logistic",ni+nf), re=rep("none",ni+nf), initial_pars=lapply(1:(ni+nf), function(x) c(5,1)))
  if(nf + ni != 3) stop("number of fleets must = 1 and number of indices must = 2")
  sel.list=list(model=rep("logistic",ni+nf), re=rep("none",ni+nf), 
    initial_pars= list(
      c(3.57,1), #fishery (see factorial pruning)
      c(1.8, 1/6), #survey 1 (see factorial pruning)
      c(1.2, 1/5.5))) #survey 2 (see factorial pruning)

  #AR1 deviations of rececruitment only over time. To include abundances at older ages as random effects use "rec+1"
  NAA.list = list(sigma='rec',cor='ar1_y')
  #NAA.list = list(sigma='rec',cor='iid') #this would make recruitment deviations iid.
  #```

  #```{r base-period}
  #set up projections to repopulate 
  proj.list = list(n.yrs=length(input$modyears), use.last.F=TRUE, use.avg.F=FALSE, use.FXSPR=FALSE,
                                                proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
                                                cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL, cont.Mre=NULL)

  #set up initial numbers at age according to equilibrium assumptions as determined by IBMWG
  h = input$mean_rec_pars[1]
  R0 = input$mean_rec_pars[2]
  sel = 1/(1+exp(-sel.list$initial_pars[[1]][2]*(1:na - sel.list$initial_pars[[1]][1])))
  spr0 = wham:::get_SPR(0, M=input$M, sel, mat=input$maturity, waassb=input$waa_catch, fracyrssb=input$fracyr_spawn, at.age = FALSE)
  a = 4*h/((1-h)*spr0)
  b = (a - 1/spr0)/R0
  F1 = input$F[1]
  sprF1 = wham:::get_SPR(F1, M=input$M, sel, mat=input$maturity, waassb=input$waa_catch, fracyrssb=input$fracyr_spawn, at.age = FALSE)
  nprF1 = wham:::get_SPR(F1, M=input$M, sel, mat=rep(1,na), waassb=rep(1,na), fracyrssb=input$fracyr_spawn, at.age = TRUE)
  R_F1 = (a - 1/sum(sprF1))/b
  input$N1 <- R_F1*nprF1 #Initial numbers at age

  #generate the input for fit_wham. Data (indices, catch) are not populated though.
  om = prepare_wham_om_input(input, recruit_model = input$recruit_model, selectivity=sel.list, NAA_re = NAA.list)#, proj.opts = proj.list)
  om$data$Fbar_ages = 10
  #source("~/work/IBMWG/wham/sandbox/prepare_wham_om_input.R")
  om_wham = fit_wham(om, do.fit = FALSE)
  seed = 8675309
  set.seed(seed)
  simsets = lapply(1:nsim, function(x) om_wham$simulate(complete = TRUE))
  scaa_input = input
  scaa_input$use_steepness = 0
  scaa_input$mean_rec_pars = scaa_input$mean_rec_pars[2]
  scaa_input = prepare_wham_om_input(scaa_input, recruit_model = 2, selectivity=sel.list)
  scaa_input$data$Fbar_ages = 10
  #fit = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE, MakeADFun.silent = TRUE)

  simres = list()
  for(i in 1:length(simsets))
  {
    print(paste0("i = ", i))
    tinput = scaa_input
    tinput$data = simsets[[i]]
    tinput$data$Fbar_ages = 10
    tinput$data$use_steepness = 0
    tinput$data$recruit_model = 2
    tfit = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE, MakeADFun.silent = TRUE)
    simres[[i]] = tfit$rep
  }
  return(list(simsets, simres))
}

x = wham_like_asap()

#liz code to setup asap
names(x[[1]][[1]])

whamout <- x[[1]][[1]]
fd<-('C:/Users/liz.brooks/liz/R/MySim/funcs/')
source(paste(fd, "setup_asap_wcoast_func_v2.r", sep="") )
wd <- "C:/Users/liz.brooks/liz/NFT/ASAP/compare_wham_asap/ibmwg/"
pd <- "C:/Users/liz.brooks/liz/NFT/ASAP/compare_wham_asap/ibmwg/plots/"
#some lazy decisions, not generalized yet:
# waa pointers (they rack matrices, i stack them)
# sel blocks and sel types for fishery and indices are in same object in wham, i'm making a one-off specification
# setup selectivity matrices here since wham doesn't use them
sel.mats1<- cbind( seq(1,whamout$n_ages)/whamout$n_ages, trunc(seq(1,4, length.out=whamout$n_ages)),
                   rep(0, whamout$n_ages), rep(1.0, whamout$n_ages)  )
sel.mats1[whamout$n_ages,2] <- -4
sel.mats2 <- cbind( c(3.5, 0.75), c(1,3), rep(0,2), rep(1.0, 2)) #a50, slope
sel.mats3<- cbind( c( whamout$n_ages/3, 2/whamout$n_ages, whamout$n_ages/2, 3/whamout$n_ages),c(2,2,3,3), rep(0,4), rep(1.0,4) ) 
sel.mats<-rbind(sel.mats1, sel.mats2, sel.mats3)  
ind.sel.mats <- rbind(sel.mats, sel.mats) # just use same par guesses for fishery and indices
# fleet age (can't find in wham) so assuming all ages
F.rep.ages <- which(whamout$mature[whamout$n_years_model,]==1)[1:2]
# default turn off discards, release mortality,etc
caa.mat <- cbind(whamout$catch_paa[1,,], whamout$agg_catch)
daa.mat <- matrix(0, nrow=whamout$n_years_model, ncol=(whamout$n_ages+1), byrow=T)
rel.prop <- matrix(0,nrow=(whamout$n_fleets*whamout$n_years_model), ncol=whamout$n_ages, byrow=T)
# no cpue indices (fish.ind spec)
# ages that indices apply to (assume it is all ages)
ind.sig <- whamout$agg_index_sigma
ind.cv1 <- sqrt(exp(ind.sig[,1]*ind.sig[,1])-1)
ind.cv2 <- sqrt(exp(ind.sig[,2]*ind.sig[,2])-1)
#ind.mat <- matrix(NA, nrow=(whamout$n_years_model*whamout$n_indices), ncol=(whamout$n_ages+4) , byrow=T )
ind.mat1 <- cbind(seq(1, whamout$n_years_model), whamout$agg_indices[,1], ind.cv1, whamout$index_paa[1,,], whamout$index_Neff[,1] )
ind.mat2 <- cbind(seq(1, whamout$n_years_model), whamout$agg_indices[,2],ind.cv2,  whamout$index_paa[2,,], whamout$index_Neff[,2] )
ind.mat <- rbind(ind.mat1, ind.mat2)
recr.cv <- 0.5
catch.cv <- 0.1
disc.cv <-  0.3
naa.y1 <- (whamout$catch_paa[1,1,]*(whamout$agg_catch[1])/mean(whamout$waa[1,1,]))/seq(0.1,1, length.out=whamout$n_ages)
Fmult.y1 <- 0.15
q.y1 <- apply(whamout$agg_indices, 2, mean)/mean(whamout$agg_catch)
SSB0.guess <- 3*max(whamout$agg_catch)
fleet.names <- paste('#$fleet', seq(1,whamout$n_fleets), sep='')
survey.names <- paste('#$survey', seq(1,whamout$n_indices), sep='')
disc.flag <- disc.flag<-rep(F, whamout$n_years_model) # if FALSE, then don't print Neff=0 and CV=1 for discards
proj.specs <- matrix(NA, nrow=2, ncol=5)
proj.specs[,1] <- c(whamout$n_years_model+1, whamout$n_years_model+2) #dummy specs for 2 yr projection
proj.specs[,2] <- rep(-1, 2)
proj.specs[,3]<- c(1,3)
proj.specs[,4] <-  c(150,-99)
proj.specs[,5] <-  rep(0,2)
  
tmp <- setup.asap.w(wd=wd, pd=pd, model.id="wham2asap_test.dat", nyears=whamout$n_years_model, first.year=1, 
                    asap.nages=whamout$n_ages, nfleets=whamout$n_fleets,
                    nselblks=1, n.ind.avail=whamout$n_indices, 
                    M.mat=matrix(rep(exp(whamout$M0), whamout$n_ages), nrow=whamout$n_years_model, ncol=whamout$n_ages, byrow=T), 
                    fec.opt=0, t.spawn=whamout$fracyr_SSB[1], mat.mat=whamout$mature, 
                    #lazy specification of waa mats in next line
                    n.waa.mats=1, waa.array=array(whamout$waa[1,,], dim=c(whamout$n_years_model, whamout$n_ages,1)), waa.pointer.vec=rep(1,6),
                    sel.blks=whamout$selblock_years[,1], sel.types=whamout$selblock_models[1], sel.mats=sel.mats, 
                    fleet.age1=1, fleet.age2=whamout$n_ages, 
                    F.report.ages=F.rep.ages, F.report.opt=1, 
                    like.const=0, rel.mort.fleet=rep(0,whamout$n_fleets), caa.mats=caa.mat, daa.mats=daa.mat,
                    rel.prop=rel.prop, units.ind.agg=whamout$units_indices, units.ind.aa=whamout$units_index_paa,
                    time.ind=trunc(whamout$fracyr_indices[1,]*12), 
                    fish.ind=rep(-1, whamout$n_indices), sel.ind=whamout$selblock_models[2:3], 
                    ind.age1=rep(1, whamout$n_indices), ind.age2=rep(whamout$n_ages, whamout$n_indices), 
                    ind.use=whamout$use_indices[whamout$n_years_model,], ind.sel.mats=ind.sel.mats, ind.mat=ind.mat, 
                    p.Fmult1=1, p.Fmult.dev=3, p.recr.dev=4, p.N1=2, p.q1=1, p.q.dev=-1, p.SR=2, p.h=3,
                    recr.CV=rep(recr.cv,whamout$n_years_model), lam.ind=rep(1,whamout$n_indices),
                    lam.c.wt=rep(1,whamout$n_fleets), lam.disc=rep(0,whamout$n_fleets),
                    catch.CV=rep(catch.cv, whamout$n_years_model), disc.CV=rep(disc.cv, whamout$n_years_model), 
                    Neff.catch=whamout$catch_Neff, Neff.disc=(0*whamout$catch_Neff), 
                    lam.Fmult.y1=rep(0, whamout$n_fleets), CV.Fmult.y1=rep(1, whamout$n_fleets), 
                    lam.Fmult.dev=rep(0,whamout$n_fleets), CV.Fmult.dev=rep(1,whamout$n_fleets), 
                    lam.N1.dev=0, CV.N1.dev=1, lam.recr.dev=1,
                    lam.q.y1=rep(0, whamout$n_indices), CV.q.y1=rep(1, whamout$n_indices), lam.q.dev=rep(0, whamout$n_indices),
                    CV.q.dev=rep(1, whamout$n_indices), lam.h=0, CV.h=1, lam.SSB0=0, CV.SSB0=1, 
                    naa.y1=naa.y1, Fmult.y1=Fmult.y1, q.y1=q.y1, SSB0=SSB0.guess, h.guess=0.6, F.max=5, ignore.guess=0,
                    do.proj=0, fleet.dir=rep(1, whamout$n_fleets), proj.yr=(whamout$n_years_model+2), proj.specs=proj.specs, 
                    do.mcmc=0, mcmc.nyr.opt=0, mcmc.nboot=1000, mcmc.thin=200, mcmc.seed=5230547, 
                    recr.agepro=0, recr.start.yr=trunc(whamout$n_years_model/3), 
                    recr.end.yr=(whamout$n_years_model-3), test.val=-23456, 
                    fleet.names=fleet.names, survey.names=survey.names, disc.flag=disc.flag )   

asap <-dget(file=paste0(wd,"wham2asap_test.rdat"))

#compare output
years <- seq(1, whamout$n_years_model)
ages <- seq(1, whamout$n_ages)
wc <- "black" #color for wham plotting
ac <- "#0099FF" # color for asap plotting

pdf(onefile=T, file=paste0(pd, "whamVSasap_plots.pdf"))
#SSB
plot(years, whamout$SSB, type='l', col=wc, lwd=2, lty=1, xlab="year", ylab='ssb', ylim=c(0, max(whamout$SSB, asap$SSB)))
lines(years, asap$SSB, col=ac, lwd=3, lty=4)

# recr
plot(years, whamout$NAA[,1], type='l', col=wc, lwd=2, lty=1, xlab="year", ylab='Recruitment', ylim=c(0, max(whamout$NAA[,1], asap$N.age[,1])) )
lines(years, asap$N.age[,1], col=ac, lwd=3, lty=4)

#S-R
plot(whamout$SSB, whamout$NAA[,1], type='p', pch=1, col=wc, xlab='SSB', ylab="Recr", xlim=c(0, max(whamout$SSB, asap$SSB)), ylim=c(0, max(whamout$NAA[,1], asap$N.age[,1])))
points(asap$SSB, asap$N.age[,1], pch=20, col=ac)

# Fmult
wham.fmult <- apply(whamout$FAA_tot, 1, max)
plot(years, wham.fmult, type='l', col=wc, lwd=2, lty=1, xlab="year", ylab='Fmult', ylim=c(0, max(wham.fmult, asap$fleet.Fmult)) )
lines(years, asap$fleet.Fmult, col=ac, lwd=3, lty=3)

#Fish Sel
plot(ages, whamout$selAA[[1]][1,], type='l', col=wc, lwd=2, lty=1, xlab='Age', ylab='Fishery Sel')
lines(ages, asap$fleet.sel.mats[[1]][1,], col=ac, lwd=3, lty=4)

#Ind.Sel
par(mfrow=c(1,2))
plot(ages, whamout$selAA[[2]][1,], type='l', col=wc, lwd=2, lty=1, xlab='Age', ylab='Fishery Sel')
lines(ages, asap$index.sel[1,], col=ac, lwd=3, lty=4)

plot(ages, whamout$selAA[[3]][1,], type='l', col=wc, lwd=2, lty=1, xlab='Age', ylab='Fishery Sel')
lines(ages, asap$index.sel[2,], col=ac, lwd=3, lty=4)

dev.off()
 