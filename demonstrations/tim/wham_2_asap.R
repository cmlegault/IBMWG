
wham_2_asap <- function(sim)
{
  whamout <- sim
  fd<-('~/work/IBMWG/demonstrations/tim/')
  source(paste(fd, "setup_asap_wcoast_func_v2.r", sep="") )
  wd <- pd <- '~/work/IBMWG/demonstrations/tim/'
  #pd <- "C:/Users/liz.brooks/liz/NFT/ASAP/compare_wham_asap/ibmwg/plots/"
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
  F.rep.ages <- 10 #which(whamout$mature[whamout$n_years_model,]==1)[1:2]
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
    lam.N1.dev=0, CV.N1.dev=1, 
    lam.recr.dev=0,
    lam.q.y1=rep(0, whamout$n_indices), CV.q.y1=rep(1, whamout$n_indices), lam.q.dev=rep(0, whamout$n_indices),
    CV.q.dev=rep(1, whamout$n_indices), lam.h=0, CV.h=1, lam.SSB0=0, CV.SSB0=1, 
    naa.y1=naa.y1, Fmult.y1=Fmult.y1, q.y1=q.y1, SSB0=SSB0.guess, h.guess=0.6, F.max=5, ignore.guess=0,
    do.proj=0, fleet.dir=rep(1, whamout$n_fleets), proj.yr=(whamout$n_years_model+2), proj.specs=proj.specs, 
    do.mcmc=0, mcmc.nyr.opt=0, mcmc.nboot=1000, mcmc.thin=200, mcmc.seed=5230547, 
    recr.agepro=0, recr.start.yr=trunc(whamout$n_years_model/3), 
    recr.end.yr=(whamout$n_years_model-3), test.val=-23456, 
    fleet.names=fleet.names, survey.names=survey.names, disc.flag=disc.flag )   
}

#asap <-dget(file=paste0(wd,"wham2asap_test.rdat"))

