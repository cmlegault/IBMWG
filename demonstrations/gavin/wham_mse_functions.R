#functions to run WHAM MSE

### run a given wham MSE simulation
## will want to abstract more of the set up from this as it's the same for each simulation
do_wham_mse_sim <- function(seed = 42, input = NULL) {  #JJD
  # retro misspecficiation is invoked if retro_type is "M" or "Catch"
  # function does 1 simulation for the wham mse
  # i.e. 1 realization of the base and projection period
  # add more arguments so can abstract more of scenario setup from the inner function
  # now almost all set up is done in get_base_input
  if(is.null(input)) stop("need an input object like that provided by get_base_input()")
  #print(adv.yr)
  
  adv.yr <- input$adv.yr
  nprojyrs <- input$nprojyrs
  retro_type <- input$retro_type
  n_selblocks <- input$n_selblocks
  Fhist <- input$Fhist
  
  # GF - had to copy these from input specs, mod so part of arguments
  #na = input$na #number of ages
  #nf=input$nf #number of fleets (we only need 1)
  #ni=input$ni #number of indices 
  #input$recruit_model = 3 #Beverton-Holt
    
  #a50 = 5 and slope = 1 for logistic selectivity for each fleet and index
  #sel.list=list(model=rep("logistic",ni+nf), re=rep("none",ni+nf), initial_pars=lapply(1:(ni+nf), function(x) c(5,1)))
  #if(nf + ni != 3) stop("number of fleets must = 1 and number of indices must = 2")
  #sel.list=list(model=rep("logistic",ni+nf), re=rep("none",ni+nf), 
  #  initial_pars= list(
  #    c(3.57,1), #fishery (see factorial pruning)
  #    c(1.8, 1/6), #survey 1 (see factorial pruning)
  #    c(1.2, 1/5.5))) #survey 2 (see factorial pruning)

  #AR1 deviations of rececruitment only over time. To include abundances at older ages as random effects use "rec+1"
  #NAA.list = list(sigma='rec',cor='ar1_y')
  #NAA.list = list(sigma='rec',cor='iid') #this would make recruitment deviations iid.
  #```

  #```{r base-period}
  #set up projections to repopulate 
  #proj.list = list(n.yrs=length(input$modyears), use.last.F=TRUE, use.avg.F=FALSE, use.FXSPR=FALSE,
  #                                              proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
  #                                              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL, cont.Mre=NULL)

  #set up initial numbers at age according to equilibrium assumptions as determined by IBMWG
  #h = input$mean_rec_pars[1]
  #R0 = input$mean_rec_pars[2]
  #sel = 1/(1+exp(-sel.list$initial_pars[[1]][2]*(1:na - sel.list$initial_pars[[1]][1])))
  #spr0 = wham:::get_SPR(0, M=input$M, sel, mat=input$maturity, waassb=input$waa_catch, fracyrssb=input$fracyr_spawn, at.age = FALSE)
  #a = 4*h/((1-h)*spr0)
  #b = (a - 1/spr0)/R0
  #F1 = input$F[1]
  #sprF1 = wham:::get_SPR(F1, M=input$M, sel, mat=input$maturity, waassb=input$waa_catch, fracyrssb=input$fracyr_spawn, at.age = FALSE)
  #nprF1 = wham:::get_SPR(F1, M=input$M, sel, mat=rep(1,na), waassb=rep(1,na), fracyrssb=input$fracyr_spawn, at.age = TRUE)
  #R_F1 = (a - 1/sum(sprF1))/b
  #input$N1 <- R_F1*nprF1 #Initial numbers at age

  #generate the input for fit_wham. Data (indices, catch) are not populated though.
  #x = prepare_wham_om_input(input, recruit_model = input$recruit_model, selectivity=input$sel.list, NAA_re = input$NAA.list, proj.opts = input$proj.list)
  #x$data$Fbar_ages = 10 #not sure if this is needed anywhere, but I did need it for examining retro values.
  
  
  #NEED TO PROVIDE INCORRECT M to MSE, if M is incorrect!
  observed_om_input = input
  if(retro_type == "M"){
      if(Fhist == 1 & n_selblocks == 1) Mscale = 1.6
      if(Fhist == 2 & n_selblocks == 1) Mscale = 1.8
      if(Fhist == 1 & n_selblocks == 2) Mscale = 1.6
      if(Fhist == 2 & n_selblocks == 2) Mscale = 1.8 
      #true operating model knows correct M
      true_om_input = change_M_om(observed_om_input, M_new_ratio = Mscale, n_ramp_years = 10, year_change = 2009) 
  } else true_om_input = observed_om_input #no M mis-specifcation
  
  observed_om = fit_wham(observed_om_input, do.fit = FALSE)
  #observed_sim = observed_om$simulate(complete=TRUE)
  true_om = fit_wham(true_om_input, do.fit = FALSE)
  set.seed(seed) 
  #simulated data and other report items from true operating model
  true_sim = true_om$simulate(complete= TRUE)
  #put data and simulated random parameters (Recruitment) into operating model, so true_om projections will be consistent with simulations
  true_om$env$data = true_sim[names(true_om_input$data)]
  #put in correct recruitment series
  ind = which(names(true_om$env$par) == "log_NAA")
  true_om$env$par[ind] = true_sim$log_NAA[,1] 
  set.seed(seed) 
  true_sim = true_om$simulate(complete= TRUE)
  if(retro_type == "Catch"){
    if(Fhist == 1 & n_selblocks == 1) Cscale = 5
    if(Fhist == 2 & n_selblocks == 1) Cscale = 2.5
    if(Fhist == 1 & n_selblocks == 2) Cscale = 5
    if(Fhist == 2 & n_selblocks == 2) Cscale = 2.25
    observed_sim = change_catch_sim(sim = true_sim, catch_ratio = 1/Cscale, year_change = 2010, years = 1970:2019)
  } 
  else observed_sim = true_sim
  #put in wrong M, if necessary
  observed_sim$MAA = observed_om$report()$MAA
  observed_sim = get.IBM.input(y=observed_sim,i=0) #JJD; this adds to the y list stuff needed for index methods 
  
  #GF modify the IBM options based on the scenario
  observed_sim$expand_method <- input$expand_method
  observed_sim$M_CC_method <- input$M_CC_method
  

  sim_data_series = list(observed_sim)
  observed_rep = observed_om$report()
  SSBlim = exp(observed_rep$log_SSB_MSY[1]) 
  Flim = exp(observed_rep$log_FMSY[1]) 
  
  #incorrect reference points if M is mis-specified
  refpts <- list(SSBlim = SSBlim,
                 Flim = Flim)
  catch_advice=observed_om_input$IBM(y=observed_sim) #JJD
  advice <- list(catch_advice) 
  # if(length(catch_advice)>1){
  #   catch_advice=catch_advice$proj.catch ##needed for AIM because run.aim function returns bunch of stuff
  # }
  ## GF: not needed so long as first object in returned list is the catch advice (which in AIM it is)

  #catch_advice <- rep(catch_advice,adv.yr) #JJD
  assess_years <- seq(1,(nprojyrs-adv.yr+1),by=adv.yr)
  nproj <-length(assess_years)
    
  #for(i in assess_years) #JJD
  for (i in 1:nproj)  
  #for (i in 1:2)  
  {
    year <- assess_years[i]
    proj_type = 5
    true_om$env$data$proj_Fcatch[year:(year+adv.yr-1)] = ifelse(retro_type=="Catch",Cscale,1) * advice[[i]] #TJM, need to make catch higher than quota if catch misspecified
    true_om$env$data$proj_F_opt[year:(year+adv.yr-1)] = proj_type #Specify Catch #JJD,TJM
    set.seed(seed)
    true_sim = true_om$simulate(complete = TRUE)
    #put data and simulated random parameters (Recruitment) into operating model, so true_om projections will be consistent with simulations
    true_om$env$data = true_sim[names(true_om_input$data)]
    #put in correct recruitment series
    ind = which(names(true_om$env$par) == "log_NAA")
    true_om$env$par[ind] = true_sim$log_NAA[,1] 
    set.seed(seed) 
    true_sim = true_om$simulate(complete= TRUE)
    #true_rep = true_om$report()
    #Check to see if F brake is needed, if so change projection type to F = 2 for the next set of projection years
    nextF = true_sim$FAA_tot[true_om_input$data$n_years_model+year:(year+adv.yr-1),10]
    if(any(is.na(nextF)) | any(nextF>2)) {
      advice[[i]] = 2
      proj_type = 4
      true_om$env$data$proj_Fcatch[year:(year+adv.yr-1)] = advice[[i]] #TJM
      true_om$env$data$proj_F_opt[year:(year+adv.yr-1)] = proj_type #Specify Catch #JJD,TJM
      set.seed(seed)
      true_sim = true_om$simulate(complete = TRUE)
      #put data and simulated random parameters (Recruitment) into operating model, so true_om projections will be consistent with simulations
      true_om$env$data = true_sim[names(true_om_input$data)]
      #put in correct recruitment series
      ind = which(names(true_om$env$par) == "log_NAA")
      true_om$env$par[ind] = true_sim$log_NAA[,1] 
      set.seed(seed) 
      true_sim = true_om$simulate(complete= TRUE)
      #true_rep = true_om$report()
      nextF = true_sim$FAA_tot[true_om_input$data$n_years_model+year:(year+adv.yr-1),10]
      #print("F too high")
      #print(nextF)
    }
    observed_om$env$data$proj_Fcatch[year:(year+adv.yr-1)] = advice[[i]] #TJM, incorrect catch projected if it is ever used.
    observed_om$env$data$proj_F_opt[year:(year+adv.yr-1)] = proj_type #Specify Catch #JJD,TJM
    #observed_rep = observed_om$report()
    #set.seed(seed)
    #observed_sim = observed_om$simulate(complete = TRUE)
    #sim_data_series[[i+1]] = true_om$simulate(complete = TRUE)
    if(retro_type == "Catch"){
      if(Fhist == 1 & n_selblocks == 1) Cscale = 5
      if(Fhist == 2 & n_selblocks == 1) Cscale = 2.5
      if(Fhist == 1 & n_selblocks == 2) Cscale = 5
      if(Fhist == 2 & n_selblocks == 2) Cscale = 2.25      
      observed_sim = change_catch_sim(sim = true_sim, catch_ratio = 1/Cscale, year_change = 2010, years = 1970:2019)
      #sim_data_series[[i+1]] = change_catch_sim(sim = sim_data_series[[i+1]], catch_ratio = 1/Cscale, year_change = 2010, years = 1970:2019)
    } 
    else observed_sim = true_sim

    #put in wrong M, if necessary
    observed_sim$MAA = observed_om$report()$MAA
    observed_sim = get.IBM.input(y=observed_sim,i=year) #JJD; GF
    sim_data_series[[i+1]] = observed_sim
    # lines(temp$years_full[1:(temp$input$data$n_years_model+i)], sim_data_series[[i]]$SSB[1:(temp$input$data$n_years_model+i)], col = i)
    #do assessment method (AIM, ASAP, etc.)
      #Example for fitting WHAM assessment model in feedback period
      #tdat = input_i
      #tdat$data = sim_data_series[[i]]
      #y = fit_wham(tdat, do.osa = FALSE, do.retro = FALSE)
      #sim_data_series[[i]]$wham_fit = y
      #catch_advice = ifelse(sim_data_series[[i]]$wham_fit$rep$SSB[temp$input$data$n_years_model+i]>220000, 1.1, 0.9)* sim_data_series[[i]]$pred_catch[temp$input$data$n_years_model+i]
    #important_assessment_result <- run_assesssment_method(sim_data)
    #catch_advice <- get_catch_advice(important_assessment_result)
    #take up where we left off
    #set catch to catch advice
    #catch_advice = ifelse(sim_data_series[[i+1]]$SSB[temp$input$data$n_years_model+i]>SSBlim, Flim, 0.9* sim_data_series[[i+1]]$FAA_tot[temp$input$data$n_years_model+i,na])* Flim
    #sim_data_series[[i+1]] = get.IBM.input(y=sim_data_series[[i+1]],i=year) #JJD; GF
    catch_advice = input$IBM(y=sim_data_series[[i+1]]) #JJD
    #if(year == 1) {
    #  print(sim_data_series[[i+1]]$FAA_tot[50+year:(year+adv.yr-1),10])
    #  stop()
    #}
    #if(length(catch_advice)>1){
    #  catch_advice=catch_advice$proj.catch ##needed for AIM because run.aim function returns bunch of stuff
    #}
    # GF don't think we need this; catch_advice=rep(catch_advice,adv.yr) #JJD
    #advice[(i+adv.yr):(i+(2*adv.yr)-1)] <- catch_advice
    advice[[i+1]] <- catch_advice
  }

  results <- list(sim_data_series = sim_data_series[[nproj+1]], #GF #[[nprojyrs]], #JJD
                  advice = advice,
                  refpts = refpts,
                  input = true_om$input,
                  observed_om = observed_om,
                  true_om = true_om,
                  true_sim = true_sim,
                  observed_sim = observed_sim,
                  #observed_rep = observed_rep, 
                  seed = seed,
                  input = input)

  return(results)
  
}  #end function do_wham_mse_sim


#################JJD
get.IBM.input<-function(y=NULL,i=NULL){
  #	should create single index that is consistent for all index based methods
  #	combining base period and projection period into single object
  y$seasonal_index<-rbind(y$agg_indices, y$agg_indices_proj)	#	seasonal indices as two columns.  rbind to combine base period with feedback period
  y$seasonal_index<-y$seasonal_index[1:(length(y$agg_catch)+i),]
  #took mean of fall 2019 and spring 2020 as agreed on July 2 meeting
  temp.index<-data.frame(spr=c(y$seasonal_index[,1],0),fall=c(0,y$seasonal_index[,2]))
  y$index<-rowMeans(temp.index[1:(nrow(temp.index)-1),])	#	check:Should this start at row 2?,  I had it start at row one so that the number of years stays consistent with catch even though year one only has one survey, not two with this particular method
  #y$index<-y$index[1:i]
  
  y$years<-seq(1,length(y$index),1)
  
  #	should create single catch that is consistent for all index based methods
  y$catch<-rbind(y$agg_catch, y$agg_catch_proj)	#assuming one fleet
  y$catch<-y$catch[1:(length(y$agg_catch)+i)]
  #	numbers at age in each survey
  obs_survey_NAA_func<-function(y){
    base_yr<-nrow(y$agg_indices)	#	yrs in base period
    proj_yr<-nrow(y$agg_indices_proj)	#	yrs in feedback period, updates every run
    
    #	base period and projections are two different objects within the list, index_paa is 3D array (first dimension is which survey)
    #	survey N = survey B/(prop at age in N * weight at age)
    survey_one_N_base<-y$agg_indices[,1]/rowSums(y$index_paa[1,,]*y$waa[y$waa_pointer_indices[1],1:base_yr,])	
    survey_two_N_base<-y$agg_indices[,2]/rowSums(y$index_paa[2,,]*y$waa[y$waa_pointer_indices[2],1:base_yr,])
    
    survey_one_N_proj<-y$agg_indices_proj[,1]/rowSums(y$index_paa_proj[1,,]*y$waa[y$waa_pointer_indices[1],(base_yr+1):sum(base_yr,proj_yr),])	#	base period and projections are two different objects within the list, index_paa is 3D array 
    survey_two_N_proj<-y$agg_indices_proj[,2]/rowSums(y$index_paa_proj[2,,]*y$waa[y$waa_pointer_indices[2],(base_yr+1):sum(base_yr,proj_yr),])
    
    #	survey NAA = survey N * proportion at age in N
    caa_survey_one_base<-survey_one_N_base*y$index_paa[1,,]
    caa_survey_two_base<-survey_two_N_base*y$index_paa[2,,]
    
    caa_survey_one_proj<-survey_one_N_proj*y$index_paa_proj[1,,]
    caa_survey_two_proj<-survey_two_N_proj*y$index_paa_proj[2,,]
    
    #	combining base period and projection
    caa_survey_one<-rbind(caa_survey_one_base,caa_survey_one_proj)
    caa_survey_two<-rbind(caa_survey_two_base,caa_survey_two_proj)
    
    caa_survey<-array(c(caa_survey_one,caa_survey_two),dim=c(nrow(caa_survey_one),ncol(caa_survey_one),2)  )
    caa_survey
  }	#	end function
  
  y$index_naa<-obs_survey_NAA_func(y)
  y$index_naa<-y$index_naa[1:(length(y$agg_catch)+i),,]
  #print(y$index_naa)
  #	create single natural mortality that is consistent for all index based methods
  y$M<-mean(y$MAA[1,])	#	Plan is for natural mortality to be constant with age, but possibly exhibit a ramp at some point in time series.
  #	WHAM output enables age specific and year specific natural mortality
  #	plan is to simply take M in year one of the base period, M prior to the ramp and average across years, but all the years should be the same. 
  
  #	check:catchability - currently q is time invariant, but at some point WHAM will likely output annual q for the two surveys and have a ramp
  #	If there is an annual q  it takes the first year of the base period
  if(is.null(nrow(y$q))==TRUE){
    y$init_q<-y$q
  }
  if(is.null(nrow(y$q))==FALSE){
    y$init_q<-y$q[1,]
  }
  
  y=IBM.options(y=y)
  
  return(y)
}

#IBM functions
#-----------------------------------------

get.stable.period <- function(y=NULL) {
  
  #	(catch, index, I.smooth=5, F.smooth=3, center=T, F.scalar=1, npts=1, avg=F, plot=T )
  #######################################################
  ## exploration of AIM toolbox model
  ## use AIM to identify stable year/index/catch values corresponding to replacement F
  ##
  ## Liz Brooks
  ## Created on:  31 August 2009
  ## Last updated: 10 August 2020
  #######################################################
  
  
  # Note 10 August 2020: this function builds on my AIM code;
  # AIM finds a relative F ("replacement F") where the population would be stable (via regression of replacement ratio on relative F)
  # i then calculate the euclidean distance from observed points [log(relative F), log(replacement ratio)] and select the point with the minimum distance to the replacement F
  # stable.index is the index value at this closest point
  # stable.catch is the catch value at this closest point
  # stable.year is the year position at this closest point (vector of year is not passed, so this is the just the place in that unknown year vector)
  
  #catch is a vector of total catch in biomass
  #index is a vector of total index in biomass
  #I.smooth is number of years of index to smooth over (5 years default)
  #F.smooth is number of years of relative F to smooth over (3 years default)
  #center T or F to center the lags on the midpoint of smoothing interval for relative F, or lag backwards
  #F.scalar is a constant that scales relative F to calculate catch as index[nyears]*relativeF*F.scalar
  #npts is the number of points nearest replacement F to calculate the "stable period" (note: they are not always adjacent index values--suggest 1 yr as default); if npts>1, and avg=T then i return the average index value and average catch across those npts, if avg=F i return the npts for index and catch
  
  # NOTE: assumes same number of years in catch and index vector!! 
  catch<-y$catch
  index<-y$expanded
  I.smooth=y$AIM_I_smooth
  F.smooth=y$AIM_F_smooth
  center=y$AIM_center
  F.scalar<-y$AIM_Fscalar
  plot=y$AIM_plot
  avg<-y$AIM_avg
  npts<-y$AIM_npts
  
  nyears <- length(index)
  years <- seq(1,nyears)
  dc <- dim(catch)
  if(is.null(dc)==F)  catch <- apply(catch,1,sum)
  
  # Calculate Replacement Ratio (rr)
  rr= rep(NA, nyears)
  denom=1.0
  for (rrloop in (1+I.smooth):nyears) {
    
    denom= sum(index[(rrloop-I.smooth):(rrloop-1)])/I.smooth
    rr[rrloop]=index[rrloop]/denom
  } #end rrloop  
  
  
  
  # Calculate Relative F (ff)
  
  if (center==T) {
    ff= rep(NA, nyears)
    denom=1.0
    ctr = trunc(F.smooth/2)
    
    for (floop in 1:nyears) {
      if (floop==1  )  {
        denom= sum(index[1:2])/2
        ff[floop]=catch[floop]/denom
      }
      
      if ((floop)>1  &  (floop+F.smooth-ctr-1)<=(nyears)  ) {
        denom= sum(index[(floop-ctr):(floop+F.smooth-ctr-1)])/F.smooth
        
        ff[floop]=catch[floop]/denom
      }
      
      if ((floop+F.smooth-ctr -1)>(nyears)   ) {
        divis = length(index[(floop-ctr):nyears])
        denom= sum(index[(floop-ctr):nyears])/divis
        ff[floop]=catch[floop]/denom
      }
    } #end floop
    
  } #end ctr if statement
  
  if (center==F) {
    ff= rep(NA, nyears)
    denom=1.0
    
    for (floop in F.smooth:nyears) {
      
      denom= sum(index[(floop-F.smooth+1):floop])/F.smooth
      ff[floop]=catch[floop]/denom
      
    } #end floop
    
  } #end ctr if statement
  
  
  
  ## autocorrelation in index
  ind.ac <- acf(index, lag.max=1, plot=F)
  
  #cross-correlation between catch and index
  catch.ind.ccf <- ccf(catch, index, lag.max=5, plot=F) 
  
  
  #Replacement ratio regression
  df <- as.data.frame(cbind(R=log(rr[(I.smooth+1):nyears]), F=log(ff[(I.smooth+1):nyears])))
  ln.rr <- lm(R~F, data=df)
  reg.pars <- summary(ln.rr)
  
  #Solve for ln(relative F) where ln(Replacement ratio) =0    
  #fstart = log(ff[1])
  fstart = log(ff[which(is.na(ff)==F)] [10])
  
  get.r.int <- function(fstart) {
    tmp.r <- reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*fstart 
    return(abs(tmp.r)  )
  }   # end get.yield.f.min function
  
  repl.f <- nlminb( start=fstart , objective=get.r.int, lower=min(log(ff), na.rm=T), upper=max(log(ff), na.rm=T),
                    control=list(eval.max=500, iter.max=200  )   )
  
  
  # retransform repl.f (median)
  rel.f.soln <- exp(repl.f$par)
  proj.catch <- F.scalar*rel.f.soln*index[nyears]
  
  
  
  #  New stuff to find stable period (2020 aug) ====
  
  rr.ff.vec <- cbind(log(ff), log(rr))
  edist <- function(x, rF=repl.f$par) { sqrt( (x[1]-rF)^2 + x[2]^2) }
  edist.relF <- apply(rr.ff.vec, 1, edist)
  
  shortest <- order(edist.relF)[1:npts]
  stable.index <- index[shortest]
  stable.year <- shortest  # this is just the position in the vector, not a year value
  stable.catch <- catch[shortest]
  
  # Note: if npts >1, then you might want to average or take median?
  if (npts>1){
    if (avg==T) {
      stable.index <- mean(stable.index)
      stable.catch <- mean(stable.catch)
      #doesn't make sense to average stable.year
    }
  }
  
  
  if (plot==T){
    plot(log(ff), log(rr), xlab="Ln(Relative F)", ylab="Ln(Replacement Ratio)")   
    lines(x=log(ff), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*log(ff)), col='red')
    abline(h=0)
    abline(v=0)
    points(x=(repl.f$par), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*repl.f$par), pch=19, col='red')
    # paint closest point green:
    points(x=rr.ff.vec[shortest,1], y=rr.ff.vec[shortest,2], pch=20, col='green3')
    
    
    
    plot(seq(1,nyears), index, type='l', xlab="Year index", ylab="Index")
    points(years[shortest], index[shortest], col='green3', pch=20, cex=1.4)
    abline(h=stable.index, col='green3', lty=4)
    
  } #end plot-test
  
  stable.list <- list(stable.year=stable.year, stable.index=stable.index, stable.catch=stable.catch, proj.catch=proj.catch, F.replacement=rel.f.soln, repl.ratio=rr, rel.F=ff, catch.index.ccf=catch.ind.ccf, reg.pars=reg.pars, repl.f.calc=repl.f )
  
  return(stable.list)
  
} #end function to identify stable period

#y$stable.year<-get.stable.period(y)$stable.year

#-------------------------------------------

Islope <- function (y) 
{
  # y = output list from WHAM containing all inputs
  # index = single survey index over time, or an avg of multiple surveys
  # catch = total catch in wt by year
  # yrsmth = # of years you want to use or "smooth" over (default of 5 years)
  # version = which version of the method do you want to use (version 1 is the default)
  # cap = should you cap the target catch to only allow changes of 20% (TRUE = yes, FALSE = no (the default))
  
  index<-y$index
  catch<-y$catch
  yrsmth<-y$Islope_yrsmth
  version<-y$Islope_version
  cap<-y$Islope_cap
  
  if(version > 4 | version < 1)
  {
    stop("You must enter a version between 1 and 4, 1=least conservative, 4 = most conservative")
  }
  
  # versions 1-4 of the Islope method (based on Geromont and Butterworth 2014)
  lambdas = c(0.4,0.4,0.4,0.2) # this param weights the slope stimates
  cmults = c(0.8,0.7,0.6,0.6)  # multiplier of recent avg catch
  
  lambda <- lambdas[version]
  cmult <- cmults[version]
  
  # which years of the index and catch vectors should be used - take the last yrsmth values
  iyrs <- (length(index) - (yrsmth - 1)):length(index)
  cyrs <- (length(catch) - (yrsmth - 1)):length(catch)
  
  i.use <- index[iyrs]
  c.use <- catch[cyrs]
  mu.C <- mean(c.use,na.rm=TRUE) # avg. catch
  C.last <- tail(catch,n=1) # most recent catch 
  
  yind <- 1:yrsmth
  Islope_lm <- lm(log(i.use) ~ yind)
  slppar <- summary(Islope_lm)$coefficients[2, 1:2]
  #Islope <- rnorm(reps, slppar[1], slppar[2])  # this is if we want to use random draws based on pt estimate and std error
  Islope <- slppar[1]
  C.targ <- cmult * mu.C * (1 + lambda * Islope)
  
  if(cap==TRUE) # this caps the change at +/- 20% per year
  {
    C.targ[C.targ >1.2*C.last] <- 1.2 * C.last
    C.targ[C.targ < 0.8 * C.last] <- 0.8 * C.last
  }
  
  names(C.targ) <-NULL
  return(list(C.targ,Islope_lm))
}

# default run
#Islope(y)

# use a slightly more conservative option
#y$Islope_version<-2	#	ranges 1:4
#Islope(y)

# use a slightly more conservative option and more years in averageing
#y$Islope_yrsmth<-7
#Islope(y)

# use the most conservative option AND cap changes at 20%
#y$Islope_cap<-TRUE
#Islope(y)

Itarget <- function(y)
{
  # this is a hybrid of the skate CR ref points and the I target control rule 
  # ref_yrs is the number of years for the reference period - this should stay fixed throughout sim so we don't have shifting ref pts. 
  # Cmult determines the target catch (relative to the avg.)
  # w is a parameter (0-1) that determines how quickly catch declines between I.target and I.threshold 
  version <- y$Itarget_version
  index<-y$index
  Ctot<-y$catch
  ref_yrs<-y$Itarget_ref_yrs
  base_yrs<-y$n_years_model
  use_yrs <- seq((base_yrs-ref_yrs+1),base_yrs,1) # use these years from the base period
  yrsmth<-y$Itarget_yrsmth
  w<-y$Itarget_w
  I.avg <- mean(index[use_yrs]) # the average of the index over the base period
  
  if(version > 4 | version < 1)
  {
    stop("You must enter a version between 1 and 4, 1=least conservative, 4 = most conservative")
  }
  
  Itarg_mult <- c(1.5, 2.0, 2.0, 2.5)
  cmult<- c(1.0,1.0,1.0,0.6)
    
  # Target = Some multiple of the Average over the reference period
  I.targ <- Itarg_mult[version] * I.avg
  I.thresh <- 0.8 * I.targ
  #C.star <- cmult[version]*mean(tail(Ctot,5),na.rm=TRUE) #  recent average catch (5 yr)
  C.star <- cmult[version]*mean(Ctot[use_yrs],na.rm=TRUE) # average catch of reference period
  I.rec <- mean(tail(index,5),na.rm=TRUE)  # recent 5 year average
  
  if(I.rec >= I.thresh) # if above thresh catch increases linearly
  {
    C.targ <- w*C.star*(1+(I.rec-I.thresh)/(I.targ-I.thresh))
  }
  else if(I.rec < I.thresh) # if below threshold, decline quadratically
  {
    C.targ <- w*C.star * (I.rec/I.thresh)^2
  }
  
  #print(c(I.rec, I.targ, I.thresh, I.rec/I.thresh, C.star, C.targ))
        
  names(C.targ)<-NULL
  
  return(C.targ)
}
#Itarget(y)

#---------------------------
#	interpretation of the skate control rule from the skate spreadsheet

true_Skate_CR <- function(y)
{
  #	assess_span is the number of years since the last assessment.  From the spreadsheet, these years are not considered in the median C/B ratio
  
  index<-y$index
  catch<-y$catch
  yrsmth<-y$true_Skate_CR_yrsmth
  assess_span<-y$true_Skate_CR_assess_span
  percent<-y$true_Skate_CR_percent		#	proportion of output assuming it is an ABC that should be the ACT, needs modification
  
  
  # moving avg - sides = 1 uses preceeding values; sides = 2 uses values on either side
  I.smooth <- stats::filter(index,rep(1/yrsmth,yrsmth), sides=1)
  C.smooth <- stats::filter(catch,rep(1/yrsmth,yrsmth), sides=1)
  
  
  c.b<-C.smooth/I.smooth/1000	
  median.c.b<-median(c.b[yrsmth:(length(c.b)-assess_span)],na.rm=T)	#	median C/B ratio over the entire time series minus the latest set of years, first few years of moving average are NA
  
  abc<-I.smooth[length(I.smooth)]*median.c.b*1000		#	this appears to be the ABC, but the spread sheet also had a target C/B ratio but unknown how it was calculated
  act<-abc*percent	#	Not sure if we want to output the ABC or the ACT or...
  
  return(abc)
}

#true_Skate_CR(y)



#--------------------
# this is a modfication of the way DLMtool calculates Z from catch at age data
# it returns Z estimate only and not a target catch.  
# The M_CC function returns a target catch by calling this function

DLM_Z <-function(y) 
{
  #	method takes abundance at age from each survey calculates catch curve and then takes the mean of the coef.
  # caa = numerical catch at age in matrix form (rows=years,cols = ages)
  # yrs = how many years of data to use
  
  
  #print("DLM_Z")
  CAA_one<-	y$index_naa[,,1]
  CAA_one<- CAA_one[,-ncol(CAA_one)] # remove plus group
  CAA_two<-	y$index_naa[,,2]	
  CAA_two<- CAA_two[,-ncol(CAA_two)]  # remove plus group
 
  yrs<-y$DLM_Z_yrs
  
  ny_one <- nrow(CAA_one) # total number of years in CAA matrix
  ny_two <- nrow(CAA_two) # total number of years in CAA matrix
  use.rows_one<-(ny_one-yrs+1):ny_one
  use.rows_two<-(ny_two-yrs+1):ny_two
  #print(CAA_one)
  #print(CAA_two)
  Csum_one <- apply(CAA_one[use.rows_one, ], 2, sum,na.rm=TRUE) # sum up by column
  Csum_two <- apply(CAA_two[use.rows_two, ], 2, sum,na.rm=TRUE) # sum up by column
  #print(Csum_one)
  #print(Csum_two)
  if(sum(Csum_one)==0 | sum(Csum_two)==0)  # this is if the CAA matrix is all NAs or NaN
  {
    #return(0) #TJM: zero causes problems for M_CC because there is some division by Z from this function
    return(NA)
  }
  
  else
  {
  maxage_one <- length(Csum_one) # max. age
  maxage_two <- length(Csum_two) # max. age
  AFS_one <- which.max(Csum_one)  # age at full selection
  AFS_two <- which.max(Csum_two)  # age at full selection
  AFS_one[AFS_one > (maxage_one - 3)] <- maxage_one - 3 # need at least 3 ages to do the regression
  AFS_two[AFS_two > (maxage_two - 3)] <- maxage_two - 3 # need at least 3 ages to do the regression
  
  y_one <- log(Csum_one[AFS_one:maxage_one]/sum(Csum_one[AFS_one:maxage_one], na.rm = T))
  y_two <- log(Csum_two[AFS_two:maxage_two]/sum(Csum_two[AFS_two:maxage_two], na.rm = T))
  xc_one <- 1:length(y_one)
  xc_two <- 1:length(y_two)
  y_one[y_one == "-Inf"] <- NA
  y_two[y_two == "-Inf"] <- NA
  mod_one <<- lm(y_one ~ xc_one)
  #print(mod_one)
  mod_two <<- lm(y_two ~ xc_two)
  #print(mod_two)
  #print(summary(mod))
  chk_one <- sum(is.na(coef(mod_one)))
  chk_two <- sum(is.na(coef(mod_two)))
  #print(chk_one)
  #print(chk_two)
  if(chk_one) 
  {
    return(NA)
  }
  else 
  {
    coefs_one <- summary(mod_one, weights = Csum_one[AFS_one:maxage_one])$coefficients[2,1:2]
    #print(coefs)
    coefs_one[is.nan(coefs_one)] <- NA
    names(coefs_one) <- NULL
  }
  if(chk_two) 
  {
    return(NA)
  }
  else 
  {
    coefs_two <- summary(mod_two, weights = Csum_two[AFS_two:maxage_two])$coefficients[2,1:2]
    #print(coefs)
    coefs_two[is.nan(coefs_two)] <- NA
    names(coefs_two) <- NULL
  }
  mean_coef<-mean(c(-coefs_one[1],-coefs_two[1]))
  #print(coefs_one)
  #print(coefs_two)
  #print(mean_coef)
  return(mean_coef)
  }
} # end DLM_Z function

# default
#DLM_Z(y)

# use more years of data
#y$DLM_Z_yrs<-5
#DLM_Z(y)


# Simple method that used catch curve to estimate Z, and assumes Fmsy = assumed M
M_CC = function (y) 
{
  # catch = catch time series in weight
  # caa = numerical catch at age in matrix form (rows=years,cols = ages)
  # yrs = how many years of data to use
  # M = assumed natural mortality
  # Fmin is the minimum estimate of recent F to prevent very low values which causes really high target catches
  
  catch<-y$catch
  #print(catch)
  #	CAA<-	y$index_naa	#	 not needed because the y list gets passed to DLM_Z function to compute catch curve
  M<-y$M
  #print(M)
  yrs<-y$M_CC_yrs
  #print(yrs)
  Fmin<-y$M_CC_Fmin
  #print(Fmin)
  M_CC_method<-y$M_CC_method
  #print(M_CC_method)
  cyrs <- (length(catch) - (yrs - 1)):length(catch)
  #print(cyrs)
  c.use <- catch[cyrs]
  #print(c.use)
  mu.C <- mean(c.use,na.rm=TRUE) # avg. catch
  #print(mu.C)
  Z.est <- DLM_Z(y)
  #print(Z.est)
  Z.out <- Z.est # save the original estimate
  
  F.est <- Z.est - M # calculate the F given Z and assumed M
  #print(F.est)
  F.est[is.na(F.est)] <- Fmin  # if you get an NA, replace with Fmin
  #print(F.est)
  F.est[F.est < Fmin] <- Fmin  # if you get a value below Fmin, use Fmin
  #print(F.est)
  Z.est <- F.est + M # recalc Z  to take into account the above in case Z.est is NA 
  
  Ac <- mu.C/((F.est/Z.est)*(1 - exp(-Z.est))) # approx biomass based on catch and estimate of F
  #print(Ac)
  
  if(M_CC_method==1){FMSY <- y$F_SPR/Z.est*(1-exp(-Z.est))}    	#	SPR
  #if(M_CC_method==2){FMSY<-mean(catch[y$stable.year]/Ac[y$stable.year])  }		#	stable historic period; #check:Divides catch/biomass in each year and then takes mean, This one doesn't make sense for CC.  Ac is a single value, not time series.  Stable period only applies to expanded biomass
  if(M_CC_method==3){FMSY <- M/Z.est*(1-exp(-Z.est))} 		#	natural mortality= Fmsy
  #print(FMSY)
  C.targ<- FMSY * Ac
  #print(C.targ)
  #print("end M_CC")
  #stop()
  return(list(C.targ,Z.out))
}

#M_CC(y)


####plan B smooth
planBsmoothfxn<-function(y){
  # devtools::install_github("cmlegault/PlanBsmooth")
  # library(PlanBsmooth)
  # library(ggplot2)
  # library(dplyr)
  # library(tidyr) 
  #planBsmooth<-ApplyPlanBsmooth(data.frame("Year"=y$years,"avg"=y$index))
  planBsmooth<-ApplyPlanBsmooth_fast(data.frame("Year"=y$years,"avg"=y$index), showwarn=FALSE)
  meancatch=mean(y$catch[(length(y$catch)-(y$expand_yrs-1)):length(y$catch)])
  catch.advice=planBsmooth$multiplier*meancatch
  return(list(catch.advice,planBsmooth))
}

#planBsmoothfxn(y=y)


#------------------------
####Expanded survey biomass		#	Bell modification
ExpandSurvey_modified<-function(y){
  #	modified from above
  #	added a scaler to modify true q
  #	expanded each survey index individually and then combined them
  #	check:PLACE HOLDER ONLY, NEED TO DETERMINE HOW TO COMBINE SURVEYS	
  #	currently takes the mean of spring survey at time T and fall survey at time T-1
  expanded_one<-y$seasonal_index[,1]/(y$init_q[1]*y$expand_q_scaler) #JJD
  expanded_two<-y$seasonal_index[,2]/(y$init_q[2]*y$expand_q_scaler) #JJD
  temp.calc<-data.frame(spr=c(expanded_one,0),fall=c(0,expanded_two))
  expanded<-rowMeans(temp.calc)[1:nrow(y$seasonal_index)]	#	assumes spring survey is available in current year
  y$expanded<-expanded
  f.spr.vals<-y$F_SPR	#	proxy Fmsy level from spawner per recruit (e.g. F40%)
  if(y$expand_method==4){
    exploit=y$catch/expanded
    #ifelse(exploit>1,print("Warning: Exploitation > 1"),print("Exploit OK, <1"))
    mu<-mean(exploit[(length(exploit)-(y$expand_yrs-1)):length(exploit)])
    catch.advice=expanded[length(expanded)]*mu
  } 
  if(y$expand_method==1) { #Fspr
    Z<-y$M+f.spr.vals 
    mu<-(f.spr.vals/Z)*(1-exp(-Z))
    catch.advice=expanded[length(expanded)]*mu
  }
  if(y$expand_method==2) {		#	stable historical period	
    stable.res<-get.stable.period(y=y)
    mu<-stable.res$F.replacement
    #Z<-proxy_F+y$M
    #mu<-(proxy_F/Z)*(1-exp(-Z))
    #mu<-mean(y$catch[y$stable.year]/expanded[y$stable.year])  		#	stable historic period		check:Divides catch/biomass in each year and then takes mean
    catch.advice=expanded[length(expanded)]*mu
  }
  if(y$expand_method==3) {		#	proxy Fmsy = natural mortality
    proxy_F<-y$M
    Z<-y$M+proxy_F 
    mu<-(proxy_F/Z)*(1-exp(-Z))
    catch.advice=expanded[length(expanded)]*mu
  }
  
  return(list(catch.advice,mu=mu,bio=expanded[length(expanded)]))
}
#ExpandSurvey_modified(y=y)

#------------------------
####AIM; courtesy of Liz Brooks
run.aim <- function(y) {
  #catch is a vector of total catch in biomass
  #index is a vector of index of abundance
  #I.smooth is number of years of index to smooth over (5 years default)
  #F.smooth is number of years of relative F to smooth over (3 years default)
  #center T or F to center the lags on the midpoint of smoothing interval for relative F, or lag backwards
  #F.scalar is a constant that scales relative F to calculate catch as index[nyears]*relativeF*F.scalar
  
  # NOTE: assumes same number of years in catch and index vector!!   
  
  catch<-y$catch
  index<-y$index
  I.smooth=y$AIM_I_smooth
  F.smooth=y$AIM_F_smooth
  center=y$AIM_center
  F.scalar<-y$AIM_Fscalar
  plot=y$AIM_plot
  
  nyears <- length(index)
  dc <- dim(catch)
  if(is.null(dc)==F)  catch <- apply(catch,1,sum)
  
  # Calculate Replacement Ratio (rr)
  rr= rep(NA, nyears)
  denom=1.0
  for (rrloop in (1+I.smooth):nyears) {
    
    denom= sum(index[(rrloop-I.smooth):(rrloop-1)])/I.smooth
    rr[rrloop]=index[rrloop]/denom
  } #end rrloop  
  
  
  
  # Calculate Relative F (ff)
  
  if (center==T) {
    ff= rep(NA, nyears)
    denom=1.0
    ctr = trunc(F.smooth/2)
    
    for (floop in 1:nyears) {
      if (floop==1  )  {
        denom= sum(index[1:2])/2
        ff[floop]=catch[floop]/denom
      }
      
      if ((floop)>1  &  (floop+F.smooth-ctr-1)<=(nyears)  ) {
        denom= sum(index[(floop-ctr):(floop+F.smooth-ctr-1)])/F.smooth
        
        ff[floop]=catch[floop]/denom
      }
      
      if ((floop+F.smooth-ctr -1)>(nyears)   ) {
        divis = length(index[(floop-ctr):nyears])
        denom= sum(index[(floop-ctr):nyears])/divis
        ff[floop]=catch[floop]/denom
      }
    } #end floop
    
  } #end ctr if statement
  
  if (center==F) {
    ff= rep(NA, nyears)
    denom=1.0
    
    for (floop in F.smooth:nyears) {
      
      denom= sum(index[(floop-F.smooth+1):floop])/F.smooth
      ff[floop]=catch[floop]/denom
      
    } #end floop
    
  } #end ctr if statement
  
  
  
  ## autocorrelation in index
  ind.ac <- acf(index, lag.max=1, plot=F)
  
  #cross-correlation between catch and index
  catch.ind.ccf <- ccf(catch, index, lag.max=5, plot=F) 
  
  
  #Replacement ratio regression
  df <- as.data.frame(cbind(R=log(rr[(I.smooth+1):nyears]), F=log(ff[(I.smooth+1):nyears])))
  ln.rr <- lm(R~F, data=df)
  reg.pars <- summary(ln.rr)
  
  #Solve for ln(relative F) where ln(Replacement ratio) =0    
  #fstart = log(ff[1])
  fstart = log(ff[which(is.na(ff)==F)] [10])
  
  get.r.int <- function(fstart) {
    tmp.r <- reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*fstart 
    return(abs(tmp.r)  )
  }   # end get.yield.f.min function
  
  repl.f <- nlminb( start=fstart , objective=get.r.int, lower=min(log(ff), na.rm=T), upper=max(log(ff), na.rm=T),
                    control=list(eval.max=500, iter.max=200  )   )
  
  if (plot==T){
    plot(log(ff), log(rr))   
    lines(x=log(ff), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*log(ff)), col='red')
    abline(h=0)
    abline(v=0)
    points(x=(repl.f$par), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*repl.f$par), pch=19, col='red')
  } #end plot-test
  
  # retransform repl.f (median)
  rel.f.soln <- exp(repl.f$par)
  proj.catch <- F.scalar*rel.f.soln*index[nyears]
  aim.settings <- list(F.smooth=F.smooth, I.smooth=I.smooth, center=center, nyears=nyears)
  aim.list <- list(proj.catch=proj.catch, F.replacement=rel.f.soln, repl.ratio=rr, rel.F=ff, ccf=catch.ind.ccf, reg.pars=reg.pars, repl.f.calc=repl.f, aim.settings=aim.settings )
  
  return(aim.list )
  
} #end run.aim function
####End AIM function

#run.aim(y)$proj.catch

#----------------------------------
ensemble<-function(y=NULL){
  advice<-Islope(y)[1]
  advice<-c(advice,Itarget(y)[1])
  advice<-c(advice,true_Skate_CR(y))
  advice<-c(advice,planBsmoothfxn(y)[[1]])
  advice<-c(advice,run.aim(y)[[1]])
  advice<-c(advice,M_CC(y))
  advice<-c(advice,ExpandSurvey_modified(y)[[1]])
  advice<-c(advice,JoeDLM(y)[[1]])
  return(list(median(advice),advice))
}
#ensemble(y)


#	Joe Langan's dynamic linear model function

JoeDLM=function(y){
  #two packages I use because their functions help with speed-up
  require(dlm)
  require(RandomFieldsUtils)
  
  survey<-y$seasonal_index
  catch<-y$catch
  prop.inc<-y$JoeDLM_prop_inc		#	proportion to increase biomass each year
  n.ahead<-y$JoeDLM_n_ahead<-2 #forecasting x years and returning x years of catch advice
  
  # survey=ylist[[1]]
  # catch=ylist[[2]]
  # prop.inc=ylist[[3]]
  
  MCMC=1700
  burn=400
  thin=3
  #  n.ahead=2 #forecasting 2 years and returning 2 years of catch advice
  
  #get things on log scale
  y.internal=log(as.matrix(survey))
  catch=log(as.matrix(catch))
  
  ns=ncol(y.internal)
  nt=nrow(y.internal)
  
  #convert catch to catch anomalies by removing average exploitation rate relative to survey
  x=matrix(NA,nt,ns)
  lm.mat=matrix(NA,ns,2)
  for(i in 1:ns){
    m=lm(catch~y.internal[,i])
    x[,i]=m$residuals
    lm.mat[i,]=m$coefficients
  }
  
  #function to sample full conditional of measurement error
  sample.V=function(FF,theta,y.internal,a.y,b.y,nt,ns){
    yest=matrix(NA,nt,ns)
    for(i in 1:nt){
      yest[i,]=FF[i,,]%*%theta[i+1,]
    }
    
    Vlist=rep(NA,ns)
    for(i in 1:ns){
      Vlist[i]=1/rgamma(1, a.y+0.5 * nt, b.y + 0.5 *sum((y.internal[,i]-yest[,i])^2))
    }
    
    V=diag(Vlist,ns)
    return(V)
  }
  
  #function to sample full conditional of evolution error
  sample.W=function(theta,y.internal,GG,W,W0,nt,ns,Trend){
    theta.center=theta[-1,]-(theta[-(nt+1),] %*% t(GG))
    ng=ncol(GG)
    nsv=ng-2*ns+1
    SS2 <- crossprod(theta.center)[nsv : ng, nsv : ng] + W0
    SSinv=RandomFieldsUtils::solvex(SS2)
    #SSinv=solve(SS2)
    
    
    #Wslope <- solve(rwishart(df = length(ns:ng)+2 + nt,Sigma = SSinv))
    Wslope <- RandomFieldsUtils::solvex(rwishart(df = length(ns:ng)+2 + nt,Sigma = SSinv))
    Wint <- diag(0,ns)
    W <- bdiag(Wint,Wslope)
    
    
    return(W)
  }
  
  #function for calculating model estimates from state variables, observation error turned off because
  #we only care about the mean in this application and sampling rmvnorm is slow
  estcalc=function(FF,theta,V,nt,ns){
    est=matrix(NA,nt,ns)
    for(i in 1:nt){
      est[i,]=t(FF[i,,]%*%theta[i+1,])#+rmvnorm(1,rep(0,dim(V)[1]),V)
    }
    return(est)
  }
  
  #set up variance priors
  var0=mean(diag(var(y.internal,na.rm=T)))
  W01=diag(var0/4,ns*2)
  W01[which(W01==0)]=var0/8
  b.y=var0/2
  a.y=1
  V0=diag(var0/2,ns)
  
  #set up model structure and load it into container from dlm package
  g0=diag(3);g0[1,2]=1
  f0=matrix(c(1, 0,1), nrow = 1)
  g=g0%x% diag(ns)
  f=f0%x% diag(ns)
  p=ncol(f)
  jf=f; jf[,-c((p-ns+1):p)]=0;jf[,c((p-ns+1):p)]=jf[,c((p-ns+1):p)]*c(1:ns)
  W0=bdiag(diag(ns),W01)
  mod <- dlm(FF =f,
             V = V0,
             GG = g ,
             W = W0,
             m0 = c(colMeans(y.internal), rep(0,ns),rep(-1,ns)),
             C0 = diag(c(rep(9,ns),rep(.0625,ns),rep(.25,ns))),
             X=as.matrix(x),JFF=jf)
  
  #create G matrix and F array, where F array contains covariate values
  GG=mod$GG
  FFt=array(1,c(nt,ns,p))
  inds=(p-ns+1):p
  for(i in 1:nt){
    FF0=mod$FF
    FF0[,inds]=FF0[,inds]*x[i,]
    FFt[i,,]=FF0
  }
  
  #bins to save Gibbs values
  Thetasave=array(NA,c(nt+1,p,MCMC*thin))
  Msave=array(NA,c(1,p,MCMC*thin))
  Vsave=array(NA,c(ns,ns,MCMC*thin))
  Wsave=array(NA,c(p,p,MCMC*thin))
  Csave=Wsave
  estsave=array(NA,c(nt,ns,MCMC*thin))
  
  #Gibbs sampler
  for(it in 1:(MCMC*thin)){
    ff=dlmFilter(y.internal,mod)
    theta=dlmBSample(ff)
    mod$V=sample.V(FFt,theta,y.internal,a.y,b.y,nt,ns)
    mod$W=sample.W(theta,y.internal,mod$GG,mod$W,W01,nt,ns,Trend)
    
    
    estsave[,,it]=estcalc(FFt,theta,mod$V,nt,ns)
    Vsave[,,it]=mod$V
    Wsave[,,it]=mod$W
    Thetasave[,,it]=theta
    Msave[,,it]=ff$m[nt+1,]
    Csave[,,it]=dlmSvd2var(u=ff$U.C[[nt+1]],d=ff$D.C[nt+1,])
  }
  
  #Thinning
  tseq=seq(1,MCMC*thin,thin)
  Vsave=Vsave[,,tseq,drop=F]
  Wsave=Wsave[,,tseq]
  Thetasave=Thetasave[,,tseq]
  Msave=Msave[,,tseq,drop=F]
  Csave=Csave[,,tseq]
  estsave=estsave[,,tseq,drop=F]
  #Burn-in
  Vsave=Vsave[,,-(1:burn),drop=F]
  Wsave=Wsave[,,-(1:burn)]
  Csave=Csave[,,-(1:burn)]
  Thetasave=Thetasave[,,-(1:burn)]
  Msave=Msave[,,-(1:burn),drop=F]
  estsave=estsave[,,-(1:burn),drop=F]
  
  #forecast function, stuff is turned off so that it only returns the mean forecast
  #again, we only care about the mean in this application, so this is faster
  frcst=function(p,ns,FFf,GG,W,V,m,C,n.ahead){
    #Forecasting
    af=matrix(NA,n.ahead+1,p)
    Rf=array(NA,c(n.ahead+1,p,p))
    af[1,]=m
    Rf[1,,]=C
    forc=matrix(NA,n.ahead,ns)
    for(i in 1:n.ahead){
      af[i+1,]=GG%*%matrix(af[i,],nc=1)
      #Rf[i+1,,]=GG%*%Rf[i,,]%*%t(GG)+W
      ff=FFf[i,,]%*%matrix(af[i+1,],nc=1)
      #Qf=FFf[i,,]%*%Rf[i+1,,]%*%t(FFf[i,,])#+V
      #forc[i,]=rmvnorm(1,ff,Qf)
      forc[i,]=ff
    }
    return(forc)
    
  }
  
  #catch advice function to give to optim
  #uses catch adjustment term to set future harvest, calculates target survey abundance,
  #uses future harvest to generate mean forecast, compares forecast to target abundance, 
  #returns difference between forecast and target as objective to be minimized
  catch.advice=function(param,survey,catch,prop.inc,lm.mat,
                        n.ahead,Wsave,Vsave,Msave,Csave,mod,nt,ns,p){
    c.adj=param
    fcatch=rep(NA,n.ahead)
    for(i in 1:n.ahead){
      if(i==1){
        fcatch[i]=log(exp(catch[length(catch)])*(1+c.adj))
      }else{
        fcatch[i]=log(exp(fcatch[i-1])*(1+prop.inc))
      }
    }
    
    target=matrix(NA,n.ahead,ns)
    for(i in 1:n.ahead){
      if(i==1){
        target[i,]=survey[nt,]*(1+prop.inc)
      }else{
        target[i,]=target[i-1,]*(1+prop.inc)
      }
    }
    
    target=log(target)
    
    xf=matrix(NA,dim(target)[1],dim(target)[2])
    for(j in 1:ncol(xf)){
      xf[,j]=fcatch-(target[,j]*lm.mat[j,2]+lm.mat[j,1])
    }
    
    FFf=array(1,c(n.ahead,ns,p))
    inds=(p-ns+1):p
    for(i in 1:n.ahead){
      FF0=mod$FF
      FF0[,inds]=FF0[,inds]*xf[i,]
      FFf[i,,]=FF0
    }
    
    proj=array(NA,c(n.ahead,ns,dim(Msave)[[3]]))
    for(k in 1:dim(Msave)[[3]]){
      proj[,,k]=frcst(p,ns,FFf,mod$GG,Wsave[,,k],Vsave[,,k],Msave[,,k],Csave[,,k],n.ahead)
    }
    
    proj=apply(proj,c(1,2),mean)
    diff=proj-target
    
    obj=mean(abs(diff))
    
    return(obj)
    
  }
  
  #use optim to solve for catch advice
  ca=optim(par=0,fn=catch.advice,survey=as.matrix(survey),catch=catch,prop.inc=prop.inc,lm.mat=lm.mat,
           n.ahead=n.ahead,Wsave=Wsave,Vsave=Vsave,Msave=Msave,Csave=Csave,mod=mod,nt=nt,ns=ns,p=p, 
           method="Brent",lower=-1,upper=1,control=list(abstol=1e-10,reltol=1e-10))$par
  
  
  #converts catch adjustment term from optim into actual harvest levels to return as catch advice
  fcatch=rep(NA,n.ahead)
  for(i in 1:n.ahead){
    if(i==1){
      fcatch[i]=exp(catch[length(catch)])*(1+ca)
    }else{
      fcatch[i]=fcatch[i-1]*(1+prop.inc)
    }
  }
  
  #output: 1) catch advice, 2) model estimates of survey indices for each iteration, 3) state variables for each iteration, 
  #4) evolution error variance for each iteration, 5) measurement error variance for each iteration
  
  return(list(fcatch,estsave,Thetasave,Wsave,Vsave))
}

#	mod=JoeDLM(y)
#	catch advice
#	mod[[1]]	#	 check:can produce multiple years of catch advise based on input y$JoeDLM_n_ahead.  Current default is 2 years of catch advise


