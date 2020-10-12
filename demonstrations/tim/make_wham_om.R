make_wham_om <- function(input) {  #JJD
  #make a wham (TMB) object that can be called in various scenarios
  if(is.null(input)) stop("need an input object like that provided by get_base_input()")
  #print(adv.yr)
  
  adv.yr <- input$adv.yr
  nprojyrs <- input$nprojyrs
  retro_type <- input$retro_type
  n_selblocks <- input$data$n_selblocks
  Fhist <- input$Fhist
  
  #NEED TO PROVIDE INCORRECT M to MSE, if M is incorrect!
  observed_om_input = input
  if(retro_type == "M"){
      if(Fhist == 1 & n_selblocks == 3) Mscale = 1.6
      if(Fhist == 2 & n_selblocks == 3) Mscale = 1.8
      if(Fhist == 1 & n_selblocks == 4) Mscale = 1.6
      if(Fhist == 2 & n_selblocks == 4) Mscale = 1.8 
      #true operating model knows correct M
      true_om_input = change_M_om(observed_om_input, M_new_ratio = Mscale, n_ramp_years = 10, year_change = 2009) 
  } else true_om_input = observed_om_input #no M mis-specifcation
  
  observed_om = wham::fit_wham(observed_om_input, do.fit = FALSE)
  observed_rep = observed_om$report()
  true_om = wham::fit_wham(true_om_input, do.fit = FALSE)
  #simulated data and other report items from true operating model
  true_sim = true_om$simulate(complete= TRUE)
  #put data and simulated random parameters (Recruitment) into operating model, so true_om projections will be consistent with simulations
  true_om$env$data = true_sim[names(true_om_input$data)]
  #put in correct recruitment series
  ind = which(names(true_om$env$par) == "log_NAA")
  true_om$env$par[ind] = true_sim$log_NAA[,1] 
  true_rep = true_om$report()

  a = exp(true_rep$log_SR_a[true_om_input$data$n_years_model])
  b = exp(true_rep$log_SR_b[true_om_input$data$n_years_model])
  if(n_selblocks == 3) {
    sel = true_rep$selAA[[1]][true_om_input$data$n_years_model,]
  } else {
    sel = true_rep$selAA[[4]][true_om_input$data$n_years_model,]  
  }
  maturity = true_om_input$data$mature[true_om_input$data$n_years_model,]  
  waa = true_om_input$data$waa[1,true_om_input$data$n_years_model,] 
  fracssb = true_om_input$data$fracyr_SSB[true_om_input$data$n_years_model]
  true_M = true_rep$MAA[true_om_input$data$n_years_model,]
  true_refpts = c(
    R_MSY = exp(true_rep$log_R_MSY[true_om_input$data$n_years_model]), 
    SSB_MSY = exp(true_rep$log_SSB_MSY[true_om_input$data$n_years_model]), 
    F_MSY = exp(true_rep$log_FMSY[true_om_input$data$n_years_model]),
    MSY = exp(true_rep$log_MSY[true_om_input$data$n_years_model]),
    F_40 = exp(true_rep$log_FXSPR[true_om_input$data$n_years_model]),
    R_0 = exp(true_rep$log_SR_R0[true_om_input$data$n_years_model]),
    SPR_0 = exp(true_rep$log_SPR0[true_om_input$data$n_years_model]),
    SPR_40 = exp(true_rep$log_SPR_FXSPR[true_om_input$data$n_years_model]),
    YPR_40 = exp(true_rep$log_YPR_FXSPR[true_om_input$data$n_years_model])
  )
  sprmsy = true_refpts[["SSB_MSY"]]/true_refpts[["R_MSY"]]
  spr.frac.SSB.msy = function(log_F,spr.msy = sprmsy,frac=0.5, Min = true_M){
    spr = wham:::get_SPR(exp(log_F), M = Min, sel = sel, mat=maturity, waassb=waa, fracyrssb=fracssb, at.age = FALSE)
    obj = (a*spr-1 - frac*(a*spr.msy-1))^2 #the necessary parts of SSB(F)
    return(obj)
  }
  F05 = exp(nlminb(log(0.2), spr.frac.SSB.msy)$par)
  F01 = exp(nlminb(log(0.2), spr.frac.SSB.msy, frac=0.1)$par)
  true_refpts = c(
    true_refpts, 
    SSB0 = true_refpts[["SPR_0"]] * true_refpts[['R_0']],
    R_40 = (a - 1/true_refpts[["SPR_40"]])/b,
    SSB_40 = (a * true_refpts[["SPR_40"]] - 1)/b,
    Y_40 = true_refpts[["YPR_40"]] * (a - 1/true_refpts[["SPR_40"]])/b,
    SPR_MSY = sprmsy,
    F_dot_5_SSB_MSY = F05,
    F_dot_1_SSB_MSY = F01
  )
  observed_M = observed_rep$MAA[true_om_input$data$n_years_model,]
  observed_refpts = c(
    R_MSY = exp(observed_rep$log_R_MSY[true_om_input$data$n_years_model]), 
    SSB_MSY = exp(observed_rep$log_SSB_MSY[true_om_input$data$n_years_model]), 
    F_MSY = exp(observed_rep$log_FMSY[true_om_input$data$n_years_model]), 
    MSY = exp(observed_rep$log_MSY[true_om_input$data$n_years_model]),
    F_40 = exp(observed_rep$log_FXSPR[true_om_input$data$n_years_model]),
    R_0 = exp(observed_rep$log_SR_R0[true_om_input$data$n_years_model]),
    SPR_0 = exp(observed_rep$log_SPR0[true_om_input$data$n_years_model]),
    SPR_40 = exp(observed_rep$log_SPR_FXSPR[true_om_input$data$n_years_model]),
    YPR_40 = exp(observed_rep$log_YPR_FXSPR[true_om_input$data$n_years_model])
  )
  sprmsy = observed_refpts[["SSB_MSY"]]/observed_refpts[["R_MSY"]]
  F05 = exp(nlminb(log(0.2), spr.frac.SSB.msy, Min = observed_M)$par)
  F01 = exp(nlminb(log(0.2), spr.frac.SSB.msy, Min = observed_M, frac=0.1)$par)
  observed_refpts = c(
    observed_refpts, 
    SSB0 = observed_refpts[["SPR_0"]] * observed_refpts[['R_0']],
    R_40 = (a - 1/observed_refpts[["SPR_40"]])/b,
    SSB_40 = (a * observed_refpts[["SPR_40"]] - 1)/b,
    Y_40 = observed_refpts[["YPR_40"]] * (a - 1/observed_refpts[["SPR_40"]])/b,
    SPR_MSY = sprmsy,
    F_dot_5_SSB_MSY = F05,
    F_dot_1_SSB_MSY = F01
  )
  
  #both correct and incorrect reference points if M is mis-specified
  refpts <- list(true_refpts = true_refpts,
                 observed_refpts = observed_refpts,
                 observed_MAA = observed_rep$MAA)


  return(list(om = true_om, refpts = refpts))
  
}  #end function do_wham_mse_sim
