#	Statistical Catch At Age model

SCAA = function(y) {
  nyr_add = y$scaa_nyr_add
  om = y$observed_om
  y = y[names(y) != "observed_om"]
  if(nyr_add> y$n_years_proj) stop(paste0("nyr_add = ", nyr_add, " is greater than number of projection years in simulated data (", y$n_years_proj,")"))
  FfromCatch = function(catch, NAA, waa, M, sel){
    catchF = function(logF) sum(NAA * waa * (1 - exp(-(exp(logF)*sel + M))) * exp(logF) * sel/(exp(logF)*sel + M))
    obj.fn = function(logF) (catch - catchF(logF))^2
    return(exp(nlminb(log(0.2), obj.fn)$par))
  }
  catchproj = function(NAA, F, waa, M, sel, R, nyr = 10){
    na = length(NAA)
    NAAproj = matrix(NA, nyr-1, na)
    NAAproj[,1] = R
    NAAproj = rbind(NAA,NAAproj)
    catchproj = sum(NAAproj[1,] * waa *(1 - exp(-(F[1] * sel + M))) * F[1] * sel/(F[1] * sel + M))
    for(i in 2:nyr) {
      for(a in 2:(na-1)) NAAproj[i,a] = NAAproj[i-1,a-1] * exp(-(F[i-1] * sel[a-1] + M[a-1]))
      NAAproj[i,na] = sum(NAAproj[i-1,na - 1:0] * exp(-(F[i-1] * sel[na - 1:0] + M[na - 1:0])))
      catchproj[i] = sum(NAAproj[i,] * waa *(1 - exp(-(F[i] * sel + M))) * F[i] * sel/(F[i] * sel + M))
    }
    return(catchproj)
  }
  get_SCAA_catch_advice = function(fit, rho, catch_t, nyr=2) {
    rep = fit$rep
    data = fit$env$data
    NAA = rep$NAA[data$n_years_model,]/(1+rho[-(1:2)])
    MAA = rep$MAA[data$n_years_model,]
    waa = data$waa[1,data$n_years_model,]
    mat = data$mature[data$n_years_model,]
    R = mean(rep$NAA[,1])
    sel =rep$FAA_tot[data$n_years_model,]
    sel = sel/max(sel)
    #SSB = rep$SSB[data$n_years_model]/(1+rho[["SSB"]])
    Fref = exp(rep$log_FXSPR[data$n_years_model])
    #lastcatch = sum(data$agg_catch[data$n_years_model,])
    #print(lastcatch)
    nextF = FfromCatch(catch_t, NAA, waa, MAA, sel)
    #print(SSB)
    #SSBref = mean(rep$NAA[1:data$n_years_model,1]) * exp(rep$log_SPR_FXSPR[data$n_years_model])
    #print(SSBref)
    #if(SSB < 0.5 * SSBref) nextF = c(nextF, rep(get_F_rebuild(fit, rho, SSBref),9))
    #else 
    nextF = c(nextF, rep(0.75 * Fref,nyr-1))
    catch = catchproj(NAA, nextF, waa, MAA, sel, R, nyr = nyr)
    return(list(catch[nyr], nextF))
  }
  n_selblocks = ifelse(length(y$selAA) == 3, 1, 2)
  scaa_input = get_base_input(n_selblocks, Fhist=1, Fmsy_scale=1, scaa=TRUE)
  if(nyr_add>0) {
    update_input = update_wham_input(y, om, n_years_add=nyr_add)
    scaa_input$data = update_input$data
    scaa_input$par = update_input$par
    scaa_input$par$mean_rec_pars = scaa_input$par$mean_rec_pars[2]
    scaa_input$map = update_input$map
    scaa_input$map$trans_NAA_rho = factor(rep(NA, length(scaa_input$par$trans_NAA_rho)))
    scaa_input$map$log_NAA_sigma = factor(rep(NA, length(scaa_input$par$log_NAA_sigma)))
    scaa_input$map$mean_rec_pars = factor(rep(NA, length(scaa_input$par$mean_rec_pars)))
  }
  else {
    update_input = om$input
    scaa_input$data = y
  }
  scaa_input$data$Fbar_ages = 10
  scaa_input$data$use_steepness = 0
  scaa_input$data$recruit_model = 2
  catch_t = sum(y$agg_catch_proj[nyr_add+1,])
  update_sim_fit = fit_wham(scaa_input, do.fit = TRUE, do.retro = TRUE, do.sdrep=FALSE, do.osa = FALSE, MakeADFun.silent = TRUE, retro.silent = TRUE)
  rho = mohns_rho(update_sim_fit)
  catch_advice = get_SCAA_catch_advice(update_sim_fit, rho, catch_t)
  catch_advice[[3]] = catch_t
  catch_advice[[4]] = rho
  return(catch_advice)
}
