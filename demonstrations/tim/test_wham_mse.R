library(wham)
source("demonstrations/gavin/base_input.R")
source("~/work/wham/wham/R/update_wham_input.R")
source("~/work/wham/wham/R/prepare_wham_input.R")

Fhist = 1
Fhist = 2
Fmsy_scale = 2.5
n_selblocks = 1
n_selblocks = 2
om_input = get_base_input(n_selblocks, Fhist, Fmsy_scale)
#x = om_input$par$logit_selpars[3,11:12]
#om$par$logit_selpars[3,11] = log(3) - log(10-3)
#10/(1 + exp(-x))
om_wham = fit_wham(om_input, do.fit = FALSE)
simdat = om_wham$simulate(complete = TRUE)

#sim_input = om
#sim_input$data = simdat
#sim_fit = fit_wham(temp, do.fit = TRUE, do.retro = FALSE, do.osa = FALSE)
#update_input = update_wham_input(simdat, om_wham, n_years_add=20)
#update_om = fit_wham(update_input, do.fit = FALSE)
#update_sim_fit = fit_wham(update_input, do.fit = TRUE, do.retro = FALSE, do.osa = FALSE)


  scaa_input = get_base_input(n_selblocks, Fhist, Fmsy_scale, scaa=TRUE)
  update_input = update_wham_input(simdat, om_wham, n_years_add=20)
  
  scaa_input$data = update_input$data
  scaa_input$data$Fbar_ages = 10
  scaa_input$data$use_steepness = 0
  scaa_input$data$recruit_model = 2
  scaa_input$par = update_input$par
  scaa_input$par$mean_rec_pars = scaa_input$par$mean_rec_pars[2]
  scaa_input$map = update_input$map
  scaa_input$map$trans_NAA_rho = factor(rep(NA, length(scaa_input$par$trans_NAA_rho)))
  scaa_input$map$log_NAA_sigma = factor(rep(NA, length(scaa_input$par$log_NAA_sigma)))
  scaa_input$map$mean_rec_pars = factor(rep(NA, length(scaa_input$par$mean_rec_pars)))
  update_sim_fit = fit_wham(scaa_input, do.fit = TRUE, do.retro = TRUE, do.sdrep=FALSE, do.osa = FALSE)
  #fit = fit_wham(scaa_input, do.osa = FALSE, do.sdrep = FALSE, do.retro = TRUE, MakeADFun.silent = TRUE)
  rho = mohns_rho(update_sim_fit)

  
#get_SCAA_catch_advice(update_sim_fit, rho, catch_t = sum(simdat$agg_catch_proj[21,]))
SCAA = function(simdat, nyr_add=2)
{
  if(nyr_add> simdat$n_years_proj) stop(paste0("nyr_add = ", nyr_add, " is greater than number of projection years in simulated data (", simdat$n_proj_years,")"))
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
    return(list(catch[2], nextF))
  }
  n_selblocks = ifelse(length(simdat$selAA) == 3, 1, 2)
  scaa_input = get_base_input(n_selblocks, Fhist=1, Fmsy_scale=1, scaa=TRUE)
  update_input = update_wham_input(simdat, om_wham, n_years_add=nyr_add)
  
  scaa_input$data = update_input$data
  scaa_input$data$Fbar_ages = 10
  scaa_input$data$use_steepness = 0
  scaa_input$data$recruit_model = 2
  scaa_input$par = update_input$par
  scaa_input$par$mean_rec_pars = scaa_input$par$mean_rec_pars[2]
  scaa_input$map = update_input$map
  scaa_input$map$trans_NAA_rho = factor(rep(NA, length(scaa_input$par$trans_NAA_rho)))
  scaa_input$map$log_NAA_sigma = factor(rep(NA, length(scaa_input$par$log_NAA_sigma)))
  scaa_input$map$mean_rec_pars = factor(rep(NA, length(scaa_input$par$mean_rec_pars)))
  catch_t = sum(simdat$agg_catch_proj[nyr_add+1,])
  
  update_sim_fit = fit_wham(scaa_input, do.fit = TRUE, do.retro = TRUE, do.sdrep=FALSE, do.osa = FALSE, MakeADFun.silent = TRUE, retro.silent = TRUE)
  rho = mohns_rho(update_sim_fit)
  catch_advice = get_SCAA_catch_advice(update_sim_fit, rho, catch_t)
  catch_advice[[3]] = catch_t
  return(catch_advice)
}

#SCAA(simdat)

get_F_rebuild = function(fit, rho, ssbref, nyr=10) {
  print("F_rebuild start")
  rep = fit$rep
  data = fit$env$data
  NAA = rep$NAA[data$n_years_model,]/(1+rho[-(1:2)])
  MAA = rep$MAA[data$n_years_model,]
  waa = data$waa[1,data$n_years_model,]
  mat = data$mature[data$n_years_model,]
  R = mean(rep$NAA[,1])
  sel =rep$FAA_tot[data$n_years_model,]
  sel = sel/max(sel)
  lastcatch = sum(data$agg_catch[data$n_years_model,])
  print("F_rebuild")
  nextF = FfromCatch(lastcatch, NAA, waa, MAA, sel)
  print(nextF)
  print("F_rebuild")
  SSB_F0 = SSBproj(NAA, c(nextF, rep(0,9)), waa, MAA, mat, sel, R, data$fracyr_SSB[data$n_years_model])
  print("F_rebuild")
  print(SSB_F0)
  if(SSB_F0<ssbref) return(1e-8)
  else{
  print("SSB_F0 > ssbref) F_rebuild")
    obj.fn = function(log_F) (ssbref - SSBproj(NAA, c(nextF, rep(exp(log_F),9)), waa, MAA, mat, sel, R, data$fracyr_SSB[data$n_years_model]))^2
    Frebuild = exp(nlminb(log(0.2), obj.fn)$par)
    print("ssbref")
    print(ssbref)
    print(SSBproj(NAA, c(nextF, rep(Frebuild,9)), waa, MAA, mat, sel, R, data$fracyr_SSB[data$n_years_model]))
    print(Frebuild)
  print("F_rebuild end")
    return(Frebuild)
  }  
}

SSBproj = function(NAA, F, waa, M, mature, sel, R, fracyrssb, nyr = 10){
  print("SSBproj start")
  na = length(NAA)
  NAAproj = matrix(NA, nyr-1, na)
  NAAproj[,1] = R
  NAAproj = rbind(NAA,NAAproj)
  SSBproj = sum(NAAproj[1,] * waa * mature * exp(-(F[1] * sel + M)*fracyrssb))
  for(i in 2:nyr) {
    for(a in 2:(na-1)) NAAproj[i,a] = NAAproj[i-1,a-1] * exp(-(F[i-1] * sel[a-1] + M[a-1]))
    NAAproj[i,na] = sum(NAAproj[i-1,na - 1:0] * exp(-(F[i-1] * sel[na - 1:0] + M[na - 1:0])))
    SSBproj[i] = sum(NAAproj[i,] * waa * mature * exp(-(F[i] * sel + M)*fracyrssb))
  }
  print(SSBproj)
  print("SSBproj end")
  return(SSBproj[nyr])
}
