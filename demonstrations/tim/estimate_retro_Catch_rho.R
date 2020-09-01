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

estimate_retro_Catch_rho = function(C_ratio = 4, nsim = 50, n_selblocks = 1, Fhist = 1, Fmsy_scale = 2.5){

  om = get_base_input(n_selblocks, Fhist, Fmsy_scale)

  om_wham = fit_wham(om, do.fit = FALSE)
  scaa_input = get_base_input(n_selblocks, Fhist, Fmsy_scale, scaa=TRUE)

  seed = 8675309
  set.seed(seed)
  simsets = lapply(1:nsim, function(x) change_catch_sim(sim = om_wham$simulate(complete = TRUE), catch_ratio = 1/C_ratio, year_change = 2010, years = 1970:2019))
  simres = matrix(NA, length(simsets), om$data$n_ages+2)
  for(i in 1:length(simsets))
  {
    print(paste0("i = ", i))
    tinput = scaa_input
    tinput$data = simsets[[i]]
    tinput$data$Fbar_ages = 10
    tinput$data$use_steepness = 0
    tinput$data$recruit_model = 2
    tfit = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE, MakeADFun.silent = TRUE)
    simres[i,] = mohns_rho(tfit)
    print(apply(simres,2,mean,na.rm=TRUE))
  }
  colnames(simres) = names(mohns_rho(tfit))
  apply(simres,2, mean)

  return(list(simres, tfit))
}

#scen11 = estimate_retro_Catch_rho(C_ratio = 5) #SSB 0.4, F -0.28, R 0.32

#scen12 = estimate_retro_Catch_rho(C_ratio = 2, Fhist = 2) #SSB 0.54, F -0.38, R 0.52
#scen21 = estimate_retro_Catch_rho(C_ratio = 5, n_selblocks = 2) #SSB 0.45, F -0.31, R 0.30
#scen22 = estimate_retro_Catch_rho(C_ratio = 2.25, n_selblocks = 2, Fhist = 2) #SSB 0.52, F -0.41, R 0.45
