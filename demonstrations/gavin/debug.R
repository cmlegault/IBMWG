library(wham)
library(PlanBsmooth)
library(ggplot2)
library(dplyr)
library(tidyr) 

source("IBM_options.R")
source("wham_mse_functions.R")
source("base_input.R")
source("../tim/wham_retro_functions.R")

n_selblocks = 1 #constant fishery selectivity
n_selblocks = 2 #if = 2 second half of base period (and feedback period) has alternative fishery selectivity (a50= 5)
Fmsy_scale = 2.5 #What proportion of Fmsy to fish at
#Fhist = 1 #Fish at Fmsy_scale*Fmsy (defined by recent selectivity) for first half of base, then Fmsy second half
Fhist = 2 #Fish at Fmsy_scale*Fmsy (defined by recent selectivity) for all of base period.
C_ratio = 2.5
nsim = 1
om = get_base_input(n_selblocks, Fhist, Fmsy_scale)

na = om$data$n_ages #number of ages
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
