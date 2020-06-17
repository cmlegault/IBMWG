#devtools::install_github("timjmiller/wham", dependencies=TRUE, ref = "om_mode")
library(wham)
source("../gavin/wham_mse_functions.R")
source("wham_retro_functions.R")

#Retro type definitions
#size of retro: ~0.5
#Catch/F:  3-5 times higher than reported. Change 10 years from end of base period
#q not priority
#M: 10? year ramp ending 10 years before the end of the base period (3-5 time higher than assumed)
#fit SCAA model to estimate retro.

input = get_input()
#change any inputs if necessary

na = length(input$maturity) #number of ages
nf=NCOL(input$F) #number of fleets (we only need 1)
ni=length(input$q) #number of indices 
recruit_model = 3 #Beverton-Holt
nbase = 20 #number of scenarios

  
#a50 = 5 and slope = 1 for logistic selectivity for each fleet and index
sel.list=list(model=rep("logistic",ni+nf), re=rep("none",ni+nf), initial_pars=lapply(1:(ni+nf), function(x) c(5,1)))

#AR1 deviations of rececruitment only over time. To include abundances at older ages as random effects use "rec+1"
NAA.list = list(sigma='rec',cor='ar1_y')
#NAA.list = list(sigma='rec',cor='iid') #this would make recruitment deviations iid.

seed = 8675309

#this will simulate population and observations
om = prepare_wham_om_input(input, recruit_model = recruit_model, selectivity=sel.list, NAA_re = NAA.list)#, proj.opts = proj.list)
#om_wham = fit_wham(om, do.fit = FALSE)

scaa_input = prepare_wham_om_input(input, recruit_model = 2, selectivity=sel.list)
om_M_change = change_M_om(om, M_new_ratio = 1.75, n_ramp_years = 10, year_change = 2009) 
om_M_change_wham = fit_wham(om_M_change, do.fit = FALSE)
om_M_change_wham$report()$MAA
set.seed(seed)
simsets = lapply(1:50, function(x) om_M_change_wham$simulate(complete = TRUE))

simres = matrix(NA, length(simsets), na+2)
for(i in 1:length(simsets))
{
  print(paste0("i = ", i))
  tinput = scaa_input
  tinput$data = simsets[[i]]
  tfit = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE, MakeADFun.silent = TRUE)
  simres[i,] = mohns_rho(tfit) #bad retro
  print(apply(simres,2,mean,na.rm=TRUE))
}
colnames(simres) = names(mohns_rho(tfit))
apply(simres,2, mean)
#Mohn's rho for SSB ~ 0.72
