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

recruit_model = 3 #Beverton-Holt
nbase = 20 #number of scenarios
sim.seeds = matrix(sample(1:1000000, nbase*1000, replace = FALSE), 1000, nbase) #set random seeds for each base data set and scenario  
input = get_input()
#a50 = 5 and slope = 1 for logistic selectivity for each fleet and index
nsel = length(input$q) + NCOL(input$F)
sel.list=list(model=rep("logistic",nsel), re=rep("none",nsel), initial_pars=lapply(1:nsel, function(x) c(5,1)))

#AR1 deviations of rececruitment only over time. To include abundances at older ages as random effects use "rec+1"
NAA.list = list(sigma='rec',cor='ar1_y')
#NAA.list = list(sigma='rec',cor='iid') #this would make recruitment deviations iid.

seed = 8675309

cratio = 2 #ratio of true catch to observed catch
input$F = input$F*cratio #increase F during base period to get higher F, could specify this without regard to catch misreporting ratio
om = prepare_wham_om_input(input, recruit_model = recruit_model, selectivity=sel.list, NAA_re = NAA.list)
om_wham = fit_wham(om, do.fit = FALSE)
set.seed(123)
#set.seed(sim.seeds[669,15]) 
scaa_input = prepare_wham_om_input(input, selectivity=sel.list)

set.seed(seed)
simsets = lapply(1:50, function(x) change_catch_sim(sim = om_wham$simulate(complete = TRUE), catch_ratio = 1/cratio, year_change = 2010, years = 1970:2019))

simres = matrix(NA, length(simsets), scaa_input$data$n_ages+2)
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
#Mohn's rho for SSB ~ 0.51
