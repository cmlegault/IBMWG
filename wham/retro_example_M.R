#devtools::install_github("timjmiller/wham", dependencies=TRUE, ref = "om_mode")
library(wham)
source("../wham_mse_functions.R")
source("../wham_retro_functions.R")

#Retro type definitions
#size of retro: ~0.5
#Catch/F:  3-5 times higher than reported. Change 10 years from end of base period
#q not priority
#M: 10? year ramp ending 10 years before the end of the base period (3-5 time higher than assumed)
#fit SCAA model to estimate retro.

input = get_input()

na = length(input$maturity) #number of ages
nf=NCOL(input$F) #number of fleets (we only need 1)
ni=length(input$q) #number of indices 
recruit_model = 3 #Beverton-Holt
nbase = 20 #number of scenarios
sim.seeds = matrix(sample(1:1000000, nbase*1000, replace = FALSE), 1000, nbase) #set random seeds for each base data set and scenario

  
#a50 = 5 and slope = 1 for logistic selectivity for each fleet and index
sel.list=list(model=rep("logistic",ni+nf), re=rep("none",ni+nf), initial_pars=lapply(1:(ni+nf), function(x) c(5,1)))

#AR1 deviations of rececruitment only over time. To include abundances at older ages as random effects use "rec+1"
NAA.list = list(sigma='rec',cor='ar1_y')
#NAA.list = list(sigma='rec',cor='iid') #this would make recruitment deviations iid.

#set up projections to repopulate 
proj.list = list(n.yrs=40, use.last.F=TRUE, use.avg.F=FALSE, use.FXSPR=FALSE,
                                              proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
                                              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL, cont.Mre=NULL)

om = prepare_wham_om_input(input, recruit_model = recruit_model, selectivity=sel.list, NAA_re = NAA.list)#, proj.opts = proj.list)
om_wham = fit_wham(om, do.fit = FALSE)
scaa_input = prepare_wham_om_input(input, recruit_model = 2, selectivity=sel.list)
om_M_change = change_M_om(om, M_new_ratio = 3, n_ramp_years = 10, year_change = 2009) 
om_M_change_wham = fit_wham(om_M_change, do.fit = FALSE)
om_M_change_wham$report()$MAA
set.seed(123)
#set.seed(sim.seeds[669,15]) 
#fit scaa model with incorrect M
y = om_M_change_wham$simulate(complete= TRUE)
tinput = scaa_input
#tinput = om
tinput$data = y
tfit = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE)
mohns_rho(tfit)

#fit state-space model with correct M
tinput = om_M_change
#tinput = om
tinput$data = y
tfit2 = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE)
mohns_rho(tfit2)

#fit SCAA model with correct M
om_M_change_scaa_input = change_M_om(scaa_input, M_new_ratio = 3, n_ramp_years = 10, year_change = 2009) 
tinput = om_M_change_scaa_input
#tinput = om
tinput$data = y
tfit3 = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE)
mohns_rho(tfit3)


