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

#set up projections to repopulate 
proj.list = list(n.yrs=40, use.last.F=TRUE, use.avg.F=FALSE, use.FXSPR=FALSE,
                                              proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
                                              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL, cont.Mre=NULL)

qratio = 1/5 #ratio of true q to observed q
input$q = input$q*qratio #increase F during base period to get higher F, could specify this without regard to catch misreporting ratio
om = prepare_wham_om_input(input, recruit_model = recruit_model, selectivity=sel.list, NAA_re = NAA.list)
om_wham = fit_wham(om, do.fit = FALSE)
set.seed(123)
#set.seed(sim.seeds[669,15]) 
scaa_input = prepare_wham_om_input(input, selectivity=sel.list)
y = om_wham$simulate(complete= TRUE)
#reduce q in appropriate period with ramp if desired
y2 = change_q_sim(sim = y, q_ratio = 1/qratio, year_change = 2015, n_ramp_years = 1, years = 1970:2019)

#see the differences in the aggregated catch
cbind(y$agg_indices, y2$agg_indices)

#fit scaa model with incorrect catch
tinput = scaa_input
#tinput = om
tinput$data = y2
tfit = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE)
mohns_rho(tfit) #bad retro

#fit state-space model with correct catch
tinput = om
#tinput = om
tinput$data = y
tfit2 = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE)
mohns_rho(tfit2) #good retro

#fit SCAA model with correct catch
tinput = scaa_input
#tinput = om
tinput$data = y
tfit3 = fit_wham(tinput, do.osa = FALSE, do.sdrep = FALSE)
mohns_rho(tfit3) #good retro

#below is similar to simple_example.R but now using this simuated data instead of the yellowtail fit. Include projection/feedback period
om = prepare_wham_om_input(input, recruit_model = recruit_model, selectivity=sel.list, NAA_re = NAA.list, proj.opts = proj.list)
om_wham = fit_wham(om, do.fit = FALSE)
rep = om_wham$report()
SSBlim = exp(rep$log_SSB_MSY[1]) #165,000 mt
#Flim = exp(rep$log_FMSY[1]) #165,000 mt
Flim = exp(rep$log_MSY[1]) #43,500 mt

set.seed(123)
sim_data_series = list()
plot(om_wham$years, y$SSB[1:om_wham$input$data$n_years_model], ylab = "SSB", xlim = range(om_wham$years_full), xlab = "Year", type = "b", ylim =c(0,500000))
input_i = om_wham$input
#set.seed(sim.seeds[669,15]) 
y = om_wham$simulate(complete= TRUE)
y = change_q_sim(sim = y, q_ratio = 1/qratio, year_change = 2015, n_ramp_years = 1, years = 1970:2019)
#this assumes we know what SSB is, so it is not realistic
catch_advice = ifelse(y$SSB[om_wham$input$data$n_years_model]>SSBlim, Flim, 0.9*sum(y$agg_catch[y$n_years_model,]))
for(i in 1:40)
{
  om_wham$env$data$proj_Fcatch[i] = catch_advice #bump up catch that is actually taken out of the stock
  om_wham$env$data$proj_F_opt[i] = 5 #specify catch
  set.seed(123)
  #reduce observed catch in projection years
  sim_data_series[[i]] = change_q_sim(sim = om_wham$simulate(complete = TRUE), q_ratio = 1/qratio, year_change = 2015, n_ramp_years = 1, years = 1970:2019)
  sim_data_series[[i]]$agg_indices_proj[i, ] = sim_data_series[[i]]$agg_indices_proj[i, ]/qratio #have to change catch in projection years
  lines(om_wham$years_full[1:(om_wham$input$data$n_years_model+i)], sim_data_series[[i]]$SSB[1:(om_wham$input$data$n_years_model+i)], col = i)
  catch_advice = ifelse(sim_data_series[[i]]$SSB[om_wham$input$data$n_years_model+i]>SSBlim, Flim, 0.9* sum(sim_data_series[[i]]$agg_catch_proj[i,]))
}
