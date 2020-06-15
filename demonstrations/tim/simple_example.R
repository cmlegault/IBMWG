#devtools::install_github("timjmiller/wham", dependencies=TRUE, ref = "om_mode")
library(wham)

# get results to check NLL and par estimates
path_to_examples <- system.file("extdata", package="wham")
# ex3_test_results <- readRDS(file.path(path_to_examples,"ex3_test_results.rds"))

asap3 <- read_asap3_dat(file.path(path_to_examples,"ex1_SNEMAYT.dat"))

#recruitment is a random about mean with iid errors/deviations
#input1 <- prepare_wham_input(asap3, recruit_model=2, model_name="Ex 1: SNEMA Yellowtail Flounder",
#	                            selectivity=list(model=rep("age-specific",3), 
#                                	re=rep("none",3), 
#                                	initial_pars=list(c(0.5,0.5,0.5,0.5,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,1,0.5,0.5,0.5,0.5)), 
#                                	fix_pars=list(5,4,2)),
#	                            NAA_re = list(sigma="rec", cor="iid"))

#recruitment is random about mean with AR1 errors/deviations
input1 <- prepare_wham_input(asap3, recruit_model=2, model_name="Ex 1: SNEMA Yellowtail Flounder",
	                            selectivity=list(model=rep("age-specific",3), 
                                	re=rep("none",3), 
                                	initial_pars=list(c(0.5,0.5,0.5,0.5,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,1,0.5,0.5,0.5,0.5)), 
                                	fix_pars=list(5,4,2)),
	                            NAA_re = list(sigma="rec", cor="ar1_y"))

#input1 <- prepare_wham_input(asap3, recruit_model=3, model_name="Ex 1: SNEMA Yellowtail Flounder",
#	                            selectivity=list(model=rep("age-specific",3), 
#                                	re=rep("none",3), 
#                                	initial_pars=list(c(0.5,0.5,0.5,0.5,1,0.5),c(0.5,0.5,0.5,1,0.5,0.5),c(0.5,1,0.5,0.5,0.5,0.5)), 
#                                	fix_pars=list(5,4,2)),
#	                            NAA_re = list(sigma="rec", cor="iid"))


#base period is fixed
input1$data$simulate_period[1] = 0 

#set up projection with F = F40 for 40 years
proj.list = list(n.yrs=40, use.last.F=FALSE, use.avg.F=FALSE,
              use.FXSPR=TRUE, proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL)

## Fit model with projections
mod <- fit_wham(input1, do.proj=T, do.osa=F, do.retro=F, proj.opts = proj.list)#, MakeADFun.silent = TRUE)
#saveRDS(mod, file = "mod.RDS")
#mod = readRDS("mod.RDS")
#source("~/work/wham/wham/R/wham_plots_tables.R")
#plot_wham_output(mod)

sim_data_series = list()
sim_seed = 8675309
plot(mod$years, mod$rep$SSB[1:mod$input$data$n_years_model], ylab = "SSB", xlim = range(mod$years_full), xlab = "Year", type = "b")
input_i = mod$input
catch_advice = ifelse(mod$rep$SSB[mod$input$data$n_years_model]>4000, 1.1, 0.9)* mod$rep$pred_catch[mod$input$data$n_years_model]
for(i in 1:40)
{
  input_i$data$proj_Fcatch[i] = catch_advice
  input_i$data$proj_F_opt[i] = 5
  temp = TMB::MakeADFun(input_i$data, mod$parList, map = input_i$map, random = input_i$random, DLL = "wham", silent = TRUE)
  temp$fn()
  set.seed(sim_seed)
  sim_data_series[[i]] = temp$simulate(complete = TRUE)
  lines(mod$years_full[1:(mod$input$data$n_years_model+i)], sim_data_series[[i]]$SSB[1:(mod$input$data$n_years_model+i)], col = i)
  #do assessment method (AIM, ASAP, etc.)
  #important_assessment_result <- run_assesssment_method(sim_data)
  #catch_advice <- get_catch_advice(important_assessment_result)
  #take up where we left off
  #set catch to catch advice
  catch_advice = ifelse(sim_data_series[[i]]$SSB[mod$input$data$n_years_model+i]>4000, 1.1, 0.9)* sim_data_series[[i]]$pred_catch[mod$input$data$n_years_model+i]
}

sim_data_series = list()
sim_seed = 8675309
plot(mod$years, input_i$data$agg_catch, ylab = "Catch", xlim = range(mod$years_full), xlab = "Year")
lines(mod$years, mod$rep$pred_catch[1:mod$input$data$n_years_model,])
input_i = mod$input
catch_advice = ifelse(mod$rep$SSB[mod$input$data$n_years_model]>4000, 1.1, 0.9)* mod$rep$pred_catch[mod$input$data$n_years_model]
for(i in 1:40)
{
  input_i$data$proj_Fcatch[i] = catch_advice
  input_i$data$proj_F_opt[i] = 5
  temp = TMB::MakeADFun(input_i$data, mod$parList, map = input_i$map, random = input_i$random, DLL = "wham", silent = TRUE)
  temp$fn()
  set.seed(sim_seed)
  sim_data_series[[i]] = temp$simulate(complete = TRUE)
  points(mod$years_full[mod$input$data$n_years_model+1:(i)], sim_data_series[[i]]$agg_catch_proj[1:i], col = "red")
  lines(mod$years_full[1:(mod$input$data$n_years_model+i)], sim_data_series[[i]]$pred_catch[1:(mod$input$data$n_years_model+i)], col = "red")
  #do assessment method (AIM, ASAP, etc.)
  #important_assessment_result <- run_assesssment_method(sim_data)
  #catch_advice <- get_catch_advice(important_assessment_result)
  #take up where we left off
  #set catch to catch advice
  catch_advice = ifelse(sim_data_series[[i]]$SSB[mod$input$data$n_years_model+i]>4000, 1.1, 0.9)* sim_data_series[[i]]$pred_catch[mod$input$data$n_years_model+i]
}

sim_data_series = list()
sim_seed = 8675309
plot(mod$years, input_i$data$agg_indices[,1], ylab = "Index", xlim = range(mod$years_full), xlab = "Year")
lines(mod$years, mod$rep$pred_indices[1:mod$input$data$n_years_model,1])
input_i = mod$input
catch_advice = ifelse(mod$rep$SSB[mod$input$data$n_years_model]>4000, 1.1, 0.9)* mod$rep$pred_catch[mod$input$data$n_years_model]
for(i in 1:40)
{
  input_i$data$proj_Fcatch[i] = catch_advice
  input_i$data$proj_F_opt[i] = 5
  temp = TMB::MakeADFun(input_i$data, mod$parList, map = input_i$map, random = input_i$random, DLL = "wham", silent = TRUE)
  temp$fn()
  set.seed(sim_seed)
  sim_data_series[[i]] = temp$simulate(complete = TRUE)
  points(mod$years_full[mod$input$data$n_years_model+1:(i)], sim_data_series[[i]]$agg_indices_proj[1:i,1], col = "red")
  lines(mod$years_full[1:(mod$input$data$n_years_model+i)], sim_data_series[[i]]$pred_indices[1:(mod$input$data$n_years_model+i),1], col = "red")
  #do assessment method (AIM, ASAP, etc.)
  #important_assessment_result <- run_assesssment_method(sim_data)
  #catch_advice <- get_catch_advice(important_assessment_result)
  #take up where we left off
  #set catch to catch advice
  catch_advice = ifelse(sim_data_series[[i]]$SSB[mod$input$data$n_years_model+i]>4000, 1.1, 0.9)* sim_data_series[[i]]$pred_catch[mod$input$data$n_years_model+i]
}
