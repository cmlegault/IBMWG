#functions to run WHAM MSE
# modded from simnple_example2.R

#get the input list for a given scenario
get_input <- function(iscen = 1, na = 10, nf = 2, ni = 2, LVB_pars = c(100,0.3), LW_pars = c(exp(-11),3)) {
  #nf=2 #number of fleets (we only need 1)
  #ni=2 #number of indices
  #na = 10 #number of ages
  input = list()
  input$modyears = 1970:2019 #50 years
  input$maturity = 1/(1 + exp(-1*(1:na - 5))) #maturity at age
  input$fracyr_spawn = 0.25 #when spawning occurs within annual time step
  L = LVB_pars[1]*(1-exp(-LVB_pars[2]*(1:na - 0))) #make up LVB growth
  W = LW_pars[1]*L^LW_pars[2] #make up L-W function to give WAA
  input$waa_catch = t(matrix(W, na, nf)) #WAA for each fleet
  input$waa_indices = t(matrix(W, na, ni)) #WAA for each index
  input$waa_totcatch = input$waa_ssb = input$waa_jan1 = rbind(W) #WAA for total catch, SSB, Jan 1 pop
  input$catch_cv = rep(0.1, nf) #CVs for aggregate catch for each fleet
  input$catch_Neff = rep(200, nf) #Effective sample size for age comp for each fleet
  input$index_cv = rep(0.3, ni) #CVs for aggregate indices
  input$index_Neff = rep(100, ni) #Effectin sample size for age compe for each index
  input$fracyr_indices = (1:ni)/(ni+1) #when index observations occur within annual time step
  
  input$sel_model_fleets = rep(2, nf) #logistic selectivity for each fleet
  #input$sel_model_indices = 2
  input$sel_model_indices = rep(2,ni) #logistic selectivity for each index
  
  #input$q = 0.3
  input$q = (1:ni)/(ni+1) #catchability for each index
  input$F = matrix(rep(0.2/nf,length(input$modyears)), length(input$modyears), nf) #Annual Full F for each fleet
  input$M = rep(0.2, na) #Nat. Mort at age
  input$N1 = exp(10)*exp(-(0:(na-1))*input$M[1]) #Initial numbers at age
  #recruit_model = 2 #random devations around mean. 3 = BH (mean_rec_pars = a,b R = aS/(1 + bS)), 4 = Ricker (mean_rec_pars = a,b)
  recruit_model = 3 #Beverton-Holt
  input$mean_rec_pars = numeric(c(0,1,2,2)[recruit_model])
  a = 4*0.7/(0.3*25.8) #h=0.7, phi0=25.8
  b = (a - 1/25.8)/exp(10) #R0 = exp(10)
  if(recruit_model == 2) input$mean_rec_pars[] = exp(10)
  if(recruit_model == 3) input$mean_rec_pars[] = c(a, b)
  if(recruit_model == 4) input$mean_rec_pars[2] = exp(-10)
  input$NAA_rho = c(0, 0.7) #AR1 rho for age and year (Here: just AR1 with year for recruitment deviations)
  input$NAA_sigma = 0.5*prod(sqrt(1-input$NAA_rho^2)) #recruitment sd, Marginal SD = 0.5. Need more values if full state-space abundance at age
  return(input)
} #end get_input function


### run a given wham MSE simulation
## will want to abstract more of the set up from this as it's the same for each simulation
do_wham_mse_sim <- function(seed = 42, input = NULL, nprojyrs = 10) {
  # function does 1 simulation for the wham mse
  # i.e. 1 realization of the base and projection period
  # add more arguments so can abstract more of scenario setup from the inner function

  # GF - had to copy these from input specs, mod so part of arguments
  na = 10 #number of ages
  nf=2 #number of fleets (we only need 1)
  ni=2 #number of indices 
  recruit_model = 3 #Beverton-Holt
    
#a50 = 5 and slope = 1 for logistic selectivity for each fleet and index
sel.list=list(model=rep("logistic",ni+nf), re=rep("none",ni+nf), initial_pars=lapply(1:(ni+nf), function(x) c(5,1)))

#AR1 deviations of rececruitment only over time. To include abundances at older ages as random effects use "rec+1"
NAA.list = list(sigma='rec',cor='ar1_y')
#NAA.list = list(sigma='rec',cor='iid') #this would make recruitment deviations iid.
#```

#```{r base-period}
#set up projections to repopulate 
proj.list = list(n.yrs=40, use.last.F=TRUE, use.avg.F=FALSE, use.FXSPR=FALSE,
                                              proj.F=NULL, proj.catch=NULL, avg.yrs=NULL,
                                              cont.ecov=TRUE, use.last.ecov=FALSE, avg.ecov.yrs=NULL, proj.ecov=NULL, cont.Mre=NULL)

#generate the input for fit_wham. Data (indices, catch) are not populated though.
x = prepare_wham_om_input(input, recruit_model = recruit_model, selectivity=sel.list, NAA_re = NAA.list, proj.opts = proj.list)
temp = fit_wham(x, do.fit = FALSE)
rep  = temp$report() #log_MSY, log_F_MSY, log_SSB_MSY, log_FXSPR, log_SPR_MSY, log_SPR0, and much more

#set.seed(sim.seeds[669,15]) 
set.seed(seed) 
#simulated data and other report items
y = temp$simulate(complete= TRUE)

#This would fit the model
#tdat = temp$input
#tdat$data = y
#z = fit_wham(tdat, do.osa = FALSE, do.retro = FALSE, do.fit = TRUE)

#below is similar to simple_example.R but now using this simuated data instead of the yellowtail fit.
sim_data_series = list()
sim_data_series[[1]] <- y
advice <- list()
# plot(temp$years, y$SSB[1:temp$input$data$n_years_model], ylab = "SSB", xlim = range(temp$years_full), xlab = "Year", type = "b", ylim =c(0,1000000))
# ```
# 
# ```{r init-assess}
input_i = temp$input
SSBlim = exp(rep$log_SSB_MSY[1]) #165,000 mt
Flim = exp(rep$log_FMSY[1]) #165,000 mt
refpts <- list(SSBlim = SSBlim,
               Flim = Flim)
catch_advice = ifelse(y$SSB[temp$input$data$n_years_model]>SSBlim, Flim, 0.9*sum(rep$F[y$n_years_model,]))
advice[[1]] = catch_advice
# ```
# 
# 
# ```{r proj-period}

for(i in 1:nprojyrs)
{
  input_i$data$proj_Fcatch[i] = catch_advice
  input_i$data$proj_F_opt[i] = 4 #Specify F
  temp = fit_wham(input_i, do.fit = FALSE, MakeADFun.silent = TRUE)
  #temp$fn()
#  set.seed(sim.seeds[669,15])
  set.seed(seed)
  sim_data_series[[i+1]] = temp$simulate(complete = TRUE)
  # lines(temp$years_full[1:(temp$input$data$n_years_model+i)], sim_data_series[[i]]$SSB[1:(temp$input$data$n_years_model+i)], col = i)
  #do assessment method (AIM, ASAP, etc.)
    #Example for fitting WHAM assessment model in feedback period
    #tdat = input_i
    #tdat$data = sim_data_series[[i]]
    #y = fit_wham(tdat, do.osa = FALSE, do.retro = FALSE)
    #sim_data_series[[i]]$wham_fit = y
    #catch_advice = ifelse(sim_data_series[[i]]$wham_fit$rep$SSB[temp$input$data$n_years_model+i]>220000, 1.1, 0.9)* sim_data_series[[i]]$pred_catch[temp$input$data$n_years_model+i]
  #important_assessment_result <- run_assesssment_method(sim_data)
  #catch_advice <- get_catch_advice(important_assessment_result)
  #take up where we left off
  #set catch to catch advice
  catch_advice = ifelse(sim_data_series[[i+1]]$SSB[temp$input$data$n_years_model+i]>SSBlim, Flim, 0.9* sim_data_series[[i+1]]$FAA_tot[temp$input$data$n_years_model+i,na])* Flim
  advice[[i+1]] <- catch_advice
}

  results <- list(sim_data_series = sim_data_series,
                  advice = advice,
                  refpts = refpts,
                  input = input_i,
                  x = x,
                  temp = temp,
                  rep = rep, 
                  seed = seed,
                  input = input)

  return(results)
  
}  #end function do_wham_mse_sim
