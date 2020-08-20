# get the base input values for IBMWG analyses
# these will be modified by other functions,
# but this gets the defaults

# 2020/06/25 - recruitment parameters need fixing
# 2020/07/01 - TJM modded so BH can take steepness as input, agreed on OM vals now complete

get_base_input <- function() {
  input <- list()
  input$nf <- 1 #number of fleets (we only need 1)
  input$ni <- 2 #number of indices
  input$na <- 10 #number of ages
  nf <- input$nf #<- 1 #number of fleets (we only need 1)
  ni <- input$ni #<- 2 #number of indices
  na <- input$na #<- 10 #number of ages
  input$modyears <- 1970:2019 #50 years
  input$maturity <- 1/(1 + exp(-1*(1:input$na - 5))) #maturity at age
  input$maturity <- c(0.04, 0.25, 0.60, 0.77, 0.85, 0.92, 1, 1, 1, 1)
  input$fracyr_spawn <- 0 #when spawning occurs within annual time step
  #L = LVB_pars[1]*(1-exp(-LVB_pars[2]*(1:na - 0))) #make up LVB growth
  #W = LW_pars[1]*L^LW_pars[2] #make up L-W function to give WAA
  W <- c(0.15, 0.5, 0.9, 1.4, 2.0, 2.6, 3.2, 4.1, 5.9, 9.0)
  input$waa_catch <- t(matrix(W, na, nf)) #WAA for each fleet
  input$waa_indices <- t(matrix(W, na, ni)) #WAA for each index
  input$waa_totcatch = input$waa_ssb = input$waa_jan1 = rbind(W) #WAA for total catch, SSB, Jan 1 pop
  input$catch_cv <- rep(0.1, nf) #CVs for aggregate catch for each fleet
  input$catch_Neff <- rep(200, nf) #Effective sample size for age comp for each fleet
  input$index_cv <- c(0.3,0.4) #rep(0.3, ni) #CVs for aggregate indices
  input$index_Neff <- rep(100, ni) #Effectin sample size for age compe for each index
  #input$fracyr_indices = (1:ni)/(ni+1) #when index observations occur within annual time step
  input$fracyr_indices <- c(0.25,0.75)
  input$sel_model_fleets = rep(2, nf) #logistic selectivity for each fleet
  #input$sel_model_indices = 2
  input$sel_model_indices = rep(2,ni) #logistic selectivity for each index
  
  #input$q = 0.3
  #input$q = (1:ni)/(ni+1) #catchability for each index
  input$q <- c(0.0002, 0.0001)
  
  #input$F = matrix(rep(0.2/nf,length(input$modyears)), length(input$modyears), nf) #Annual Full F for each fleet
  input$F <- matrix(rep(c(seq(0.1,0.7,length.out = 1995-1971+1),
                      seq(0.7,0.25,length.out = 2010-1995+1)[-1],
                      rep(0.25,2020-2010)),nf),
                    ncol = nf)
  input$M = rep(0.2, na) #Nat. Mort at age
  #input$N1 = exp(10)*exp(-(0:(na-1))*input$M[1]) #Initial numbers at age
  input$N1 <- 10000*exp(-(0:(na-1))*input$M[1]) #Initial numbers at age
  #recruit_model = 2 #random devations around mean. 3 = BH (mean_rec_pars = a,b R = aS/(1 + bS)), 4 = Ricker (mean_rec_pars = a,b)
  recruit_model = 3 #Beverton-Holt
  #input$use_steepness = 0 #don't use steepness
  input$use_steepness = 1 #mean_rec_pars = h,R0
  input$mean_rec_pars = numeric(c(0,1,2,2)[recruit_model])
  h <- 0.75 #a = 4*0.7/(0.3*25.8) #h=0.7, phi0=25.8
  R0 <- 10000 #b = (a - 1/25.8)/exp(10) #R0 = exp(10)
  #if(recruit_model == 2) input$mean_rec_pars[] = exp(10)
  #if(recruit_model == 3) 
  input$mean_rec_pars[] = c(h, R0)
  #if(recruit_model == 4) input$mean_rec_pars[2] = exp(-10)
  input$NAA_rho <- c(0, 0.4) #AR1 rho for age and year (Here: just AR1 with year for recruitment deviations)
  input$NAA_sigma <- 0.5*prod(sqrt(1-input$NAA_rho^2)) #recruitment sd, Marginal SD = 0.5. Need more values if full state-space abundance at age
  return(input)
} #end get_base_input function
