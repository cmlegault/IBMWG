#devtools::install_github("cmlegault/PlanBsmooth")
library(wham)
#library(PlanBsmooth)
library(ggplot2)
library(dplyr)
library(tidyr) 

source("IBM_options.R")
source("wham_mse_functions.R")
source("GF_planb.R")
source("base_input.R")
source("../tim/wham_retro_functions.R")
#n_selblocks = 1 #constant fishery selectivity
n_selblocks = 2 #if = 2 second half of base period (and feedback period) has alternative fishery selectivity (a50= 5)
Fmsy_scale = 2.5 #What proportion of Fmsy to fish at
#Fhist = 1 #Fish at Fmsy_scale*Fmsy (defined by recent selectivity) for first half of base, then Fmsy second half
Fhist = 1 #Fish at Fmsy_scale*Fmsy (defined by recent selectivity) for all of base period.
input=get_base_input(n_selblocks, Fhist, Fmsy_scale)
  temp = fit_wham(input, do.fit = FALSE)
#input$IBM=DLM_Z
input$adv.yr = 2
input$Fhist = Fhist
input$n_selblocks = n_selblocks
input$nprojyrs = 40
input$expand_method = 3
input$M_CC_method = 3
input$IBM=M_CC
source("wham_mse_functions.R")
input$retro_type = "None"
mse.results1=do_wham_mse_sim(input=input)
input$retro_type = "Catch"
mse.results2=do_wham_mse_sim(input=input)
input$retro_type = "M"
mse.results3=do_wham_mse_sim(input=input)

cbind(mse.results1$sim_data_series$FAA_tot[,10],mse.results2$sim_data_series$FAA_tot[,10],mse.results3$sim_data_series$FAA_tot[,10])
cbind(mse.results1$sim_data_series$NAA[,1],mse.results2$sim_data_series$NAA[,1],mse.results3$sim_data_series$NAA[,1])
cbind(mse.results1$sim_data_series$SSB,mse.results2$sim_data_series$SSB,mse.results3$sim_data_series$SSB)
# possible workflow once functions are all working
# read seeds from RNG.seeds.csv file
# read design matrix from scenarios.csv
# get_base_input()
# loop through scenarios (i)
## adjust input for scenario i
## loop through 1000 realizations (j)
### use seed j for random values
### do_wham_mse_sim(input=modified_input, seed=seed_j)
### save results for each realization in tibble for scenario i realization j
## end realization loop
# end scenarios loop
# summarize results across scenarios
# deal with one off scenarios one at a time (k)
## adjust input for one off k
## loop through 1000 realizations (j)
### use seed j for random values
### do_wham_mse_sim(input=modified_input, seed=seed_j)
### save results for each realization in tibble for one off k realization j
## end realization loop
# end one offs
# summarize results across one offs


