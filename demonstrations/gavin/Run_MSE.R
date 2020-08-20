library(wham)
library(PlanBsmooth)
library(ggplot2)
library(dplyr)
library(tidyr) 

source("IBM_options.R")
source("wham_mse_functions.R")
source("base_input.R")

input=get_base_input()
input$IBM=M_CC

mse.results=do_wham_mse_sim(input=input)

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


