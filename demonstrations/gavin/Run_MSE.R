library(wham)
source("IBM_options.R")
source("wham_mse_functions.R")
source("base_input.R")

input=get_base_input()
input$IBM=M_CC

mse.results=do_wham_mse_sim(input=input)



