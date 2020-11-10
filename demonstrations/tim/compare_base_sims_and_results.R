#devtools::install_github("timjmiller/wham", dependencies=TRUE)
library(wham)
library(tidyverse)
library(furrr)
library(purrr)
#library(googledrive)
library(dlm)
library(RandomFieldsUtils)
library(dplyr)

rscripts <- c("code/base_input.R",
              "code/change_input.R",
              "code/IBM_options.R",
              #"code/performance_metrics.R",
#              "code/wham_mse_functions.R",
              #"demonstrations/tim/wham_mse_functions_2.R",
              "demonstrations/tim/make_wham_om.R",
              "code/wham_retro_functions.R")
map(rscripts, source)

retro_lookup <- tibble(RetroSource = c("Missing Catch", "Increased M"),
                       retro_type = c("Catch","M"))

F_lookup <- tibble(Fhistory = c("always overfishing", "overfishing then fmsy"),
                   Fhist = c(2,1))
sel_lookup <- tibble(FisherySelectivity = c("Constant", 
                                            "Time-varying"),
                     n_selblocks = c(1,2))
                     
aug_input = function(input, nprojyrs, adv.yr, retro_type, n_selblocks, Fhist){
  input$nprojyrs = nprojyrs
  input$adv.yr = adv.yr 
  input$retro_type = retro_type 
  input$n_selblocks <- n_selblocks
  input$Fhist <- Fhist
  return(input)
}
om_input <- expand.grid(Fhist = 1:2,
                  n_selblocks = 1:2,
                  retro_type = c("Catch","M")) %>%
  tibble() %>% 
  mutate(input = pmap(list(n_selblocks = n_selblocks, Fhist = Fhist, Fmsy_scale = 2.5),
                    get_base_input)) %>% 
  mutate(input = pmap(list(input = input, Fhist = Fhist, retro_type = retro_type, nprojyrs = 40, adv.yr = 2, n_selblocks = n_selblocks), aug_input))

om_objects <- om_input %>%
  mutate(true_om = pmap(list(input), make_wham_om)) #%>%

mse_sim_setup <- readRDS(file = "settings/mse_sim_setup.rds")

sim_fn = function(seed, om) {
  set.seed(seed)
  om$om$simulate(complete=TRUE)
}

my_future_options = future_options() #will also make om_objects available
my_future_options$globals = ls()
my_future_options$packages <- c("wham",
                                "tidyverse",
                                "dlm",
                                "RandomFieldsUtils")

  future::plan(future::multisession)

base_sims <- mse_sim_setup %>% 
    #filter(isim == 1) %>% 
    #distinct(seed, .keep_all = TRUE) %>%
    mutate(IBM = map_chr(specs, "IBM"),
           Fhist = map_dbl(specs, "Fhist"),
           n_selblocks = map_dbl(specs, "n_selblocks"),
           retro_type = map_chr(specs, "retro_type"),
           catch_mult = map_dbl(specs, "catch.mult")) %>% 
    filter(IBM == "Islope") %>% filter(catch_mult == 1) %>%
    #filter(rowid %in% todo) %>% 
    left_join(om_objects) %>% 
    mutate(base = pmap(list(seed, true_om), sim_fn)) %>%
    select(-c(input,true_om))
    
saveRDS(base_sims, file = "demonstrations/tim/base_sims.rds")


base_sims = readRDS(file = "demonstrations/tim/base_sims.rds")
ex_res = readRDS(file = "demonstrations/tim/mse-LB-20201017193336.rds")

temp = base_sims %>%
  filter(seed == 7084879 & Fhist == 2 & n_selblocks == 1 & retro_type == "Catch")

temp_res = ex_res  %>% filter(seed == 7084879) %>%
  mutate(IBM = map_chr(specs, "IBM"),
    Fhist = map_dbl(specs, "Fhist"),
    n_selblocks = map_dbl(specs, "n_selblocks"),
    retro_type = map_chr(specs, "retro_type"),
    catch_mult = map_dbl(specs, "catch.mult")) %>%
    filter(catch_mult == 1)

temp_res %>% distinct(IBM)

cbind(temp$base[[1]]$SSB[1:50], 
  ex_res$wham[[1]]$result$true_sim$SSB[1:50])
#Hurray! they are the same!
