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
              "demonstrations/tim/wham_mse_functions_2.R",
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
  mutate(true_om = pmap(list(input), make_wham_om)) %>%

#saveRDS(om_objects, file = "demonstrations/tim/true_om_tibble.rds")

mse_sim_setup <- readRDS(file = "settings/mse_sim_setup.rds")

  mse_sim_todo <- mse_sim_setup %>% 
    filter(isim == 1) %>% 
    mutate(IBM = map_chr(specs, "IBM"),
           Fhist = map_dbl(specs, "Fhist"),
           n_selblocks = map_dbl(specs, "n_selblocks"),
           retro_type = map_chr(specs, "retro_type")) %>% 
    #filter(rowid %in% todo) %>% 
    left_join(om_objects) %>% 
    #modify the input object here
    mutate(input = pmap(list(input=input, change=specs), change_input)) %>% 
    I()
  mse_sim_todo

my_future_options = future_options() #will also make om_objects available
my_future_options$globals = ls()
my_future_options$packages <- c("wham",
                                "tidyverse",
                                "dlm",
                                "RandomFieldsUtils")

  future::plan(future::multisession)

  # load in the scenario specifications
  #mse_sim_setup <- readRDS(file = "settings/mse_sim_setup.rds")
  
  # load in the input objects
  #input_setup <- readRDS(file = "settings/input_setup.rds")
  
  # # pull out realizations to be run today from the setup list using "todo". 
  # mse_sim_todo <- mse_sim_setup %>% 
  #   filter(isim == 1) %>% 
  #   mutate(IBM = map_chr(specs, "IBM")) %>% 
  #   #filter(rowid %in% todo) %>% 
  #   left_join(input_setup) %>% 
  #   #modify the input object here
  #   mutate(input = pmap(list(input=input, change=specs), change_input)) %>% 
  #   #unnest(cols = "specs") %>% 
  #   filter(IBM != "ensemble") %>% 
  #   filter(IBM != "JoeDLM") %>% 
  #   I()
  
  ### run the MSE over each row of the mse_sims todo
  safe_wham_mse <- purrr::safely(do_wham_mse_sim, otherwise = NA_real_)

  #do the MSE for all simulations and scenarios
  #profvis::profvis(
  start <- Sys.time()
  mse_output <- mse_sim_todo %>% 
    #group_by(IBM) %>% 
    #slice(1) %>% 
    #ungroup() %>% 
    slice(1) %>% 
     mutate(wham = furrr::future_pmap(list(seed = seed, input = input, true_om_obj = true_om),
                                      safe_wham_mse, .options = my_future_options)) %>% 
    # this is the regular purrr code for iterating over the simulations
    #mutate(wham = purrr::pmap(list(seed = seed, input = input), safe_wham_mse)) %>% 
    select(-input) %>% 
    I()
  #) #ends profvis
  stop <- Sys.time()
  stop - start # how long runs took

  mse_check <- mse_output %>% 
    mutate(error = map(wham, "error"),
           error = map(error, str_flatten),
           error = map_chr(error, na_if, y ="")) %>% 
    #select(rowid, IBM, error) %>% 
    drop_na() %>% 
    I()
  mse_check

# uncomment lines if want to save files
#  saveRDS(list(start, stop), file = "elapsed.rds")  
#  saveRDS(mse_output, file = "mse_test_out.rds")
#  saveRDS(mse_check, file = "mse_check.rds")
  
  # 
  # #check which simulations ran
  # which_ran <- mse_output %>% 
  #   slice(which(!is.na(wham))) %>% 
  #   select(rowid) %>% 
  #   I()
  # which_ran <- as.integer(which_ran$rowid)
  
#   #update progress table with simulations that ran & those that didn't
#   yay <- todo[todo %in% which_ran]
#   nay <- todo[!(todo %in% which_ran)]
#   progress$uploaded[progress$rowid %in% yay] <- TRUE
#   progress$user[progress$rowid %in% nay] <- NA
#   progress$date_run[progress$rowid %in% nay] <- NA
#   
#   
#   #save the output
#   outfile = paste0("output/mse-",user,"-",str_remove_all(today,"-"),".rds")
#   saveRDS(mse_output, file = outfile)
#   
#   # upload output to google drive
#   # rowid is part of the object so does not need to be in the filename
#   if (write_to_google) {
#     drive_upload(
#       outfile,
#       paste0("ibm-test/",outfile))
#   }
#   
#   
#   #################################
#   # update progress file - this will be done again after the simulations are finished in case some didn't work
#   # write the file back to disk & commit to update
#   saveRDS(progress, file = "settings/progress_table.rds")
#   #commit back to the repo so other users don't duplicate your efforts
#   system('git commit -am "updates progress table"')
#   system("git push")
#   #################################
#   
#   
# } #end do_mse function
# 
# 
# ##########################
# # example use
# # specify how many realizations you want to run today
# #nsim = 5
# # who is doing them?
# #user = "GF" # your initials
# #do_mse(nsim = nsim, user = user, write_to_google = TRUE)
# 
