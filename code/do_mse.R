#devtools::install_github("timjmiller/wham", dependencies=TRUE)
library(wham)
library(tidyverse)
library(furrr)
library(googledrive)
library(dlm)
library(RandomFieldsUtils)


rscripts <- c(#"code/base_input.R",
              "code/change_input.R",
              "code/IBM_options.R",
              #"code/performance_metrics.R",
              "code/wham_mse_functions.R",
              "code/wham_retro_functions.R")
map(rscripts, source)

# make sure that the relevant packages and functions are forced to the workers
my_future_options <- future_options()
my_future_options$globals <- ls()
my_future_options$packages <- c("wham",
                                "tidyverse",
                                "dlm",
                                "RandomFieldsUtils")

### main function for conducting MSE
do_mse <- function(nsim = 7, user = "Me", write_to_google = FALSE) {
  
# set up the type of future (for parallelization of sims using furrr)
#future::plan(future::multisession)

  
# load in the scenario specifications
mse_sim_setup <- readRDS(file = "settings/mse_sim_setup.rds")

# load in the input objects
input_setup <- readRDS(file = "settings/input_setup.rds")

# do a pull from the repo to get latest update & load the progress file
system("git pull")
progress <- readRDS(file = "settings/progress_table.rds")

#today = "2020/08/21"
#today = format(Sys.Date(), "%Y/%m/%d") #for actual date
today = format(Sys.time(),"%Y-%m-%d-%H-%M-%S") #for actual date

# limit sims to just iscen where catch multiplier is 1
xx <- mse_sim_setup %>% unnest()
catchmult <- xx$rowid[xx$iscen < 113]

# find the first nsim realizations that have yet to be done
todo <- progress %>% filter(is.na(user)) %>%
  filter(IBM != "ensemble") %>%   ## this removes the ensembles 
  filter(IBM != "JoeDLM") %>%   ## this removes the ensembles 
  filter(rowid %in% catchmult) %>% 
  slice(1:nsim) %>% select(rowid) %>% t() %>% as.numeric() %>% 
  I()

#specify those nsim to be being done
progress$user[progress$rowid %in% todo] <- user
progress$date_run[progress$rowid %in% todo] <- today

#################################
# update progress file - this will be done again after the simulations are finished in case some didn't work
# write the file back to disk & commit to update
saveRDS(progress, file = "settings/progress_table.rds")
#commit back to the repo so other users don't duplicate your efforts
system('git commit -am "updates progress table"')
system("git push")
#################################

# pull out realizations to be run today from the setup list using "todo". 
mse_sim_todo <- mse_sim_setup %>% 
  filter(rowid %in% todo) %>% 
  left_join(input_setup) %>% 
  #modify the input object here
  mutate(input = pmap(list(input=input, change=specs), change_input)) %>% 
  I()

### run the MSE over each row of the mse_sims todo
#add a safe mode (returns error safely rather than crashing)
safe_wham_mse <- purrr::safely(do_wham_mse_sim, otherwise = NA_real_)
#do the MSE for all simulations and scenarios
#profvis::profvis(
  #system.time(
mse_output <- mse_sim_todo %>% 
   mutate(wham = furrr::future_pmap(list(seed = seed, input = input),
                           safe_wham_mse, .options = my_future_options)) %>% 
# this is the regular purrr code for iterating over the simulations
#mutate(wham = purrr::pmap(list(seed = seed, input = input), do_wham_mse_sim))
  select(-input) %>% 
  I()
#) #ends profvis


#check which simulations ran
which_ran <- mse_output %>% 
  slice(which(!is.na(wham))) %>% 
  select(rowid) %>% 
  I()
which_ran <- as.integer(which_ran$rowid)

#update progress table with simulations that ran & those that didn't
yay <- todo[todo %in% which_ran]
nay <- todo[!(todo %in% which_ran)]
progress$uploaded[progress$rowid %in% yay] <- TRUE
progress$user[progress$rowid %in% nay] <- NA
progress$date_run[progress$rowid %in% nay] <- NA


#save the output
outfile = paste0("output/mse-",user,"-",str_remove_all(today,"-"),".rds")
saveRDS(mse_output, file = outfile)

# upload output to google drive
# rowid is part of the object so does not need to be in the filename
if (write_to_google) {
drive_upload(
  outfile,
  paste0("ibm-test/",outfile))
}


#################################
# update progress file - this will be done again after the simulations are finished in case some didn't work
# write the file back to disk & commit to update
saveRDS(progress, file = "settings/progress_table.rds")
#commit back to the repo so other users don't duplicate your efforts
system('git commit -am "updates progress table"')
system("git push")
#################################


} #end do_mse function


##########################
# example use
# specify how many realizations you want to run today
#nsim = 2*224
# who is doing them?
#user = "LB" # your initials
#do_mse(nsim = nsim, user = user, write_to_google = FALSE)

