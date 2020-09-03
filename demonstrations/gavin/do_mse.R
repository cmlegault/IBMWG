####

## install needed libraries & scripts
#devtools::install_github("timjmiller/wham", dependencies=TRUE)
library(wham)
library(tidyverse)
library(furrr)
library(googledrive)
#library(PlanBsmooth)
#library(RcppRoll)
rscripts <- list.files(path = "demonstrations/gavin/", pattern = "\\.R$")
rscripts <- str_subset(rscripts, "Run_MSE", negate = TRUE)
rscripts <- str_subset(rscripts, "do_mse", negate = TRUE)
rscripts <- str_subset(rscripts, "debug", negate = TRUE)
rscripts <- str_subset(rscripts, "setup_scenarios", negate = TRUE)
rscripts <- str_subset(rscripts, "summarize_results", negate = TRUE)
rscripts <- paste0("demonstrations/gavin/",rscripts)
map(rscripts, source)

##########################
# specify how many realizations you want to run today
nsim = 5
# who is doing them?
user = "GF"

do_mse <- function(nsim = nsim, user = user, write_to_google = TRUE) {
  
# set up the type of future (for parallelization of sims using furrr)
future::plan(future::multisession)

# load in the scenario specifications
mse_sim_setup <- readRDS(file = "demonstrations/gavin/mse_sim_setup.rds")

# load in the input objects
input_setup <- readRDS(file = "demonstrations/gavin/input_setup.rds")

# do a pull from the repo to get latest update & load the progress file
system("git pull")
progress <- readRDS(file = "demonstrations/gavin/progress_table.rds")

#today = "2020/08/21"
today = format(Sys.Date(), "%Y/%m/%d") #for actual date

# find the first nsim realizations that have yet to be done
todo <- progress %>% filter(is.na(user)) %>% 
  slice(1:nsim) %>% select(rowid) %>% t() %>% as.numeric() %>% 
  I()

#specify those nsim to be being done
progress$user[todo] <- user
progress$date_run[todo] <- today

#################################
# should this be done AFTER the sims are completed and pushed?
# write the file back to disk & commit to update
saveRDS(progress, file = "demonstrations/gavin/progress_table.rds")
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

#do the MSE for all simulations and scenarios
#profvis::profvis(
  #system.time(
mse_output <- mse_sim_todo %>% 
   mutate(wham = furrr::future_pmap(list(seed = seed, input = input),
                           do_wham_mse_sim)) %>% 
# this is the regular purrr code for iterating over the simulations
#mutate(wham = purrr::pmap(list(seed = seed, input = input), do_wham_mse_sim))
  select(-input) %>% 
  I()
#) #ends profvis



#save the output
outfile = paste0("output/demo-mse-",user,"-",str_remove_all(today,"/"),".rds")
saveRDS(mse_output, file = outfile)

# upload output to google drive
# rowid is part of the object so does not need to be in the filename
if (write_to_google) {
drive_upload(
  outfile,
  paste0("ibm-test/",outfile))
}

} #end do_mse function


##########################
# specify how many realizations you want to run today
nsim = 5
# who is doing them?
user = "GF"
do_mse(nsim = nsim, user = user, write_to_google = TRUE)

