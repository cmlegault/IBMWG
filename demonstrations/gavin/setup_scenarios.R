####


## install needed libraries & scripts  (some tidying needed here once all OK)
library(tidyverse)

rscripts <- list.files(path = "demonstrations/gavin/", pattern = "\\.R$")
rscripts <- str_subset(rscripts, "Run_MSE", negate = TRUE)
rscripts <- str_subset(rscripts, "do_mse", negate = TRUE)
rscripts <- str_subset(rscripts, "setup_scenarios", negate = TRUE)
rscripts <- paste0("demonstrations/gavin/",rscripts)
map(rscripts, source)
##

##

## get base input object
base_input <- get_base_input()
#base_input
base_input$IBM <- M_CC
base_input$adv.yr <- 2


## get scenario specifications
f_vecs <- read_csv("demonstrations/gavin/base_f_vecs.csv") %>% 
  rowid_to_column(var = "t") %>% 
  pivot_longer(names_to = "f_pattern", values_to = "f",
               cols = -t) %>% 
  separate(f_pattern, into = c("dummy", "f_pattern"),
           convert = TRUE) %>% 
  select(f_pattern, f) %>% 
  group_by(f_pattern) %>% 
  nest() %>% 
  rename("F" = data) %>% 
  I()
#f_vecs is the fixed time series of base period F
# this will need additional options to run the settings for the scenarios
# e.g. calls to get the base period Fs for different F scenarios

# get scenario settings
# we would update this  TO INCLUDE CL FULL FACTORIAL, make one file
base_specs <- read_csv("demonstrations/gavin/base-scenarios.csv") %>% 
  left_join(f_vecs) %>% 
  mutate(F = map(F, as.matrix),
         mean_rec_pars = rep(list(base_input$mean_rec_pars),nrow(.)),
         mean_rec_pars = map2(mean_rec_pars, steepness, ~put_h(.x, .y))) %>% 
  select(-f_pattern, -steepness) %>% 
  I()
projection_specs <- read_csv("demonstrations/gavin/proj-scenarios.csv")
base_specs <- base_specs %>% 
  mutate(projection_specs = map(seq_len(nrow(.)),~I(projection_specs))) %>% 
  unnest(cols = c(projection_specs)) %>% 
  I()
#base_specs then contains the settings for all scenarios


# get settings for scenarios want to run
#run default and one change in this run
base_scens <- c(0,1)
mse_setup <- base_specs %>% 
  filter(base_scen %in% base_scens) %>% 
  select(-retro_type) %>% 
  group_by(base_scen, proj_scen) %>% 
  nest() %>% 
  rename("specs" = data) %>% 
  left_join(select(base_specs, base_scen, retro_type) %>% group_by(base_scen) %>% slice(1)) %>% 
  I()
#mse_setup

## modify base input for chosen scenarios
mse_setup <- mse_setup %>% 
  ungroup() %>% 
  mutate(input = rep(list(base_input), nrow(.)),
         input = map2(specs, input, ~change_input(.y, .x))) %>% 
  I()
#mse_setup


### add rows for each realization for each scenario

#dimension problem
nbase <- nrow(mse_setup) #number of scenarios
nsim <- 1000
#nprojyrs <- 40

#read in seeds
seeds <- read_csv("code/RNG.seeds.csv",col_names=FALSE) %>% 
  t() %>% 
  as.numeric() %>% 
  I()

sim_seeds <- tibble(isim = 1:nsim,
                    seed = seeds)

#set up output tibble for each simulation
mse_sim_setup <- mse_setup %>% 
  mutate(sim_seeds = map(seq_len(nrow(.)),~I(sim_seeds))) %>% 
  unnest(cols = sim_seeds) %>% 
  rowid_to_column() %>% 
  select(rowid,everything()) %>% 
  I()
mse_sim_setup

# mse_sim_setup now contains a full settings list for all the realizations of all scenarios
saveRDS(mse_sim_setup, file = "demonstrations/gavin/mse_sim_setup.rds")

## create lookup for which runs have been done

progress <- mse_sim_setup %>% 
  select(rowid) %>% 
  mutate(user = rep(NA, nrow(.)),
         date_run = rep(NA, nrow(.)))
saveRDS(progress, file = "demonstrations/gavin/progress_table.rds")



