library(tidyverse)
library(wham)

rscripts <- c("code/base_input.R",
              "code/change_input.R",
              "code/IBM_options.R",
              "code/wham_mse_functions.R",
              "code/wham_retro_functions.R")
map(rscripts, source)

### Pull settings from Design Matrix specification file
retro_lookup <- tibble(RetroSource = c("Missing Catch", "Increased M"),
                       retro_type = c("Catch","M"))

F_lookup <- tibble(Fhistory = c("always overfishing", "overfishing then fmsy"),
                   Fhist = c(2,1))
sel_lookup <- tibble(FisherySelectivity = c("Constant", 
                                            "Time-varying"),
                     n_selblocks = c(1,2))

nu_specs <- read_csv("settings/scenarios.csv")  %>% 
  left_join(retro_lookup) %>% 
  left_join(F_lookup) %>% 
  left_join(sel_lookup) %>% 
  mutate(expand_method = case_when(
    IBM == "ExpandSurvey_F%SPR" ~ 1,
    IBM == "ExpandSurvey_stableperiodF" ~ 2,
    IBM == "ExpandSurvey_FequalM" ~ 3,
    TRUE ~ 4),
    M_CC_method = ifelse(IBM=="CatchCurve_F%SPR",1,3),
    IBM = case_when(
      IBM == "PlanBsmooth" ~ "ApplyPlanBsmooth_fast", 
      str_detect(.$IBM,"ExpandSurvey") ~ "ExpandSurvey_modified",
      IBM == "AIM" ~ "run.aim",
      str_detect(.$IBM, "CatchCurve") ~ "M_CC",
      IBM == "Ensemble" ~ "ensemble",
      TRUE ~ IBM),
    adv.yr = rep(2, nrow(.)),
    nprojyrs = rep(40, nrow(.)),
    iscen = row_number(),
    Fmsy_scale = rep(2.5,nrow(.)),
    catch.mult = CatchMultiplier) %>% 
  select(iscen, retro_type, Fhist, n_selblocks, IBM, adv.yr, Fmsy_scale, catch.mult, expand_method,
         M_CC_method, nprojyrs) %>% 
  I()


base_input <- expand.grid(Fhist = 1:2,
                  n_selblocks = 1:2,
                  Fmsy_scale = 2.5) %>% 
  tibble() %>% 
  mutate(input = pmap(list(n_selblocks = n_selblocks, Fhist = Fhist, Fmsy_scale = Fmsy_scale),
                    get_base_input)) %>% 
  I()

input_setup <- nu_specs %>% 
  left_join(base_input) %>% 
  select(iscen, input) %>% 
  I()
input_setup
saveRDS(input_setup, file = "settings/input_setup.rds")


###################
# need to add the one-offs to this table
###################

mse_setup <- nu_specs %>% 
  nest_by(iscen) %>% 
  rename("specs" = data) %>% 
  I()


### add rows for each realization for each scenario``

#dimension problem
nbase <- nrow(mse_setup) #number of scenarios
nsim <- 1000

#read in seeds
seeds <- read_csv("settings/RNG.seeds.csv",col_names=FALSE) %>% 
  t() %>% 
  as.numeric() %>% 
  I()

sim_seeds <- tibble(isim = 1:nsim,
                    seed = seeds)

#set up output tibble for each simulation
mse_sim_setup <- mse_setup %>% 
  mutate(sim_seeds = list(sim_seeds)) %>%
  unnest(cols = sim_seeds) %>% 
  rowid_to_column() %>% 
  select(rowid,everything()) %>% 
  ungroup() %>% 
  I()
mse_sim_setup

# mse_sim_setup now contains a full settings list for all the realizations of all scenarios
saveRDS(mse_sim_setup, file = "settings/mse_sim_setup.rds")

## create lookup for which runs have been done

progress <- mse_sim_setup %>% 
  ungroup() %>% 
  select(rowid) %>% 
  mutate(user = rep(NA, nrow(.)),
         date_run = rep(NA, nrow(.)),
         uploaded = rep(NA, nrow(.)))
saveRDS(progress, file = "settings/progress_table.rds")
