####


## install needed libraries & scripts  (some tidying needed here once all OK)
library(tidyverse)
library(wham)

rscripts <- list.files(path = "demonstrations/gavin/", pattern = "\\.R$")
rscripts <- str_subset(rscripts, "Run_MSE", negate = TRUE)
rscripts <- str_subset(rscripts, "do_mse", negate = TRUE)
rscripts <- str_subset(rscripts, "debug", negate = TRUE)
rscripts <- str_subset(rscripts, "setup_scenarios", negate = TRUE)
rscripts <- str_subset(rscripts, "summarize_results", negate = TRUE)
rscripts <- paste0("demonstrations/gavin/",rscripts)
map(rscripts, source)
source("demonstrations/tim/wham_retro_functions.R")
##

##
# 
# ## get base input object
# base_input <- get_base_input()
# #base_input
# base_input$IBM <- M_CC
# base_input$adv.yr <- 2
# base_input$nprojyrs <- 40
# base_input$Fhist <- 1
# base_input$n_selblocks <- 1
# base_input$retro_type <- "M"
# base_input$catch.mult <- 1
# base_input$expand_method <- 4
# base_input$M_CC_method <- 3
# 

#n_selblocks = 1 #constant fishery selectivity
n_selblocks = 2 #if = 2 second half of base period (and feedback period) has alternative fishery selectivity (a50= 5)
Fmsy_scale = 2.5 #What proportion of Fmsy to fish at
#Fhist = 1 #Fish at Fmsy_scale*Fmsy (defined by recent selectivity) for first half of base, then Fmsy second half
Fhist = 2 #Fish at Fmsy_scale*Fmsy (defined by recent selectivity) for all of base period.
#input=get_base_input(n_selblocks, Fhist, Fmsy_scale)
#temp = fit_wham(input, do.fit = FALSE)

# 
# ## get scenario specifications
# f_vecs <- read_csv("demonstrations/gavin/base_f_vecs.csv") %>% 
#   rowid_to_column(var = "t") %>% 
#   pivot_longer(names_to = "f_pattern", values_to = "f",
#                cols = -t) %>% 
#   separate(f_pattern, into = c("dummy", "f_pattern"),
#            convert = TRUE) %>% 
#   select(f_pattern, f) %>% 
#   group_by(f_pattern) %>% 
#   nest() %>% 
#   rename("F" = data) %>% 
#   I()
# #f_vecs is the fixed time series of base period F
# # this will need additional options to run the settings for the scenarios
# # e.g. calls to get the base period Fs for different F scenarios
# 
# # GF: I think this is covered by the Fhist switch within `fit_wham_mse()`
# 
# # get scenario settings
# # we would update this  TO INCLUDE CL FULL FACTORIAL, make one file
# base_specs <- read_csv("demonstrations/gavin/base-scenarios.csv") %>% 
#   left_join(f_vecs) %>% 
#   mutate(F = map(F, as.matrix),
#          mean_rec_pars = rep(list(base_input$mean_rec_pars),nrow(.)),
#          mean_rec_pars = map2(mean_rec_pars, steepness, ~put_h(.x, .y))) %>% 
#   select(-f_pattern, -steepness) %>% 
#   I()
# projection_specs <- read_csv("demonstrations/gavin/proj-scenarios.csv")
# base_specs <- base_specs %>% 
#   mutate(projection_specs = map(seq_len(nrow(.)),~I(projection_specs))) %>% 
#   unnest(cols = c(projection_specs)) %>% 
#   I()
# #base_specs then contains the settings for all scenarios

### Pull settings from CL Design Matrix specification file
###
retro_lookup <- tibble(RetroSource = c("Missing Catch", "Increased M"),
                       retro_type = c("Catch","M"))

F_lookup <- tibble(Fhistory = c("always overfishing", "overfishing then fmsy"),
                   Fhist = c(2,1))
sel_lookup <- tibble(FisherySelectivity = c("Constant", 
                                            "Time-varying"),
                     n_selblocks = c(1,2))

#xx <- f_vecs$F[[1]]

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
# get settings for scenarios want to run
#run default and one change in this run
#base_scens <- c(0,1)
saveRDS(input_setup, file = "settings/input_setup.rds")


# mse_setup <- nu_specs %>% 
#     #  mse_setup <- base_specs %>% 
#   #  filter(base_scen %in% base_scens) %>% 
#   #  select(-retro_type) %>% 
#   #  group_by(base_scen, proj_scen) %>% 
#   nest_by(iscen) %>% 
#   rename("specs" = data) %>% 
#   #  left_join(select(base_specs, base_scen, retro_type) %>% group_by(base_scen) %>% slice(1)) %>% 
#   left_join(input_setup) %>% 
#   #slice(1:5) %>% 
#   #mutate(input2 = map2(specs, input, ~change_input(.y, .x))) %>%   
#   I()
# for (i in 1:nrow(mse_setup))
#   mse_setup$input[[i]] <- change_input(change = mse_setup$specs[[i]], input = mse_setup$input[[i]])
# 
# ## modify base input for chosen scenarios
# mse_setup <- mse_setup %>% 
#   ungroup() %>% 
#   mutate(input = rep(list(base_input), nrow(.)),
#          input = map2(specs, input, ~change_input(.y, .x))) %>% 
#   I()


# create the specifications setup table - input not changed here now due to object size
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
#nprojyrs <- 40

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
