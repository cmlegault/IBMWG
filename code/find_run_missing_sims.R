# find_run_missing_sims.R
# find and run the four missing simulations to examine what went wrong

#mid-September 2020 version 0.0.9000 of wham
83d0333bab4b3839eb671ec96318e6943722b1a5
#devtools::install_github("timjmiller/wham", dependencies=TRUE, ref="00f8788fa77ffddb842e45653bec956c7bdb710b") #9/11/20
devtools::install_github("timjmiller/wham", dependencies=TRUE, ref="83d0333bab4b3839eb671ec96318e6943722b1a5") #9/16/20
devtools::install_github("timjmiller/wham", dependencies=TRUE, ref="1ddfd9edc1f40720dff990c24078b9b59b99af78") #9/23/20
library(wham)
library(tidyverse)
library(furrr)

rscripts <- c("code/base_input.R",
              "code/change_input.R",
              "code/IBM_options.R",
              #"code/performance_metrics.R",
              "code/wham_mse_functions.R",
              "code/wham_retro_functions.R")
map(rscripts, source)
my_future_options = future_options()
my_future_options$globals = ls()
my_future_options$packages <- c("wham",
                                "tidyverse",
                                "dlm",
                                "RandomFieldsUtils")

# get results and simulation set up information
mse_results <- readRDS("results/perform-metrics_clean_full_DLM.rds")
scaa_results <- readRDS("results/perform-metrics_full_scaa.rds") # note full
mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")
input_setup <- readRDS(file = "settings/input_setup.rds")

# remove duplicate runs
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]

# check number of simlations per scenario
count_table <- mse_results %>%
  group_by(iscen) %>%
  summarise(n = length(unique(isim)))
count_table$n

### join with setup to figure out what's in each scenario
Fhistlab <- c("F","O") # O = always overfishing, F = Fmsy in last year of base
Sellab <- c("1", "2") # just whether 1 or 2 blocks for selectivity
CMlab <- c("A", "R") # A = catch advice applied, R = reduced (mult=0.75)
EMlab <- c("FSPR", "Fstable", "FM", "Frecent")
CClab <- c("FSPR", NA, "FM")
defined <- mse_sim_setup %>%
  filter(rowid %in% not_dupes) %>% 
  filter(isim == 1) %>%
  select(iscen, specs) %>%
  unnest(cols = specs) %>%
  inner_join(count_table, by="iscen") %>%
  mutate(IBMlab = case_when(
    IBM == "true_Skate_CR" ~ "Skate",
    IBM == "M_CC" ~ paste("CC", CClab[M_CC_method], sep="-"),
    IBM == "planBsmoothfxn" ~ "PBS",
    IBM == "ExpandSurvey_modified" ~ paste("ES", EMlab[expand_method], sep="-"),
    IBM == "run.aim" ~ "AIM",
    IBM == "JoeDLM" ~ "DLM",
    IBM == "ensemble" ~ "Ensemble",
    TRUE ~ IBM),
    Scenlab = paste(substr(retro_type, 1, 1), Fhistlab[Fhist], Sellab[n_selblocks], ifelse(catch.mult == 1, CMlab[1], CMlab[2]), sep = ""))
defined
names(defined)
unique(defined$IBMlab)
unique(defined$Scenlab)

# now find the bad runs
bad1 <- defined %>%
  filter(IBMlab == "AIM",
         retro_type == "Catch",
         Fhist == 1,
         n_selblocks == 1,
         catch.mult == 1)

bad1sims <- mse_results %>%
  filter(iscen == bad1$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad1$missing_isim <- setdiff(1:1000, bad1sims$isim)

bad2 <- defined %>%
  filter(IBMlab == "AIM",
         retro_type == "Catch",
         Fhist == 2,
         n_selblocks == 1,
         catch.mult == 1)

bad2sims <- mse_results %>%
  filter(iscen == bad2$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad2$missing_isim <- setdiff(1:1000, bad2sims$isim)

bad3 <- defined %>%
  filter(IBMlab == "ES-Fstable",
         retro_type == "Catch",
         Fhist == 2,
         n_selblocks == 1,
         catch.mult == 1)

bad3sims <- mse_results %>%
  filter(iscen == bad3$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad3$missing_isim <- setdiff(1:1000, bad3sims$isim)

bad4 <- defined %>%
  filter(IBMlab == "ES-Fstable",
         retro_type == "Catch",
         Fhist == 2,
         n_selblocks == 2,
         catch.mult == 1)

bad4sims <- mse_results %>%
  filter(iscen == bad4$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad4$missing_isim <- setdiff(1:1000, bad4sims$isim)

fourbad <- data.frame(iscen = c(bad1$iscen, bad2$iscen, bad3$iscen, bad4$iscen),
                      isim = c(bad1$missing_isim, 
                               bad2$missing_isim, 
                               bad3$missing_isim, 
                               bad4$missing_isim))
fourbad

left_join(fourbad, mse_sim_setup, by = c("iscen", "isim")) %>%
  select(iscen, isim, rowid, seed)

# select just the four bad runs
todo <- left_join(fourbad, mse_sim_setup, by = c("iscen", "isim")) %>% 
  select(rowid) %>% t() %>% as.numeric() %>% 
  I()

# pull out realizations to be run today from the setup list using "todo". 
source("code/wham_mse_functions.R")
mse_sim_todo <- mse_sim_setup %>% 
  filter(rowid %in% todo) %>% 
  left_join(input_setup) %>% 
  #modify the input object here
  mutate(input = pmap(list(input=input, change=specs), change_input)) %>% 
  I()

#this will do the run for the first "row" of mse_sim_todo which throws the error
#You can see that one of the last relative Fs is 0 and then the log provides -Inf and wrecks the call to lm() 
x = do_wham_mse_sim(mse_sim_todo$seed[1],mse_sim_todo$input[[1]])

#rows 2 and 4 seems to run?
x = do_wham_mse_sim(mse_sim_todo$seed[2],mse_sim_todo$input[[2]])
x = do_wham_mse_sim(mse_sim_todo$seed[3],mse_sim_todo$input[[3]])
x = do_wham_mse_sim(mse_sim_todo$seed[4],mse_sim_todo$input[[4]])

### run the MSE over each row of the mse_sims todo
#add a safe mode (returns error safely rather than crashing)
safe_wham_mse <- purrr::safely(do_wham_mse_sim, otherwise = NA_real_)
#do the MSE for all simulations and scenarios
#profvis::profvis(
#system.time(
mse_output <- mse_sim_todo %>% 
  #mutate(wham = furrr::future_pmap(list(seed = seed, input = input),
  #                                 safe_wham_mse, .options = my_future_options)) %>% 
  # this is the regular purrr code for iterating over the simulations
  mutate(wham = purrr::pmap(list(seed = seed, input = input), do_wham_mse_sim))
  select(-input) %>% 
  I()
#) #ends profvis


# get the following Error message when run mse_output line:
  Error: Problem with `mutate()` input `wham`.
  x NA/NaN/Inf in 'x'
  ℹ Input `wham` is `purrr::pmap(list(seed = seed, input = input), do_wham_mse_sim)`.
