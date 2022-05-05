# 1_compile_data.R
# get data and store as csv or RDS for easy access and use in making figures for CJFAS paper
# uses cleaned performance metrics from base, full SCAA, and no retro  

library(tidyverse)

# set working directory to source file location to begin

# get reference points
# note: we used year 50 for reporting refpts in WG report
# to get alternative refpts using year 1, multiply by ratio reftpt50/refpt1
refpts <- read.csv("../../demonstrations/chris/base_sims/refpts.csv", 
                   stringsAsFactors = FALSE) %>%
  mutate(ssbmsyratio = ssbmsy50 / ssbmsy1,
         fmsyratio = fmsy50 / fmsy1,
         msyratio = msy50 / msy1) %>%
  mutate(Fhist = ifelse(Fhist == "F", 1, 2))

# get results and simulation set up information
mse_results <- readRDS("../../results/perform-metrics_clean_full_DLM.rds")
scaa_results <- readRDS("../../results/perform-metrics_full_scaa.rds") # note full
mse_sim_setup <- readRDS("../../settings/mse_sim_setup.rds")

# remove duplicate runs
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]
dd <- duplicated(mse_results$rowid)
mse_results <- mse_results[!dd,]

# check number of simulations per scenario
count_table <- mse_results %>%
  group_by(iscen) %>%
  summarise(n = n())
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
    IBM == "planBsmoothfxn" ~ "Ismooth",
    IBM == "ExpandSurvey_modified" ~ paste("ES", EMlab[expand_method], sep="-"),
    IBM == "run.aim" ~ "AIM",
    IBM == "JoeDLM" ~ "DynLin",
    IBM == "ensemble" ~ "Ensemble",
    TRUE ~ IBM),
    Scenlab = paste(substr(retro_type, 1, 1), Fhistlab[Fhist], Sellab[n_selblocks], ifelse(catch.mult == 1, CMlab[1], CMlab[2]), sep = ""))
defined
names(defined)
unique(defined$IBMlab)
unique(defined$Scenlab)

defined_scaa <- defined %>%
  filter(IBMlab == "Ensemble") %>%
  mutate(IBM = "SCAA",
         IBMlab = "SCAA")

###pull out the ssb metrics
ssb_results <- mse_results %>% 
  select(iscen, isim, ssb_metrics) %>% 
  mutate(ssb_metrics = map(ssb_metrics, enframe)) %>% 
  unnest(cols = c(ssb_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
ssb_results
unique(ssb_results$metric)

###pull out the f metrics
f_results <- mse_results %>% 
  select(iscen, isim, f_metrics) %>% 
  mutate(f_metrics = map(f_metrics, enframe)) %>% 
  unnest(cols = c(f_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
f_results
unique(f_results$metric)

###pull out the catch metrics
catch_results <- mse_results %>% 
  select(iscen, isim, catch_metrics) %>% 
  mutate(catch_metrics = map(catch_metrics, enframe)) %>% 
  unnest(cols = c(catch_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
catch_results
unique(catch_results$metric)

###pull out the ssb metrics for scaa runs
ssb_results_scaa <- scaa_results %>% 
  select(iscen, isim, ssb_metrics) %>% 
  mutate(ssb_metrics = map(ssb_metrics, enframe)) %>% 
  unnest(cols = c(ssb_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
ssb_results_scaa
unique(ssb_results_scaa$metric)

###pull out the f metrics for scaa runs
f_results_scaa <- scaa_results %>% 
  select(iscen, isim, f_metrics) %>% 
  mutate(f_metrics = map(f_metrics, enframe)) %>% 
  unnest(cols = c(f_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
f_results_scaa
unique(f_results_scaa$metric)

###pull out the catch metrics for scaa
catch_results_scaa <- scaa_results %>% 
  select(iscen, isim, catch_metrics) %>% 
  mutate(catch_metrics = map(catch_metrics, enframe)) %>% 
  unnest(cols = c(catch_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
catch_results_scaa
unique(catch_results_scaa$metric)

# subset to the metrics of interest
ssb_sims <- ssb_results %>%
  filter(grepl("avg_ssb_ssbmsy", metric)) %>%
  inner_join(defined, by = c("iscen"))

ssb_sims_scaa <- ssb_results_scaa %>%
  filter(grepl("avg_ssb_ssbmsy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

f_sims <- f_results %>%
  filter(grepl("avg_f_fmsy", metric)) %>%
  inner_join(defined, by = c("iscen"))

f_sims_scaa <- f_results_scaa %>%
  filter(grepl("avg_f_fmsy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

catch_sims <- catch_results %>%
  filter(grepl("avg_catch_msy", metric)) %>%
  inner_join(defined, by = c("iscen"))

catch_sims_scaa <- catch_results_scaa %>%
  filter(grepl("avg_catch_msy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

ciav_sim <- catch_results %>%
  filter(metric == "a_iav_catch") %>%
  inner_join(defined, by = c("iscen"))

ciav_sim_scaa <- catch_results_scaa %>%
  filter(metric == "a_iav_catch") %>%
  inner_join(defined_scaa, by = c("iscen"))

noverfished <- ssb_results %>%
  filter(grepl("n_less_05_bmsy", metric)) %>%
  group_by(iscen, isim) %>%
  summarize(sumval = sum(value)) %>%
  mutate(metric = "a_n_less_05_bmsy") %>%
  rename(value = sumval) %>%
  inner_join(defined, by = c("iscen"))

noverfished_scaa <- ssb_results_scaa %>%
  filter(grepl("n_less_05_bmsy", metric)) %>%
  group_by(iscen, isim) %>%
  summarize(sumval = sum(value)) %>%
  mutate(metric = "a_n_less_05_bmsy") %>%
  rename(value = sumval) %>%
  inner_join(defined_scaa, by = c("iscen"))

noverfishing <- f_results %>%
  filter(grepl("n_gr_fmsy", metric)) %>%
  group_by(iscen, isim) %>%
  summarize(sumval = sum(value)) %>%
  mutate(metric = "a_n_gr_fmsy") %>%
  rename(value = sumval) %>%
  inner_join(defined, by = c("iscen"))

noverfishing_scaa <- f_results_scaa %>%
  filter(grepl("n_gr_fmsy", metric)) %>%
  group_by(iscen, isim) %>%
  summarize(sumval = sum(value)) %>%
  mutate(metric = "a_n_gr_fmsy") %>%
  rename(value = sumval) %>%
  inner_join(defined_scaa, by = c("iscen"))

sims <- rbind(ssb_sims, ssb_sims_scaa,
              f_sims, f_sims_scaa,
              catch_sims, catch_sims_scaa,
              ciav_sim, ciav_sim_scaa,
              noverfished, noverfished_scaa,
              noverfishing, noverfishing_scaa) %>%
  mutate(time.avg = case_when(substr(metric,1,1) == "l" ~ "L", 
                              substr(metric,1,1) == "s" ~ "S",
                              substr(metric,1,1) == "a" ~ "A")) %>%
  mutate(metric = substr(metric, 3, 99)) %>% 
  select(value, iscen, isim, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult, time.avg, Scenlab, metric) 

saveRDS(sims, file="sims.RDS")


