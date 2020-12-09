# make_tables_figures.R
# uses cleaned performance metrics from base, SCAA, and no retro sets 
# to create summary tables and figures
# output goes to tables_figs directory

library(tidyverse)

# get resuls and simulation set up information
mse_results <- readRDS("results/perform-metrics_clean.rds")
scaa_results <- readRDS("results/perform-metrics_scaa.rds")
noretro_results <- readRDS("results/perform-metrics_noretro.rds")
mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")

# remove duplicate runs
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]

mse_results <- mse_results %>%
  filter(rowid %in% not_dupes)

scaa_results <- scaa_results %>%
  filter(rowid %in% not_dupes)

noretro_results <- noretro_results %>%
  filter(rowid %in% not_dupes)

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
    IBM == "planBsmoothfxn" ~ "PlanB",
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

defined_noretro <- defined %>%
  filter(IBMlab != "DLM",
         retro_type == "Catch",
         n_selblocks == 1,
         catch.mult == 1) %>%
  mutate(retro_type = "None",
         Scenlab = paste0("N", Fhistlab[Fhist], "1A"))

defined_scaa <- defined %>%
  filter(IBMlab == "Ensemble",
         n_selblocks == 1,
         catch.mult == 1) %>%
  mutate(IBM = "SCAA",
         IBMlab = "SCAA")

# counting scenarios and simulations
countIBM <- defined %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

countIBM_noretro <- defined_noretro %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

countIBM_scaa <- defined_scaa %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

nscentab <- rbind(countIBM, countIBM_noretro, countIBM_scaa) %>%
  select(IBMlab, Scenlab, nscenarios) %>%
  pivot_wider(names_from = Scenlab, values_from = nscenarios)
nscentab

nsimtab <- rbind(countIBM, countIBM_noretro, countIBM_scaa) %>%
  select(IBMlab, Scenlab, nsim) %>%
  pivot_wider(names_from = Scenlab, values_from = nsim)
nsimtab
write.csv(nsimtab, file = "tables_figs/nsimtab.csv", row.names = FALSE)

nsim_plot <- ggplot(rbind(countIBM, countIBM_noretro, countIBM_scaa), 
                    aes(x=Scenlab, y=nsim)) +
  geom_bar(stat = "identity") +
  facet_wrap(~IBMlab) +
  labs(x="Scenario", y="Number of Simulations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# helpful decoder of abbreviations for scenarios
scendf <- data.frame(x=rep(1, 5),
                     y=seq(5, 1, -1),
                     mytext=c("CF1A to MO2R explained",
                              "Retrotype: C=catch, M=natural mortality, N=None",
                              "F history: F=overfishing then Fmsy, O=Always Overfishing",
                              "Selblocks: 1 or 2",
                              "Catch Advice Multiplier: A=1 (applied), R=0.75 (reduced"))

scenlab_plot <- ggplot(scendf, aes(x=x, y=y)) +
  geom_text(aes(label = mytext)) +
  expand_limits(y=c(0,6)) +
  labs(title="Scenario Label Decoder") +
  theme_void()

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

###pull out the ssb metrics for noretro runs
ssb_results_noretro <- noretro_results %>% 
  select(iscen, isim, ssb_metrics) %>% 
  mutate(ssb_metrics = map(ssb_metrics, enframe)) %>% 
  unnest(cols = c(ssb_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
ssb_results_noretro
unique(ssb_results_noretro$metric)

###pull out the f metrics for noretro runs
f_results_noretro <- noretro_results %>% 
  select(iscen, isim, f_metrics) %>% 
  mutate(f_metrics = map(f_metrics, enframe)) %>% 
  unnest(cols = c(f_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
f_results_noretro
unique(f_results_noretro$metric)

###pull out the catch metrics for noretro
catch_results_noretro <- noretro_results %>% 
  select(iscen, isim, catch_metrics) %>% 
  mutate(catch_metrics = map(catch_metrics, enframe)) %>% 
  unnest(cols = c(catch_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
catch_results_noretro
unique(catch_results_noretro$metric)

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

### compute mean for all metrics for each scenario
ssb_mean_by_scenario <- ssb_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined, by = "iscen")

f_mean_by_scenario <- f_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined, by = "iscen")

catch_mean_by_scenario <- catch_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined, by = "iscen")

ssb_mean_by_scenario_noretro <- ssb_results_noretro %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined_noretro, by = "iscen")

f_mean_by_scenario_noretro <- f_results_noretro %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined_noretro, by = "iscen")

catch_mean_by_scenario_noretro <- catch_results_noretro %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined_noretro, by = "iscen")

ssb_mean_by_scenario_scaa <- ssb_results_scaa %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined_scaa, by = "iscen")

f_mean_by_scenario_scaa <- f_results_scaa %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined_scaa, by = "iscen")

catch_mean_by_scenario_scaa <- catch_results_scaa %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  inner_join(defined_scaa, by = "iscen")

# get main results corresponding to scenarios done for SCAA and noretro
noretro_scen <- unique(ssb_mean_by_scenario_noretro$Scenlab)
scaa_scen <- unique(ssb_mean_by_scenario_scaa$Scenlab)

ssb_mean_by_scenario_scen <- ssb_mean_by_scenario %>%
  filter(Scenlab %in% scaa_scen)

f_mean_by_scenario_scen <- f_mean_by_scenario %>%
  filter(Scenlab %in% scaa_scen)

catch_mean_by_scenario_scen <- catch_mean_by_scenario %>%
  filter(Scenlab %in% scaa_scen)

# combine noretro and and base limited to scaa scenarios into noretro
# remove DLM from base runs because not run for no retro
ssb_mean_by_scenario_noretro <- rbind(ssb_mean_by_scenario_noretro,
                                      filter(ssb_mean_by_scenario_scen,
                                             IBMlab != "DLM"))

f_mean_by_scenario_noretro <- rbind(f_mean_by_scenario_noretro,
                                    filter(f_mean_by_scenario_scen,
                                           IBMlab != "DLM"))

catch_mean_by_scenario_noretro <- rbind(catch_mean_by_scenario_noretro,
                                        filter(catch_mean_by_scenario_scen,
                                               IBMlab != "DLM"))

# combine scaa and base limited to scaa scenarios into scaa
ssb_mean_by_scenario_scaa <- rbind(ssb_mean_by_scenario_scaa,
                                   ssb_mean_by_scenario_scen)

f_mean_by_scenario_scaa <- rbind(f_mean_by_scenario_scaa,
                                 f_mean_by_scenario_scen)

catch_mean_by_scenario_scaa <- rbind(catch_mean_by_scenario_scaa,
                                     catch_mean_by_scenario_scen)

### save mean_by_scenario results for easier modeling and ranking
saveRDS(ssb_mean_by_scenario, 
        file = "tables_figs/ssb_mean_by_scenario.rds")

saveRDS(f_mean_by_scenario,
        file = "tables_figs/f_mean_by_scenario.rds")

saveRDS(catch_mean_by_scenario, 
        file = "tables_figs/catch_mean_by_scenario.rds")

saveRDS(ssb_mean_by_scenario_noretro, 
        file = "tables_figs/ssb_mean_by_scenario_noretro.rds")

saveRDS(f_mean_by_scenario_noretro,
        file = "tables_figs/f_mean_by_scenario_noretro.rds")

saveRDS(catch_mean_by_scenario_noretro, 
        file = "tables_figs/catch_mean_by_scenario_noretro.rds")

saveRDS(ssb_mean_by_scenario_scaa, 
        file = "tables_figs/ssb_mean_by_scenario_scaa.rds")

saveRDS(f_mean_by_scenario_scaa,
        file = "tables_figs/f_mean_by_scenario_scaa.rds")

saveRDS(catch_mean_by_scenario_scaa, 
        file = "tables_figs/catch_mean_by_scenario_scaa.rds")

### make subsets of results for easier plotting
ssb_probs <- ssb_mean_by_scenario %>%
  filter(grepl("_is_", metric))
  
ssb_ns <- ssb_mean_by_scenario %>%
  filter(grepl("_n_", metric))

ssb_ratios <- ssb_mean_by_scenario %>%
  filter(grepl("_avg_ssb_ssbmsy", metric))

f_probs <- f_mean_by_scenario %>%
  filter(grepl("_is_", metric))

f_ns <- f_mean_by_scenario %>%
  filter(grepl("_n_", metric))

f_ratios <- f_mean_by_scenario %>%
  filter(grepl("_avg_f_fmsy", metric))

catch_means <- catch_mean_by_scenario %>%
  filter(metric %in% c("l_avg_catch", "l_sd_catch", "s_avg_catch", "s_sd_catch"))

catch_ratios <- catch_mean_by_scenario %>%
  filter(grepl("_avg_catch_msy", metric))

catch_other <- catch_mean_by_scenario %>%
  filter(metric %in% c("a_iav_catch", "l_iav_catch", "s_iav_catch", "l_prop_g_msy_2_of_3"))

ssb_probs_noretro <- ssb_mean_by_scenario_noretro %>%
  filter(grepl("_is_", metric))

ssb_ns_noretro <- ssb_mean_by_scenario_noretro %>%
  filter(grepl("_n_", metric))

ssb_ratios_noretro <- ssb_mean_by_scenario_noretro %>%
  filter(grepl("_avg_ssb_ssbmsy", metric))

f_probs_noretro <- f_mean_by_scenario_noretro %>%
  filter(grepl("_is_", metric))

f_ns_noretro <- f_mean_by_scenario_noretro %>%
  filter(grepl("_n_", metric))

f_ratios_noretro <- f_mean_by_scenario_noretro %>%
  filter(grepl("_avg_f_fmsy", metric))

catch_means_noretro <- catch_mean_by_scenario_noretro %>%
  filter(metric %in% c("l_avg_catch", "l_sd_catch", "s_avg_catch", "s_sd_catch"))

catch_ratios_noretro <- catch_mean_by_scenario_noretro %>%
  filter(grepl("_avg_catch_msy", metric))

catch_other_noretro <- catch_mean_by_scenario_noretro %>%
  filter(metric %in% c("a_iav_catch", "l_iav_catch", "s_iav_catch", "l_prop_g_msy_2_of_3"))

ssb_probs_scaa <- ssb_mean_by_scenario_scaa %>%
  filter(grepl("_is_", metric))

ssb_ns_scaa <- ssb_mean_by_scenario_scaa %>%
  filter(grepl("_n_", metric))

ssb_ratios_scaa <- ssb_mean_by_scenario_scaa %>%
  filter(grepl("_avg_ssb_ssbmsy", metric))

f_probs_scaa <- f_mean_by_scenario_scaa %>%
  filter(grepl("_is_", metric))

f_ns_scaa <- f_mean_by_scenario_scaa %>%
  filter(grepl("_n_", metric))

f_ratios_scaa <- f_mean_by_scenario_scaa %>%
  filter(grepl("_avg_f_fmsy", metric))

catch_means_scaa <- catch_mean_by_scenario_scaa %>%
  filter(metric %in% c("l_avg_catch", "l_sd_catch", "s_avg_catch", "s_sd_catch"))

catch_ratios_scaa <- catch_mean_by_scenario_scaa %>%
  filter(grepl("_avg_catch_msy", metric))

catch_other_scaa <- catch_mean_by_scenario_scaa %>%
  filter(metric %in% c("a_iav_catch", "l_iav_catch", "s_iav_catch", "l_prop_g_msy_2_of_3"))

### plotting functions  
confetti_plot <- function(mytib, myx, myy, myxlab, myylab, mytitle){
  myx <- enquo(myx)
  myy <- enquo(myy)
  myplot <- ggplot(mytib, aes(x = !! myx, y = !! myy)) +
    geom_point() +
    facet_wrap(~metric) +
    labs(x=myxlab, y=myylab, title=mytitle) +
    theme_bw()
  return(myplot)
}

colorize_confetti <- function(myplot){
  print(myplot + geom_point(aes(color = retro_type)))
  print(myplot + geom_point(aes(color = IBMlab)))
  print(myplot + geom_point(aes(color = factor(Fhist))))
  print(myplot + geom_point(aes(color = factor(n_selblocks))))
  print(myplot + geom_point(aes(color = factor(catch.mult))))
  invisible(NULL)
}

# makes base, no retro, and SCAA plots assuming list of these 3 sent
make_box_plot <- function(mytib, myx, myy, myxlab, myylab, mytitle){
  myx <- enquo(myx)
  myy <- enquo(myy)
  mytitleext <- c("", "(No retro scenarios)", "(SCAA scenarios)")
  myplot <- list()
  for (i in 1:3){
    myplot[[i]] <- ggplot(mytib[[i]], aes(x = !! myx, y = !! myy)) +
      geom_boxplot() +
      facet_wrap(~metric) +
      labs(x=myxlab, y=myylab, title=paste(mytitle, mytitleext[i])) +
      theme_bw() +
      coord_flip() 
  }
  return(myplot)
}

### make named plots
# confetti plots
ssb_probs_plot <- confetti_plot(ssb_probs, iscen, value, "Scenario", "Probablity", "SSB")

ssb_ns_plot <- confetti_plot(ssb_ns, iscen, value, "Scenario", "Mean Number of Years", "SSB")

ssb_ratios_plot <- confetti_plot(ssb_ratios, iscen, value, "Scenario", "Mean SSB/SSBmsy", "SSB")

f_probs_plot <- confetti_plot(f_probs, iscen, value, "Scenario", "Probablity", "F")

f_ns_plot <- confetti_plot(f_ns, iscen, value, "Scenario", "Mean Number of Years", "F")

f_ratios_plot <- confetti_plot(f_ratios, iscen, value, "Scenario", "Mean F/Fmsy", "F")


catch_means_plot <- confetti_plot(catch_means, iscen, value, "Scenario", "Mean of Metric", "Catch")

catch_ratios_plot <- confetti_plot(catch_ratios, iscen, value, "Scenario", "Mean Catch/MSY", "Catch")

catch_other_plot <- confetti_plot(catch_other, iscen, value, "Scenario", "Mean of Metric", "Catch")

# box plots
ssb_box_probs_IBM <- make_box_plot(list(ssb_probs, ssb_probs_noretro, ssb_probs_scaa), IBMlab, value, "IBM", "Probability", "SSB")

ssb_box_probs_Scen <- make_box_plot(list(ssb_probs, ssb_probs_noretro, ssb_probs_scaa), Scenlab, value, "Scenario", "Probability", "SSB")

ssb_box_ns_IBM <- make_box_plot(list(ssb_ns, ssb_ns_noretro, ssb_ns_scaa), IBMlab, value, "IBM", "Mean Number of Years", "SSB")

ssb_box_ns_Scen <- make_box_plot(list(ssb_ns, ssb_ns_noretro, ssb_ns_scaa), Scenlab, value, "Scenario", "Mean Number of Years", "SSB")

ssb_box_ratios_IBM <- make_box_plot(list(ssb_ratios, ssb_ratios_noretro, ssb_ratios_scaa), IBMlab, value, "IBM", "Mean SSB/SSBmsy", "SSB")

ssb_box_ratios_Scen <- make_box_plot(list(ssb_ratios, ssb_ratios_noretro, ssb_ratios_scaa), Scenlab, value, "Scenario", "Mean SSB/SSBmsy", "SSB")

f_box_probs_IBM <- make_box_plot(list(f_probs, f_probs_noretro, f_probs_scaa), IBMlab, value, "IBM", "Probability", "F")

f_box_probs_Scen <- make_box_plot(list(f_probs, f_probs_noretro, f_probs_scaa), Scenlab, value, "Scenario", "Probability", "F")

f_box_ns_IBM <- make_box_plot(list(f_ns, f_ns_noretro, f_ns_scaa), IBMlab, value, "IBM", "Mean Number of Years", "F")

f_box_ns_Scen <- make_box_plot(list(f_ns, f_ns_noretro, f_ns_scaa), Scenlab, value, "Scenario", "Mean Number of Years", "F")

f_box_ratios_IBM <- make_box_plot(list(f_ratios, f_ratios_noretro, f_ratios_scaa), IBMlab, value, "IBM", "Mean F/Fmsy", "F")

f_box_ratios_Scen <- make_box_plot(list(f_ratios, f_ratios_noretro, f_ratios_scaa), Scenlab, value, "Scenario", "Mean F/Fmsy", "F")

catch_box_means_IBM <- make_box_plot(list(catch_means, catch_means_noretro, catch_means_scaa), IBMlab, value, "IBM", "Mean of Metric", "Catch")

catch_box_means_Scen <- make_box_plot(list(catch_means, catch_means_noretro, catch_means_scaa), Scenlab, value, "Scenario", "Mean of Metric", "Catch")

catch_box_ratios_IBM <- make_box_plot(list(catch_ratios, catch_ratios_noretro, catch_ratios_scaa), IBMlab, value, "IBM", "Mean Catch/MSY", "Catch")

catch_box_ratios_Scen <- make_box_plot(list(catch_ratios, catch_ratios_noretro, catch_ratios_scaa), Scenlab, value, "Scenario", "Mean Catch/MSY", "Catch")

catch_box_other_IBM <- make_box_plot(list(catch_other, catch_other_noretro, catch_other_scaa), IBMlab, value, "IBM", "Mean of Metric", "Catch")

catch_box_other_Scen <- make_box_plot(list(catch_other, catch_other_noretro, catch_other_scaa), Scenlab, value, "Scenario", "Mean of Metric", "Catch")

### examine what factors led to good and bad outcomes
which_rebuild <- rbind(ssb_probs, ssb_probs_scaa) %>%
  filter(metric == "l_is_ge_bmsy", value >= 0.9) 
which_rebuild$retro_type
which_rebuild$IBMlab
which_rebuild$Scenlab
unique(which_rebuild$IBMlab)
unique(which_rebuild$Scenlab)

which_crash <- rbind(ssb_probs, ssb_probs_scaa) %>%
  filter(metric == "l_is_less_01_bmsy", value >= 0.90) 
which_crash$retro_type
which_crash$IBMlab
which_crash$Scenlab
unique(which_crash$IBMlab)
unique(which_crash$Scenlab)

### trade-off plots

# makes base, no retro, and SCAA plots assuming list of these 3 sent
make_td_plot <- function(mytib, myxlab, myylab, mytitle){
  mytitleext <- c("", "(No retro scenarios)", "(SCAA scenarios)")
  myplot <- list()
  for (i in 1:3){
    myplot[[i]] <- ggplot(mytib[[i]], aes(x=x_value, y=y_value, color=retro_type)) +
    geom_point() +
    facet_wrap(~IBMlab) +
    labs(x=myxlab, y=myylab, title=paste(mytitle, mytitleext[i])) +
    theme_bw()
  }
  return(myplot)
}

names(ssb_mean_by_scenario)
names(f_mean_by_scenario)
names(catch_mean_by_scenario)
ssb_temp <- ssb_mean_by_scenario %>%
  mutate(metric = paste0("ssb_", metric))
f_temp <- f_mean_by_scenario %>%
  mutate(metric = paste0("f_", metric))
catch_temp <- catch_mean_by_scenario %>%
  mutate(metric = paste0("catch_", metric))
td <- rbind(ssb_temp, f_temp, catch_temp)

ssb_temp_noretro <- ssb_mean_by_scenario_noretro %>%
  mutate(metric = paste0("ssb_", metric))
f_temp_noretro <- f_mean_by_scenario_noretro %>%
  mutate(metric = paste0("f_", metric))
catch_temp_noretro <- catch_mean_by_scenario_noretro %>%
  mutate(metric = paste0("catch_", metric))
td_noretro <- rbind(ssb_temp_noretro, f_temp_noretro, catch_temp_noretro)

ssb_temp_scaa <- ssb_mean_by_scenario_scaa %>%
  mutate(metric = paste0("ssb_", metric))
f_temp_scaa <- f_mean_by_scenario_scaa %>%
  mutate(metric = paste0("f_", metric))
catch_temp_scaa <- catch_mean_by_scenario_scaa %>%
  mutate(metric = paste0("catch_", metric))
td_scaa <- rbind(ssb_temp_scaa, f_temp_scaa, catch_temp_scaa)

td1_l <- td %>%
  filter(metric %in% c("ssb_l_is_ge_bmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_is_ge_bmsy, y_value = catch_l_avg_catch_msy)

td1_l_noretro <- td_noretro %>%
  filter(metric %in% c("ssb_l_is_ge_bmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_is_ge_bmsy, y_value = catch_l_avg_catch_msy)

td1_l_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_l_is_ge_bmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_is_ge_bmsy, y_value = catch_l_avg_catch_msy)

td1_s <- td %>%
  filter(metric %in% c("ssb_s_is_ge_bmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_ge_bmsy, y_value = catch_s_avg_catch_msy)

td1_s_noretro <- td_noretro %>%
  filter(metric %in% c("ssb_s_is_ge_bmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_ge_bmsy, y_value = catch_s_avg_catch_msy)

td1_s_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_s_is_ge_bmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_ge_bmsy, y_value = catch_s_avg_catch_msy)

td1_l_plot <- make_td_plot(list(td1_l, td1_l_noretro, td1_l_scaa), "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Long Term")

td1_s_plot <- make_td_plot(list(td1_s, td1_s_noretro, td1_s_scaa), "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Short Term")

td2_l <- td %>%
  filter(metric %in% c("ssb_l_is_less_05_bmsy", "f_l_is_gr_fmsy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_is_less_05_bmsy, y_value = f_l_is_gr_fmsy)

td2_l_noretro <- td_noretro %>%
  filter(metric %in% c("ssb_l_is_less_05_bmsy", "f_l_is_gr_fmsy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_is_less_05_bmsy, y_value = f_l_is_gr_fmsy)

td2_l_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_l_is_less_05_bmsy", "f_l_is_gr_fmsy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_is_less_05_bmsy, y_value = f_l_is_gr_fmsy)

td2_s <- td %>%
  filter(metric %in% c("ssb_s_is_less_05_bmsy", "f_s_is_gr_fmsy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_less_05_bmsy, y_value = f_s_is_gr_fmsy)

td2_s_noretro <- td_noretro %>%
  filter(metric %in% c("ssb_s_is_less_05_bmsy", "f_s_is_gr_fmsy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_less_05_bmsy, y_value = f_s_is_gr_fmsy)

td2_s_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_s_is_less_05_bmsy", "f_s_is_gr_fmsy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_less_05_bmsy, y_value = f_s_is_gr_fmsy)

td2_l_plot <- make_td_plot(list(td2_l, td2_l_noretro, td2_l_scaa), "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Long Term")

td2_s_plot <- make_td_plot(list(td2_s, td2_s_noretro, td2_s_scaa), "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Short Term")

td3_l <- td %>%
  filter(metric %in% c("ssb_l_avg_ssb_ssbmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_avg_ssb_ssbmsy, y_value = catch_l_avg_catch_msy)

td3_l_noretro <- td_noretro %>%
  filter(metric %in% c("ssb_l_avg_ssb_ssbmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_avg_ssb_ssbmsy, y_value = catch_l_avg_catch_msy)

td3_l_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_l_avg_ssb_ssbmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_avg_ssb_ssbmsy, y_value = catch_l_avg_catch_msy)

td3_s <- td %>%
  filter(metric %in% c("ssb_s_avg_ssb_ssbmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_avg_ssb_ssbmsy, y_value = catch_s_avg_catch_msy)

td3_s_noretro <- td_noretro %>%
  filter(metric %in% c("ssb_s_avg_ssb_ssbmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_avg_ssb_ssbmsy, y_value = catch_s_avg_catch_msy)

td3_s_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_s_avg_ssb_ssbmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_avg_ssb_ssbmsy, y_value = catch_s_avg_catch_msy)

td3_l_plot <- make_td_plot(list(td3_l, td3_l_noretro, td3_l_scaa), "SSB/SSBmsy", "Catch/MSY", "Long Term")

td3_s_plot <- make_td_plot(list(td3_s, td3_s_noretro, td3_s_scaa), "SSB/SSBmsy", "Catch/MSY", "Short Term")

# tradeoffs showing individual simulations as points
# too confusing to make this one a 3 parter, so run base, noretro, and scc separately
make_td_sim_plot <- function(mytibble, myxlab, myylab, mytitle, myxmax, myymax, mycol){
    myplot <- ggplot(mytibble, aes(x=x_value, y=y_value)) +
    geom_point(color=mycol) +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    labs(x=myxlab, y=myylab, title=mytitle) +
    expand_limits(x=myxmax, y=myymax) +
    theme_bw()
  return(myplot)
}

ssb_sims <- ssb_results %>%
  filter(grepl("avg_ssb_ssbmsy", metric)) %>%
  inner_join(defined, by = c("iscen"))

f_sims <- f_results %>%
  filter(grepl("avg_f_fmsy", metric)) %>%
  inner_join(defined, by = c("iscen"))

catch_sims <- catch_results %>%
  filter(grepl("avg_catch_msy", metric)) %>%
  inner_join(defined, by = c("iscen"))

sims <- rbind(ssb_sims, f_sims, catch_sims) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value)

# have to do the scaa, noretro, and scen by parts then combine
ssb_sims_scaa <- ssb_results_scaa %>%
  filter(grepl("avg_ssb_ssbmsy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

f_sims_scaa <- f_results_scaa %>%
  filter(grepl("avg_f_fmsy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

catch_sims_scaa <- catch_results_scaa %>%
  filter(grepl("avg_catch_msy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

ssb_sims_noretro <- ssb_results_noretro %>%
  filter(grepl("avg_ssb_ssbmsy", metric)) %>%
  inner_join(defined_noretro, by = c("iscen"))

f_sims_noretro <- f_results_noretro %>%
  filter(grepl("avg_f_fmsy", metric)) %>%
  inner_join(defined_noretro, by = c("iscen"))

catch_sims_noretro <- catch_results_noretro %>%
  filter(grepl("avg_catch_msy", metric)) %>%
  inner_join(defined_noretro, by = c("iscen"))

ssb_sims_scen <- ssb_sims %>%
  filter(Scenlab %in% scaa_scen)

f_sims_scen <- f_sims %>%
  filter(Scenlab %in% scaa_scen)

catch_sims_scen <- catch_sims %>%
  filter(Scenlab %in% scaa_scen)

sims_noretro <- rbind(ssb_sims_noretro, f_sims_noretro, catch_sims_noretro,
                      filter(ssb_sims_scen, IBMlab != "DLM"),
                      filter(f_sims_scen, IBMlab != "DLM"),
                      filter(catch_sims_scen, IBMlab != "DLM")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value)

sims_scaa <- rbind(ssb_sims_scaa, f_sims_scaa, catch_sims_scaa,
                   ssb_sims_scen, f_sims_scen, catch_sims_scen) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value)

myscenlabs <- sort(unique(sims$Scenlab))
nscenlabs <- length(myscenlabs)
myibmlabs <- sort(unique(sims$IBMlab))
nibmlabs <- length(myibmlabs)

myscenlabs_noretro <- sort(unique(sims_noretro$Scenlab))
nscenlabs_noretro <- length(myscenlabs_noretro)
myibmlabs_noretro <- sort(unique(sims_noretro$IBMlab))
nibmlabs_noretro <- length(myibmlabs_noretro)

myscenlabs_scaa <- sort(unique(sims_scaa$Scenlab))
nscenlabs_scaa <- length(myscenlabs_scaa)
myibmlabs_scaa <- sort(unique(sims_scaa$IBMlab))
nibmlabs_scaa <- length(myibmlabs_scaa)

mysmax_l <- max(sims$l_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_l <- max(sims$l_avg_f_fmsy, na.rm = TRUE)
mycmax_l <- max(sims$l_avg_catch_msy, na.rm = TRUE)
mysmax_s <- max(sims$s_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_s <- max(sims$s_avg_f_fmsy, na.rm = TRUE)
mycmax_s <- max(sims$s_avg_catch_msy, na.rm = TRUE)

mysmax_l_noretro <- max(sims_noretro$l_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_l_noretro <- max(sims_noretro$l_avg_f_fmsy, na.rm = TRUE)
mycmax_l_noretro <- max(sims_noretro$l_avg_catch_msy, na.rm = TRUE)
mysmax_s_noretro <- max(sims_noretro$s_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_s_noretro <- max(sims_noretro$s_avg_f_fmsy, na.rm = TRUE)
mycmax_s_noretro <- max(sims_noretro$s_avg_catch_msy, na.rm = TRUE)

mysmax_l_scaa <- max(sims_scaa$l_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_l_scaa <- max(sims_scaa$l_avg_f_fmsy, na.rm = TRUE)
mycmax_l_scaa <- max(sims_scaa$l_avg_catch_msy, na.rm = TRUE)
mysmax_s_scaa <- max(sims_scaa$s_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_s_scaa <- max(sims_scaa$s_avg_f_fmsy, na.rm = TRUE)
mycmax_s_scaa <- max(sims_scaa$s_avg_catch_msy, na.rm = TRUE)

td4_l_IBM_plot <- list()
for (i in 1:nscenlabs){
  tmpdf <- filter(sims, Scenlab == myscenlabs[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myscenlabs[i], "Long Term")
  td4_l_IBM_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_l, mycmax_l, "black") +
    facet_wrap(~IBMlab)
}

td4_s_IBM_plot <- list()
for (i in 1:nscenlabs){
  tmpdf <- filter(sims, Scenlab == myscenlabs[i]) %>%
    mutate(x_value=s_avg_ssb_ssbmsy, y_value=s_avg_catch_msy)
  mytitle <- paste(myscenlabs[i], "Short Term")
  td4_s_IBM_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_s, mycmax_s, "black") +
    facet_wrap(~IBMlab)
}

td4_l_Scen_plot <- list()
for (i in 1:length(myibmlabs)){
  tmpdf <- filter(sims, IBMlab == myibmlabs[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myibmlabs[i], "Long Term")
  td4_l_Scen_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_l, mycmax_l, "blue") +
    facet_wrap(~Scenlab)
}

td4_s_Scen_plot <- list()
for (i in 1:length(myibmlabs)){
  tmpdf <- filter(sims, IBMlab == myibmlabs[i]) %>%
    mutate(x_value=s_avg_ssb_ssbmsy, y_value=s_avg_catch_msy)
  mytitle <- paste(myibmlabs[i], "Short Term")
  td4_s_Scen_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_s, mycmax_s, "blue") +
    facet_wrap(~Scenlab)
}

td4_l_IBM_noretro_plot <- list()
for (i in 1:nscenlabs_noretro){
  tmpdf <- filter(sims_noretro, Scenlab == myscenlabs_noretro[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myscenlabs_noretro[i], "Long Term (No retro scenarios)")
  td4_l_IBM_noretro_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_l_noretro, mycmax_l_noretro, "black") +
    facet_wrap(~IBMlab)
}

td4_s_IBM_noretro_plot <- list()
for (i in 1:nscenlabs_noretro){
  tmpdf <- filter(sims_noretro, Scenlab == myscenlabs_noretro[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myscenlabs_noretro[i], "Short Term (No retro scenarios)")
  td4_s_IBM_noretro_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_s_noretro, mycmax_s_noretro, "black") +
    facet_wrap(~IBMlab)
}

td4_l_Scen_noretro_plot <- list()
for (i in 1:length(myibmlabs_noretro)){
  tmpdf <- filter(sims_noretro, IBMlab == myibmlabs_noretro[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myibmlabs_noretro[i], "Long Term (No retro scenarios)")
  td4_l_Scen_noretro_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_l_noretro, mycmax_l_noretro, "blue") +
    facet_wrap(~Scenlab)
}

td4_s_Scen_noretro_plot <- list()
for (i in 1:length(myibmlabs_noretro)){
  tmpdf <- filter(sims_noretro, IBMlab == myibmlabs_noretro[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myibmlabs_noretro[i], "Short Term (No retro scenarios)")
  td4_s_Scen_noretro_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_s_noretro, mycmax_s_noretro, "blue") +
    facet_wrap(~Scenlab)
}

td4_l_IBM_scaa_plot <- list()
for (i in 1:nscenlabs_scaa){
  tmpdf <- filter(sims_scaa, Scenlab == myscenlabs_scaa[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myscenlabs_scaa[i], "Long Term (SCAA scenarios)")
  td4_l_IBM_scaa_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_l_scaa, mycmax_l_scaa, "black") +
    facet_wrap(~IBMlab)
}

td4_s_IBM_scaa_plot <- list()
for (i in 1:nscenlabs_scaa){
  tmpdf <- filter(sims_scaa, Scenlab == myscenlabs_scaa[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myscenlabs_scaa[i], "Short Term (SCAA scenarios)")
  td4_s_IBM_scaa_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_s_scaa, mycmax_s_scaa, "black") +
    facet_wrap(~IBMlab)
}

td4_l_Scen_scaa_plot <- list()
for (i in 1:length(myibmlabs_scaa)){
  tmpdf <- filter(sims_scaa, IBMlab == myibmlabs_scaa[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myibmlabs_scaa[i], "Long Term (SCAA scenarios)")
  td4_l_Scen_scaa_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_l_scaa, mycmax_l_scaa, "blue") +
    facet_wrap(~Scenlab)
}

td4_s_Scen_scaa_plot <- list()
for (i in 1:length(myibmlabs_scaa)){
  tmpdf <- filter(sims_scaa, IBMlab == myibmlabs_scaa[i]) %>%
    mutate(x_value=l_avg_ssb_ssbmsy, y_value=l_avg_catch_msy)
  mytitle <- paste(myibmlabs_scaa[i], "Short Term (SCAA scenarios)")
  td4_s_Scen_scaa_plot[[i]] <- make_td_sim_plot(tmpdf, "SSB/SSBmsy", "Catch/MSY", mytitle, mysmax_s_scaa, mycmax_s_scaa, "blue") +
    facet_wrap(~Scenlab)
}

# compare metrics plot
# bb controls whether largest mean value at top (TRUE) or smallest (FALSE)
# makes 3 plots for base, noretro, and scaa scenarios (send all 3 in list)
compare_all_plot <- function(mytib, myxlab, mytitle, bb){
  mytitleext <- c("", "(No retro scenarios)", "(SCAA scenarios)")
  myplot <- list()
  for (i in 1:3){
    mysum <- mytib[[i]] %>%
      group_by(IBMlab) %>%
      summarize(meanval = mean(value)) %>%
      inner_join(mytib[[i]], by = "IBMlab")
    if (bb == FALSE){ 
      mysum$meanval = -1.0 * mysum$meanval
    }
    myplot[[i]] <- ggplot(mysum, aes(x=value, y=reorder(IBMlab, meanval), color=factor(catch.mult))) +
      geom_point() +
      facet_grid(retro_type~Fhistlab[Fhist]+n_selblocks) +
      labs(x=myxlab, y="", color="Catch Mult", 
           title=paste(mytitle, mytitleext[i])) +
      theme_bw()
  }
  return(myplot)
}

ssb_l_3 <- list(filter(ssb_mean_by_scenario, 
                       metric == "l_avg_ssb_ssbmsy"),
                filter(ssb_mean_by_scenario_noretro, 
                       metric == "l_avg_ssb_ssbmsy"),
                filter(ssb_mean_by_scenario_scaa, 
                       metric == "l_avg_ssb_ssbmsy"))

ssb_ssbmsy_l <- compare_all_plot(ssb_l_3, "SSB/SSBmsy", "Long Term", TRUE)

f_l_3 <- list(filter(f_mean_by_scenario, 
                     metric == "l_avg_f_fmsy"),
              filter(f_mean_by_scenario_noretro, 
                     metric == "l_avg_f_fmsy"),
              filter(f_mean_by_scenario_scaa, 
                     metric == "l_avg_f_fmsy"))

f_fmsy_l <- compare_all_plot(f_l_3, "F/Fmsy", "Long Term", FALSE)

c_l_3 <- list(filter(catch_mean_by_scenario, 
                     metric == "l_avg_catch_msy"),
              filter(catch_mean_by_scenario_noretro, 
                     metric == "l_avg_catch_msy"),
              filter(catch_mean_by_scenario_scaa, 
                     metric == "l_avg_catch_msy"))

catch_msy_l <- compare_all_plot(c_l_3, "Catch/MSY", "Long Term", TRUE)

ssb_s_3 <- list(filter(ssb_mean_by_scenario, 
                       metric == "s_avg_ssb_ssbmsy"),
                filter(ssb_mean_by_scenario_noretro, 
                       metric == "s_avg_ssb_ssbmsy"),
                filter(ssb_mean_by_scenario_scaa, 
                       metric == "s_avg_ssb_ssbmsy"))

ssb_ssbmsy_s <- compare_all_plot(ssb_s_3, "SSB/SSBmsy", "Short Term", TRUE)

f_s_3 <- list(filter(f_mean_by_scenario, 
                     metric == "s_avg_f_fmsy"),
              filter(f_mean_by_scenario_noretro, 
                     metric == "s_avg_f_fmsy"),
              filter(f_mean_by_scenario_scaa, 
                     metric == "s_avg_f_fmsy"))

f_fmsy_s <- compare_all_plot(f_s_3, "F/Fmsy", "Short Term", FALSE)

c_s_3 <- list(filter(catch_mean_by_scenario, 
                     metric == "s_avg_catch_msy"),
              filter(catch_mean_by_scenario_noretro, 
                     metric == "s_avg_catch_msy"),
              filter(catch_mean_by_scenario_scaa, 
                     metric == "s_avg_catch_msy"))

catch_msy_s <- compare_all_plot(c_s_3, "Catch/MSY", "Short Term", TRUE)

# probabilty and nyears overfished overfishing 
get_status_data <- function(myssbtib, myftib){
  
  prob_overfished <- filter(myssbtib,
                     metric %in% c("l_is_less_05_bmsy", 
                                   "s_is_less_05_bmsy")) %>%
    mutate(period = ifelse(substr(metric, 1, 1) == "l", "Long Term", "Short Term")) %>%
    group_by(IBMlab, period) %>%
    summarise(meanval = mean(value)) %>%
    mutate(sdc = "Overfished")

  prob_overfishing <- filter(myftib,
                             metric %in% c("l_is_gr_fmsy", "s_is_gr_fmsy")) %>%
    mutate(period = ifelse(substr(metric, 1, 1) == "l", "Long Term", "Short Term")) %>%
    group_by(IBMlab, period) %>%
    summarise(meanval = mean(value)) %>%
    mutate(sdc = "Overfishing")

  prob_status <- rbind(prob_overfished, prob_overfishing)

  nyrs_overfished <- filter(myssbtib,
                            metric %in% c("l_n_less_05_bmsy", 
                                          "s_n_less_05_bmsy")) %>%
    mutate(period = ifelse(substr(metric, 1, 1) == "l", "Long Term", "Short Term")) %>%
    group_by(IBMlab, period) %>%
    summarise(meanval = mean(value)) %>%
    mutate(sdc = "Overfished")
  
  nyrs_overfishing <- filter(myftib,
                             metric %in% c("l_n_gr_fmsy", "s_n_gr_fmsy")) %>%
    mutate(period = ifelse(substr(metric, 1, 1) == "l", "Long Term", "Short Term")) %>%
    group_by(IBMlab, period) %>%
    summarise(meanval = mean(value)) %>%
    mutate(sdc = "Overfishing")
  
  nyrs_status <- rbind(nyrs_overfished, nyrs_overfishing)

  mystatus <- list(prob_status = prob_status,
                   nyrs_status = nyrs_status)

  return(mystatus)
}

make_status_plot <- function(mytib, myxlab, mygridscales){
  mytitle <- c("Base scenarios", "No retro scenarios", "SCAA scenarios")
  myplot <- list()
  for (i in 1:3){
    myplot[[i]] <- ggplot(mytib[[i]], aes(x=meanval, y=IBMlab)) +
      geom_point() +
      facet_grid(sdc~period, scales = mygridscales) +
      expand_limits(x=c(0, 1)) +
      labs(x=myxlab, y="", title=mytitle[i]) +
      theme_bw()
  }
  return(myplot)
}

mystatus <- get_status_data(ssb_mean_by_scenario, f_mean_by_scenario)

mystatus_noretro <- get_status_data(ssb_mean_by_scenario_noretro,
                                    f_mean_by_scenario_noretro)

mystatus_scaa <- get_status_data(ssb_mean_by_scenario_scaa,
                                 f_mean_by_scenario_scaa)

prob_status_plot <- make_status_plot(list(mystatus[[1]], 
                                          mystatus_noretro[[1]],
                                          mystatus_scaa[[1]]), 
                                     "Probability", "fixed")
nyrs_status_plot <- make_status_plot(list(mystatus[[2]], 
                                          mystatus_noretro[[2]],
                                          mystatus_scaa[[2]]), 
                                     "Number of Years", "free_x")


# compute number of sims in each quadrant and odds ratio
# A | B
# C | D
# r = AD / BC

quad_sims <- sims %>%
  mutate(s_quad = case_when(
    (s_avg_f_fmsy > 1 & s_avg_ssb_ssbmsy < 1) ~ "A",
    (s_avg_f_fmsy > 1 & s_avg_ssb_ssbmsy >= 1) ~ "B",
    (s_avg_f_fmsy <= 1 & s_avg_ssb_ssbmsy < 1) ~ "C",
    (s_avg_f_fmsy <= 1 & s_avg_ssb_ssbmsy >= 1) ~ "D",
    TRUE ~ "error"),
    l_quad = case_when(
      (l_avg_f_fmsy > 1 & l_avg_ssb_ssbmsy < 1) ~ "A",
      (l_avg_f_fmsy > 1 & l_avg_ssb_ssbmsy >= 1) ~ "B",
      (l_avg_f_fmsy <= 1 & l_avg_ssb_ssbmsy < 1) ~ "C",
      (l_avg_f_fmsy <= 1 & l_avg_ssb_ssbmsy >= 1) ~ "D",
      TRUE ~ "error"))
unique(quad_sims$s_quad)
unique(quad_sims$l_quad)

quad_res_s <- quad_sims %>%
  group_by(IBMlab, Scenlab, s_quad) %>%
  summarise(n = n())

quad_res_l <- quad_sims %>%
  group_by(IBMlab, Scenlab, l_quad) %>%
  summarise(n = n())

odds_ratio_s <- quad_res_s %>%
  pivot_wider(names_from = "s_quad", values_from = "n", values_fill = 0) %>%
  mutate(r = A * D / (B * C))

odds_ratio_l <- quad_res_l %>%
  pivot_wider(names_from = "l_quad", values_from = "n", values_fill = 0) %>%
  mutate(r = A * D / (B * C))

odds_s_plot <- ggplot(odds_ratio_s, aes(x = r, y = Scenlab)) +
  geom_point() +
  facet_wrap(~IBMlab, nrow = 3) +
  labs(x="Odds Ratio", y="", title = "Short Term") +
  xlim(0, 200) +
  theme_bw()
ggsave(filename = "tables_figs/odd_ratio_s.png", width = 6.5, height = 6.5, units = "in", odds_s_plot)

odds_l_plot <- ggplot(odds_ratio_l, aes(x = r, y = Scenlab)) +
  geom_point() +
  facet_wrap(~IBMlab, nrow = 3) +
  labs(x="Odds Ratio", y="", title = "Long Term") +
  xlim(0, 200) +
  theme_bw()
ggsave(filename = "tables_figs/odd_ratio_l.png", width = 6.5, height = 6.5, units = "in", odds_l_plot)

### put plots into pdfs

pdf(file = "tables_figs/confetti_plots.pdf")
print(ssb_probs_plot)
colorize_confetti(ssb_probs_plot)
print(ssb_ns_plot)
colorize_confetti(ssb_ns_plot)
print(ssb_ratios_plot)
colorize_confetti(ssb_ratios_plot)

print(f_probs_plot)
colorize_confetti(f_probs_plot)
print(f_ns_plot)
colorize_confetti(f_ns_plot)
print(f_ratios_plot)
colorize_confetti(f_ratios_plot)

print(catch_means_plot)
colorize_confetti(catch_means_plot)
print(catch_ratios_plot)
colorize_confetti(catch_ratios_plot)
print(catch_other_plot)
colorize_confetti(catch_other_plot)

dev.off()

outfile <- c("tables_figs/tables_figures.pdf",
             "tables_figs/tables_figures_noretro.pdf",
             "tables_figs/tables_figures_scaa.pdf")

for (i in 1:3){
  
  pdf(file = outfile[i])
  
  print(nsim_plot)
  print(scenlab_plot)
  
  print(ssb_box_probs_IBM[[i]]) 
  print(ssb_box_probs_Scen[[i]])
  print(ssb_box_ns_IBM[[i]])
  print(ssb_box_ns_Scen[[i]])
  print(ssb_box_ratios_IBM[[i]]) 
  print(ssb_box_ratios_Scen[[i]])
  
  print(f_box_probs_IBM[[i]]) 
  print(f_box_probs_Scen[[i]])
  print(f_box_ns_IBM[[i]])
  print(f_box_ns_Scen[[i]])
  print(f_box_ratios_IBM[[i]]) 
  print(f_box_ratios_Scen[[i]])
  
  print(catch_box_means_IBM[[i]])
  print(catch_box_means_Scen[[i]])
  print(catch_box_ratios_IBM[[i]]) 
  print(catch_box_ratios_Scen[[i]])
  print(catch_box_other_IBM[[i]]) 
  print(catch_box_other_Scen[[i]])
  
  print(td1_l_plot[[i]])
  print(td1_s_plot[[i]])
  print(td2_l_plot[[i]])
  print(td2_s_plot[[i]])
  print(td3_l_plot[[i]])
  print(td3_s_plot[[i]])
  
  if (i == 1){
    walk(td4_l_IBM_plot, print)
    walk(td4_s_IBM_plot, print)
    walk(td4_l_Scen_plot, print)
    walk(td4_s_Scen_plot, print)
  }
  
  if (i == 2){
    walk(td4_l_IBM_noretro_plot, print)
    walk(td4_s_IBM_noretro_plot, print)
    walk(td4_l_Scen_noretro_plot, print)
    walk(td4_s_Scen_noretro_plot, print)
  }
  
  if (i == 3){
    walk(td4_l_IBM_scaa_plot, print)
    walk(td4_s_IBM_scaa_plot, print)
    walk(td4_l_Scen_scaa_plot, print)
    walk(td4_s_Scen_scaa_plot, print)
  }
  
  print(ssb_ssbmsy_l[[i]])
  print(f_fmsy_l[[i]]) 
  print(catch_msy_l[[i]])
  print(ssb_ssbmsy_s[[i]])
  print(f_fmsy_s[[i]]) 
  print(catch_msy_s[[i]]) 
  
  print(prob_status_plot[[i]])
  print(nyrs_status_plot[[i]])
  
  dev.off()
}
