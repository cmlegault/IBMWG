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

defined_scaa <- defined %>%
  filter(IBMlab == "Ensemble",
         n_selblocks == 1,
         catch.mult == 1) %>%
  mutate(IBM = "SCAA",
         IBMlab = "SCAA")

defined_noretro <- defined %>%
  filter(IBMlab != "DLM",
         retro_type == "Catch",
         n_selblocks == 1,
         catch.mult == 1) %>%
  mutate(retro_type = "None",
         Scenlab = paste0("N", Fhistlab[Fhist], "1A"))

# counting scenarios and simulations
countIBM <- defined %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

countIBM_scaa <- defined_scaa %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

countIBM_noretro <- defined_noretro %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

nscentab <- rbind(countIBM, countIBM_scaa, countIBM_noretro) %>%
  select(IBMlab, Scenlab, nscenarios) %>%
  pivot_wider(names_from = Scenlab, values_from = nscenarios)
nscentab

nsimtab <- rbind(countIBM, countIBM_scaa, countIBM_noretro) %>%
  select(IBMlab, Scenlab, nsim) %>%
  pivot_wider(names_from = Scenlab, values_from = nsim)
nsimtab
write.csv(nsimtab, file = "tables_figs/nsimtab.csv", row.names = FALSE)

nsim_plot <- ggplot(rbind(countIBM, countIBM_scaa, countIBM_noretro), 
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

# get main results corresponding to scenarios done for SCAA and noretro
scaa_scen <- unique(ssb_mean_by_scenario_scaa$Scenlab)
noretro_scen <- unique(ssb_mean_by_scenario_noretro$Scenlab)

ssb_mean_by_scenario_scen <- ssb_mean_by_scenario %>%
  filter(Scenlab %in% scaa_scen)

f_mean_by_scenario_scen <- f_mean_by_scenario %>%
  filter(Scenlab %in% scaa_scen)

catch_mean_by_scenario_scen <- catch_mean_by_scenario %>%
  filter(Scenlab %in% scaa_scen)

# combine scaa, noretro, and main limited to scaa scenarios into scaa
ssb_mean_by_scenario_scaa <- rbind(ssb_mean_by_scenario_scaa,
                                   ssb_mean_by_scenario_noretro,
                                   ssb_mean_by_scenario_scen)

f_mean_by_scenario_scaa <- rbind(f_mean_by_scenario_scaa,
                                 f_mean_by_scenario_noretro,
                                 f_mean_by_scenario_scen)

catch_mean_by_scenario_scaa <- rbind(catch_mean_by_scenario_scaa,
                                     catch_mean_by_scenario_noretro,
                                     catch_mean_by_scenario_scen)

### save mean_by_scenario results for easier modeling and ranking
saveRDS(ssb_mean_by_scenario, 
        file = "tables_figs/ssb_mean_by_scenario.rds")

saveRDS(f_mean_by_scenario,
        file = "tables_figs/f_mean_by_scenario.rds")

saveRDS(catch_mean_by_scenario, 
        file = "tables_figs/catch_mean_by_scenario.rds")

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

make_box_plot <- function(mytib, myx, myy, myxlab, myylab, mytitle){
  myx <- enquo(myx)
  myy <- enquo(myy)
  myplot <- ggplot(mytib, aes(x = !! myx, y = !! myy)) +
    geom_boxplot() +
    facet_wrap(~metric) +
    labs(x=myxlab, y=myylab, title=mytitle) +
    theme_bw() +
    coord_flip() 
  return(myplot)
}

### make named plots
ssb_probs_plot <- confetti_plot(ssb_probs, iscen, value, "Scenario", "Probablity", "SSB")

ssb_ns_plot <- confetti_plot(ssb_ns, iscen, value, "Scenario", "Mean Number of Years", "SSB")

ssb_ratios_plot <- confetti_plot(ssb_ratios, iscen, value, "Scenario", "Mean SSB/SSBmsy", "SSB")

ssb_box_probs_IBM <- make_box_plot(ssb_probs, IBMlab, value, "IBM", "Probability", "SSB")

ssb_box_probs_Scen <- make_box_plot(ssb_probs, Scenlab, value, "Scenario", "Probability", "SSB")

ssb_box_ns_IBM <- make_box_plot(ssb_ns, IBMlab, value, "IBM", "Mean Number of Years", "SSB")

ssb_box_ns_Scen <- make_box_plot(ssb_ns, Scenlab, value, "Scenario", "Mean Number of Years", "SSB")

ssb_box_ratios_IBM <- make_box_plot(ssb_ratios, IBMlab, value, "IBM", "Mean SSB/SSBmsy", "SSB")

ssb_box_ratios_Scen <- make_box_plot(ssb_ratios, Scenlab, value, "Scenario", "Mean SSB/SSBmsy", "SSB")

ssb_box_probs_IBM_scaa <- make_box_plot(ssb_probs_scaa, IBMlab, value, "IBM", "Probability", "SSB (SCAA scenarios)")

ssb_box_probs_Scen_scaa <- make_box_plot(ssb_probs_scaa, Scenlab, value, "Scenario", "Probability", "SSB (SCAA scenarios)")

ssb_box_ns_IBM_scaa <- make_box_plot(ssb_ns_scaa, IBMlab, value, "IBM", "Mean Number of Years", "SSB (SCAA scenarios)")

ssb_box_ns_Scen_scaa <- make_box_plot(ssb_ns_scaa, Scenlab, value, "Scenario", "Mean Number of Years", "SSB (SCAA scenarios)")

ssb_box_ratios_IBM_scaa <- make_box_plot(ssb_ratios_scaa, IBMlab, value, "IBM", "Mean SSB/SSBmsy", "SSB (SCAA scenarios)")

ssb_box_ratios_Scen_scaa <- make_box_plot(ssb_ratios_scaa, Scenlab, value, "Scenario", "Mean SSB/SSBmsy", "SSB (SCAA scenarios)")

f_probs_plot <- confetti_plot(f_probs, iscen, value, "Scenario", "Probablity", "F")

f_ns_plot <- confetti_plot(f_ns, iscen, value, "Scenario", "Mean Number of Years", "F")

f_ratios_plot <- confetti_plot(f_ratios, iscen, value, "Scenario", "Mean F/Fmsy", "F")

f_box_probs_IBM <- make_box_plot(f_probs, IBMlab, value, "IBM", "Probability", "F")

f_box_probs_Scen <- make_box_plot(f_probs, Scenlab, value, "Scenario", "Probability", "F")

f_box_ns_IBM <- make_box_plot(f_ns, IBMlab, value, "IBM", "Mean Number of Years", "F")

f_box_ns_Scen <- make_box_plot(f_ns, Scenlab, value, "Scenario", "Mean Number of Years", "F")

f_box_ratios_IBM <- make_box_plot(f_ratios, IBMlab, value, "IBM", "Mean F/Fmsy", "F")

f_box_ratios_Scen <- make_box_plot(f_ratios, Scenlab, value, "Scenario", "Mean F/Fmsy", "F")

f_box_probs_IBM_scaa <- make_box_plot(f_probs_scaa, IBMlab, value, "IBM", "Probability", "F (SCAA scenarios)")

f_box_probs_Scen_scaa <- make_box_plot(f_probs_scaa, Scenlab, value, "Scenario", "Probability", "F (SCAA scenarios)")

f_box_ns_IBM_scaa <- make_box_plot(f_ns_scaa, IBMlab, value, "IBM", "Mean Number of Years", "F (SCAA scenarios)")

f_box_ns_Scen_scaa <- make_box_plot(f_ns_scaa, Scenlab, value, "Scenario", "Mean Number of Years", "F (SCAA scenarios)")

f_box_ratios_IBM_scaa <- make_box_plot(f_ratios_scaa, IBMlab, value, "IBM", "Mean F/Fmsy", "F (SCAA scenarios)")

f_box_ratios_Scen_scaa <- make_box_plot(f_ratios_scaa, Scenlab, value, "Scenario", "Mean F/Fmsy", "F (SCAA scenarios)")

catch_means_plot <- confetti_plot(catch_means, iscen, value, "Scenario", "Mean of Metric", "Catch")

catch_ratios_plot <- confetti_plot(catch_ratios, iscen, value, "Scenario", "Mean Catch/MSY", "Catch")

catch_other_plot <- confetti_plot(catch_other, iscen, value, "Scenario", "Mean of Metric", "Catch")

catch_box_means_IBM <- make_box_plot(catch_means, IBMlab, value, "IBM", "Mean of Metric", "Catch")

catch_box_means_Scen <- make_box_plot(catch_means, Scenlab, value, "Scenario", "Mean of Metric", "Catch")

catch_box_ratios_IBM <- make_box_plot(catch_ratios, IBMlab, value, "IBM", "Mean Catch/MSY", "Catch")

catch_box_ratios_Scen <- make_box_plot(catch_ratios, Scenlab, value, "Scenario", "Mean Catch/MSY", "Catch")

catch_box_other_IBM <- make_box_plot(catch_other, IBMlab, value, "IBM", "Mean of Metric", "Catch")

catch_box_other_Scen <- make_box_plot(catch_other, Scenlab, value, "Scenario", "Mean of Metric", "Catch")

catch_box_means_IBM_scaa <- make_box_plot(catch_means_scaa, IBMlab, value, "IBM", "Mean of Metric", "Catch (SCAA scenarios)")

catch_box_means_Scen_scaa <- make_box_plot(catch_means_scaa, Scenlab, value, "Scenario", "Mean of Metric", "Catch (SCAA scenarios)")

catch_box_ratios_IBM_scaa <- make_box_plot(catch_ratios_scaa, IBMlab, value, "IBM", "Mean Catch/MSY", "Catch (SCAA scenarios)")

catch_box_ratios_Scen_scaa <- make_box_plot(catch_ratios_scaa, Scenlab, value, "Scenario", "Mean Catch/MSY", "Catch (SCAA scenarios)")

catch_box_other_IBM_scaa <- make_box_plot(catch_other_scaa, IBMlab, value, "IBM", "Mean of Metric", "Catch (SCAA scenarios)")

catch_box_other_Scen_scaa <- make_box_plot(catch_other_scaa, Scenlab, value, "Scenario", "Mean of Metric", "Catch (SCAA scenarios)")

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

make_td_plot <- function(mytibble, myxlab, myylab, mytitle){
  myplot <- ggplot(mytibble, aes(x=x_value, y=y_value, color=retro_type)) +
    geom_point() +
    facet_wrap(~IBMlab) +
    labs(x=myxlab, y=myylab, title=mytitle) +
    theme_bw()
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

td1_s_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_s_is_ge_bmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_ge_bmsy, y_value = catch_s_avg_catch_msy)

td1_l_plot <- make_td_plot(td1_l, "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Long Term")

td1_l_scaa_plot <- make_td_plot(td1_l_scaa, "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Long Term (SCAA scenarios")

td1_s_plot <- make_td_plot(td1_s, "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Short Term")

td1_s_scaa_plot <- make_td_plot(td1_s_scaa, "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Short Term (SCAA scenarios")

td2_l <- td %>%
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

td2_s_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_s_is_less_05_bmsy", "f_s_is_gr_fmsy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_is_less_05_bmsy, y_value = f_s_is_gr_fmsy)

td2_l_plot <- make_td_plot(td2_l, "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Long Term")

td2_l_scaa_plot <- make_td_plot(td2_l_scaa, "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Long Term (SCAA scenarios)")

td2_s_plot <- make_td_plot(td2_s, "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Short Term")

td2_s_scaa_plot <- make_td_plot(td2_s_scaa, "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Short Term (SCAA scenarios)")

td3_l <- td %>%
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

td3_s_scaa <- td_scaa %>%
  filter(metric %in% c("ssb_s_avg_ssb_ssbmsy", "catch_s_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_s_avg_ssb_ssbmsy, y_value = catch_s_avg_catch_msy)

td3_l_plot <- make_td_plot(td3_l, "SSB/SSBmsy", "Catch/MSY", "Long Term")

td3_l_scaa_plot <- make_td_plot(td3_l_scaa, "SSB/SSBmsy", "Catch/MSY", "Long Term (SCAA scenarios)")

td3_s_plot <- make_td_plot(td3_s, "SSB/SSBmsy", "Catch/MSY", "Short Term")

td3_s_scaa_plot <- make_td_plot(td3_s_scaa, "SSB/SSBmsy", "Catch/MSY", "Short Term (SCAA scenarios)")

# tradeoffs showing individual simulations as points
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

# have to so the scaa, noretro, and scen by parts then combine
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

sims_scaa <- rbind(ssb_sims_scaa, f_sims_scaa, catch_sims_scaa,
                   ssb_sims_noretro, f_sims_noretro, catch_sims_noretro,
                   ssb_sims_scen, f_sims_scen, catch_sims_scen) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value)

myscenlabs <- sort(unique(sims$Scenlab))
nscenlabs <- length(myscenlabs)
myibmlabs <- sort(unique(sims$IBMlab))
nibmlabs <- length(myibmlabs)

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
compare_all_plot <- function(mytibble, myxlab, mytitle){
  mysum <- mytibble %>%
    group_by(IBMlab) %>%
    summarize(meanval = mean(value)) %>%
    inner_join(mytibble, by = "IBMlab")
  myplot <- ggplot(mysum, aes(x=value, y=reorder(IBMlab, meanval), color=factor(catch.mult))) +
    geom_point() +
    facet_grid(retro_type~Fhistlab[Fhist]+n_selblocks) +
    labs(x=myxlab, y="", color="Catch Mult", title=mytitle) +
    theme_bw()
  return(myplot)
}

ssb_ssbmsy_l <- compare_all_plot(filter(ssb_mean_by_scenario, 
                                        metric == "l_avg_ssb_ssbmsy"), 
                                 "SSB/SSBmsy", "Long Term")

f_fmsy_l <- compare_all_plot(filter(f_mean_by_scenario, 
                                    metric == "l_avg_f_fmsy"), 
                             "F/Fmsy", "Long Term")

catch_msy_l <- compare_all_plot(filter(catch_mean_by_scenario, 
                                    metric == "l_avg_catch_msy"), 
                             "Catch/MSY", "Long Term")

ssb_ssbmsy_s <- compare_all_plot(filter(ssb_mean_by_scenario, 
                                        metric == "s_avg_ssb_ssbmsy"), 
                                 "SSB/SSBmsy", "Short Term")

f_fmsy_s <- compare_all_plot(filter(f_mean_by_scenario, 
                                    metric == "s_avg_f_fmsy"), 
                             "F/Fmsy", "Short Term")

catch_msy_s <- compare_all_plot(filter(catch_mean_by_scenario, 
                                       metric == "s_avg_catch_msy"), 
                                "Catch/MSY", "Short Term")

### put plots into pdf
pdf(file = "tables_figs/tables_figures.pdf")
nsim_plot
scenlab_plot

ssb_box_probs_IBM 
ssb_box_probs_Scen
ssb_box_ns_IBM
ssb_box_ns_Scen
ssb_box_ratios_IBM 
ssb_box_ratios_Scen
ssb_box_probs_IBM_scaa 
ssb_box_probs_Scen_scaa
ssb_box_ns_IBM_scaa
ssb_box_ns_Scen_scaa
ssb_box_ratios_IBM_scaa 
ssb_box_ratios_Scen_scaa
# ssb_probs_plot 
# colorize_confetti(ssb_probs_plot)
# ssb_ns_plot 
# colorize_confetti(ssb_ns_plot)
# ssb_ratios_plot 
# colorize_confetti(ssb_ratios_plot)

f_box_probs_IBM 
f_box_probs_Scen
f_box_ns_IBM
f_box_ns_Scen
f_box_ratios_IBM 
f_box_ratios_Scen
f_box_probs_IBM_scaa 
f_box_probs_Scen_scaa
f_box_ns_IBM_scaa
f_box_ns_Scen_scaa
f_box_ratios_IBM_scaa 
f_box_ratios_Scen_scaa
# f_probs_plot 
# colorize_confetti(f_probs_plot)
# f_ns_plot 
# colorize_confetti(f_ns_plot)
# f_ratios_plot 
# colorize_confetti(f_ratios_plot)

catch_box_means_IBM
catch_box_means_Scen
catch_box_ratios_IBM 
catch_box_ratios_Scen
catch_box_other_IBM 
catch_box_other_Scen
catch_box_means_IBM_scaa
catch_box_means_Scen_scaa
catch_box_ratios_IBM_scaa 
catch_box_ratios_Scen_scaa
catch_box_other_IBM_scaa 
catch_box_other_Scen_scaa
# catch_means_plot 
# colorize_confetti(catch_means_plot)
# catch_ratios_plot 
# colorize_confetti(catch_ratios_plot)
# catch_other_plot 
# colorize_confetti(catch_other_plot)

td1_l_plot
td1_s_plot
td1_l_scaa_plot
td1_s_scaa_plot

td2_l_plot
td2_s_plot
td2_l_scaa_plot
td2_s_scaa_plot

td3_l_plot
td3_s_plot
td3_l_scaa_plot
td3_s_scaa_plot

walk(td4_l_IBM_plot, print)
walk(td4_s_IBM_plot, print)
walk(td4_l_Scen_plot, print)
walk(td4_s_Scen_plot, print)

walk(td4_l_IBM_scaa_plot, print)
walk(td4_s_IBM_scaa_plot, print)
walk(td4_l_Scen_scaa_plot, print)
walk(td4_s_Scen_scaa_plot, print)

ssb_ssbmsy_l
f_fmsy_l 
catch_msy_l
ssb_ssbmsy_s
f_fmsy_s 
catch_msy_s 

dev.off()




