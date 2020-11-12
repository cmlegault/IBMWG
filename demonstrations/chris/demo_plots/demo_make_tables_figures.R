# demo_make_tables_figures.R
# uses demo_perform-metrics.rds results to create summary tables and figures
# changed to use results/perform-metrics_clean and _scaa files

library(tidyverse)
library(aplpack)
library(pracma)
library(car)
library(gplots)

# ### read in the performance metrics results
# mse_results <- readRDS("demonstrations/chris/demo_plots/demo-perform-metrics.rds")
# startdim <- dim(mse_results)
# 
# # remove any duplicate rows
# mse_results <- mse_results %>%
#   distinct()
# enddim <- dim(mse_results)
# startdim - enddim # if greater than zero, then there were duplicates
# names(mse_results)
# head(mse_results)
# 
# # find and remove duplicate scenarios
# mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")
# dupes <- duplicated(mse_sim_setup[,-(1:2)])
# not_dupes <- mse_sim_setup$rowid[!dupes]
# mse_results <- mse_results %>%
#   filter(rowid %in% not_dupes)
# 
# # remove duplicate rowids keeping most recently added
# revrowid <- rev(mse_results$rowid)
# myrowid_dupes <- duplicated(revrowid)
# myrowid_not_dupes <- revrowid[!myrowid_dupes]
# mse_results <- mse_results %>%
#   filter(rowid %in% myrowid_not_dupes)
 
mse_results <- readRDS("perform-metrics_clean.rds")
scaa_results <- readRDS("perform-metrics_scaa.rds")
noretro_results <- readRDS("perform-metrics_noretro.rds")
mse_sim_setup <- readRDS("mse_sim_setup.rds")
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]

mse_results <- mse_results %>%
  filter(rowid %in% not_dupes)

scaa_results <- scaa_results %>%
  filter(rowid %in% not_dupes)

noretro_results <- noretro_results %>%
  filter(rowid %in% not_dupes)

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

nsim_plot <- ggplot(rbind(countIBM, countIBM_scaa, countIBM_noretro), 
                    aes(x=Scenlab, y=nsim)) +
  geom_bar(stat = "identity") +
  facet_wrap(~IBMlab) +
  labs(x="Scenario", y="Number of Simulations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

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
        file = "ssb_mean_by_scenario.rds")

saveRDS(f_mean_by_scenario,
        file = "f_mean_by_scenario.rds")

saveRDS(catch_mean_by_scenario, 
        file = "catch_mean_by_scenario.rds")

saveRDS(ssb_mean_by_scenario_scaa, 
        file = "ssb_mean_by_scenario_scaa.rds")

saveRDS(f_mean_by_scenario_scaa,
        file = "f_mean_by_scenario_scaa.rds")

saveRDS(catch_mean_by_scenario_scaa, 
        file = "catch_mean_by_scenario_scaa.rds")

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

### put plots into pdf
pdf(file = "demonstrations/chris/demo_plots/demo_make_tables_figures.pdf")
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
ssb_probs_plot 
colorize_confetti(ssb_probs_plot)
ssb_ns_plot 
colorize_confetti(ssb_ns_plot)
ssb_ratios_plot 
colorize_confetti(ssb_ratios_plot)

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
f_probs_plot 
colorize_confetti(f_probs_plot)
f_ns_plot 
colorize_confetti(f_ns_plot)
f_ratios_plot 
colorize_confetti(f_ratios_plot)

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
catch_means_plot 
colorize_confetti(catch_means_plot)
catch_ratios_plot 
colorize_confetti(catch_ratios_plot)
catch_other_plot 
colorize_confetti(catch_other_plot)

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

for (i in 1:length(td4_l_IBM_plot)){
  print(td4_l_IBM_plot[[i]])
}
for (i in 1:length(td4_s_IBM_plot)){
  print(td4_s_IBM_plot[[i]])
}
for (i in 1:length(td4_l_Scen_plot)){
  print(td4_l_Scen_plot[[i]])
}
for (i in 1:length(td4_s_Scen_plot)){
  print(td4_s_Scen_plot[[i]])
}

for (i in 1:length(td4_l_IBM_scaa_plot)){
  print(td4_l_IBM_scaa_plot[[i]])
}
for (i in 1:length(td4_s_IBM_scaa_plot)){
  print(td4_s_IBM_scaa_plot[[i]])
}
for (i in 1:length(td4_l_Scen_scaa_plot)){
  print(td4_l_Scen_scaa_plot[[i]])
}
for (i in 1:length(td4_s_Scen_scaa_plot)){
  print(td4_s_Scen_scaa_plot[[i]])
}

dev.off()





# Some analysis that Liz added (distribution of data with and without transformation; ANOVA for important factors by IBM; Bagplots; Heatmaps)
## ANOVA for avg_ssb_ssbmsy, avg_f_fmsy, and avg_catch_msy BY IBM


#make anova table
make.aov.table <- function(dataset, time.as.factor, out.table.name)  {
  
  method <- unique(dataset$IBMlab)  
  
  if (time.as.factor==T) test.factors <- "retro_type + time.avg + Fhist   + Fhist*catch.mult + Fhist*time.avg + retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+ catch.mult*time.avg + n_selblocks + catch.mult"
  if (time.as.factor==F) test.factors <- "retro_type + Fhist + n_selblocks + catch.mult +  retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult"
  
  n.factors <- length(strsplit(test.factors, split="+", fixed=T) [[1]])
  
  ss.table <- matrix(nrow=n.factors, ncol=length(method))
  sig.table <- matrix(nrow=n.factors, ncol=length(method))
  
  
  for (i in 1:length(method)) {
    
    
    tmp <- dataset[dataset$IBMlab==method[i],] 
  
  if (time.as.factor==T)    tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+  catch.mult*time.avg +   Fhist*catch.mult) , data = tmp)
  
  if (time.as.factor==F)    tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult +  retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
  
  tmp.sum <- summary(tmp.aov)[[1]]
  
  ss.tot <- sum(tmp.sum[,2])
  frac.ss <- tmp.sum[1:n.factors,2]/ss.tot
  ss.table[,i] <- frac.ss
  
  tmp.sig <- tmp.sum[1:n.factors,5]
  tmp.code <- rep(NA, n.factors)
  tmp.code[(which(tmp.sig<0.1))] <- 0.05
  tmp.code[(which(tmp.sig<0.05))] <- 0.01
  tmp.code[(which(tmp.sig<0.01))] <- 0.001
  tmp.code[(which(tmp.sig<0.001))] <- 0
  
  
  sig.table[,i] <- tmp.code
  
  
}#end i loop over method

rownames(ss.table) <- rownames(tmp.sum)[1:n.factors]
colnames(ss.table) <- method
s.table <- rbind(ss.table, "SS.frac.explained"=apply(ss.table, 2, sum) )
s.table <- round(s.table,3)
write.csv(s.table, file=paste0(out.table.name,".ANOVA.csv"))
rownames(sig.table  ) <- rownames(tmp.sum)[1:n.factors]
colnames(sig.table ) <- method
write.csv(sig.table, file=paste0("Signif.",out.table.name,".ANOVA.csv"))

}  # end function make.aov.table


# function to examine distribution of (transformed) data and check normality
plot.aov.dist <- function(dataset, time.as.factor, fig.label, out.label)  {

  method <- unique(dataset$IBMlab)  
  p.list <- list()
  pln.list <- list()
  psqrt.list <- list()
  pasinsqrt.list <- list()
  
  q.list <- list()
  qln.list <- list()
  qsqrt.list <- list()
  qasinsqrt.list <- list()
  
  pdf(file=paste0(out.label, ".pdf"), onefile=T)
  par(mfrow=c(2,2))
  
  for (i in 1:length(method)) {
    
    
    tmp <- dataset[dataset$IBMlab==method[i],] 
    p.list[[i]] <- hist(tmp$value, main=method[i], xlab=fig.label)
    pln.list[[i]] <- hist(log(tmp$value), main=method[i], xlab=paste0("LN_",fig.label))
    psqrt.list[[i]] <- hist(sqrt(tmp$value), main=method[i], xlab=paste0("SQRT_",fig.label))
    plot(x=1, y=1, type='n', bty='n', axes=F, xlab="", ylab="")
    #pasinsqrt.list[[i]] <- hist(asin(sqrt(tmp$value)), main=method[i], xlab=paste0("ARCSIN_SQRT",fig.label))
    
    # if time as factor ==F then run this code chunk
    if (time.as.factor==F)  {
    tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
    q.list[i] <- qqPlot(tmp.aov$residuals, id = FALSE, main="NO Transform" )

    tmp.aov <- aov(log(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
    qln.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE , main= "LN Transform")
    
    tmp.aov <- aov(sqrt(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
    qsqrt.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE, main="SQRT Transform" )
    
    plot(x=1, y=1, type='n', bty='n', axes=F, xlab="", ylab="")
    # tmp.aov <- aov(asin(sqrt(value)) ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
    # qasinsqrt.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE, main="ARCSIN-SQRT Transform" )
    
    } # end test for time.as.factor==F
    
   
    # if time as factor ==T then run this code chunk
    if (time.as.factor==T)  {
      tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult), data = tmp)
      q.list[i] <- qqPlot(tmp.aov$residuals, id = FALSE )
      
      tmp.aov <- aov(log(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult) , data = tmp)
      qln.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE )
      
      tmp.aov <- aov(sqrt(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult) , data = tmp)
      qsqrt.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE )
      
      plot(x=1, y=1, type='n', bty='n', axes=F, xlab="", ylab="")
      
      # tmp.aov <- aov((value) ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult) , data = tmp)
      # qasinsqrt.list[i] <- qqPlot(asin(sqrt(tmp.aov$residuals)), id = FALSE )
      
    } # end test for time.as.factor==F 
    
    
  }#end i loop over method
  
  #save PDF of distributions
  
  # pdf(file=paste0(out.label, ".pdf"), onefile=T)
  # par(mfrow=c(2,2))
  # plot(q.list)
  dev.off()
  
} # end plot.aov.dist


ssb_sims.time <- ssb_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  mutate(time.avg = ifelse(substr(ssb_sims$metric,1,1)=="l", "L", "S"))
ssb_sims.short <- ssb_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  filter(substr(ssb_sims$metric,1,1)=="s")
ssb_sims.long <- ssb_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  filter(substr(ssb_sims$metric,1,1)=="l")

  

f_sims.time <- f_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  mutate(time.avg = ifelse(substr(f_sims$metric,1,1)=="l", "L", "S"))
f_sims.short <- f_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  filter(substr(f_sims$metric,1,1)=="s")
f_sims.long <- f_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  filter(substr(f_sims$metric,1,1)=="l")


catch_sims.time <- catch_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  mutate(time.avg = ifelse(substr(catch_sims$metric,1,1)=="l", "L", "S"))
catch_sims.short <- catch_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  filter(substr(catch_sims$metric,1,1)=="s")
catch_sims.long <- catch_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  filter(substr(ssb_sims$metric,1,1)=="l")



plot.aov.dist(dataset=ssb_sims, time.as.factor=F, fig.label="AVG_SSBG_SSBMSY", out.label="Dist_AVG_SSB_SSBMSY")
plot.aov.dist(dataset=f_sims, time.as.factor=F, fig.label="AVG_F_FMSY", out.label="Dist_AVG_F_FMSY")
plot.aov.dist(dataset=catch_sims, time.as.factor=F, fig.label="AVG_CATCH_MSY", out.label="Dist_AVG_CATCH_MSY")

make.aov.table(dataset=ssb_sims.time, time.as.factor=T, out.table.name="SSB.frac.explained")
#make.aov.table(dataset=ssb_sims.short, time.as.factor=F, out.table.name="SSB.short.frac.explained")
#make.aov.table(dataset=ssb_sims.long, time.as.factor=F, out.table.name="SSB.long.frac.explained")

make.aov.table(dataset=f_sims.time, time.as.factor=T, out.table.name="F.frac.explained")
make.aov.table(dataset=catch_sims.time, time.as.factor=T, out.table.name="Catch.frac.explained")



## Bagplots ====
make.bagplot <- function(dataset, max.axis="all", plot.outliers, out.label, save.type, save.resolution)  {
  
  method <- unique(dataset$IBMlab) 
  bx.max <- rep(NA, length(method)) #vector of x-axis max
  by.max <- rep(NA, length(method)) #vector of x-axis max
  
  xmax.all <- max(dataset$l_avg_ssb_ssbmsy, dataset$s_avg_ssb_ssbmsy)
  ymax.all <- max(dataset$l_avg_catch_msy, dataset$s_avg_catch_msy)
  
  b1.short <- list()
  b1.long <- list()
  

  
  for (i in 1:length(method))  {

    tmp <- dataset[dataset$IBMlab==method[i],] 
    

    b1.short[[i]] <- compute.bagplot(tmp$s_avg_ssb_ssbmsy, tmp$s_avg_catch_msy)
    b1.long[[i]] <-  compute.bagplot(tmp$l_avg_ssb_ssbmsy, tmp$l_avg_catch_msy)
    bx.max[i] <- 1.1*max(b1.short[[i]]$hull.loop[,1])
    by.max[i] <- 1.1*max(b1.short[[i]]$hull.loop[,2])

  } # end i-loop over method
  
  graphics.off()
  if (save.type=="png") png(file=paste0(out.label,".png"), height=14, width=12, units="in",
                     res=save.resolution, type="cairo"  )
  
  if (save.type=="pdf") pdf(file=paste0(out.label,".pdf"), height=14, width=12, onefile=T)
 
  par(mfrow=c(4,4), mar=c(0,0,0,0), oma=c(5,5,1,1)) 
  
  #plot1 ==========================================================
  plot(rep(0,2), rep(0,2), type='n', xaxs='r',yaxs='i',axes=F, xlab="", ylab="",  xlim=c(0,xmax.all), ylim=c(0, ymax.all) )  
  axis(side=1, at= seq(1,xmax.all), labels=rep("", length(seq(1,xmax.all))) , cex.axis=1.1)
  axis(side=2, at=seq(1,ymax.all), labels=seq(1,ymax.all), cex.axis=1.1)
  box()
  
  #short term bagplot ===
  polygon(c(b1.short[[1]]$hull.loop[,1], b1.short[[1]]$hull.loop[1,1]), c(b1.short[[1]]$hull.loop[,2], b1.short[[1]]$hull.loop[1,2]),  col='#66666633', border='#666666cc' )
  polygon(c(b1.short[[1]]$hull.bag[,1], b1.short[[1]]$hull.bag[1,1]), c(b1.short[[1]]$hull.bag[,2], b1.short[[1]]$hull.bag[1,2]),  col='#66666677', border='#666666cc' )
  points(b1.short[[1]]$center[1], b1.short[[1]]$center[2], pch=16, col='black' )

  # long term bagplot ====
  polygon(c(b1.long[[1]]$hull.loop[,1], b1.long[[1]]$hull.loop[1,1]), c(b1.long[[1]]$hull.loop[,2], b1.long[[1]]$hull.loop[1,2]),  col='#0055AA33', border='#666666cc' )
  polygon(c(b1.long[[1]]$hull.bag[,1], b1.long[[1]]$hull.bag[1,1]), c(b1.long[[1]]$hull.bag[,2], b1.long[[1]]$hull.bag[1,2]),  col='#0055AA77' , border='#0055AAcc' )
  points(b1.long[[1]]$center[1], b1.long[[1]]$center[2], pch=16, col='blue' )
  
  
  
  # outliers ========
  if (plot.outliers==T){
    points(b1.short[[1]]$pxy.outlier[,1], b1.short[[1]]$pxy.outlier[,2], pch=1, col='#44444444')
    points(b1.long[[1]]$pxy.outlier[,1], b1.long[[1]]$pxy.outlier[,2], pch=1, col='#0055dd77')
  }

  #add kobe plot lines
  abline(v=1, col='red', lty=2)
  abline(h=1, col='red', lty=2)
  abline(v=0.5, col='orange', lty=4)

  title(main=method[1], line=-1, cex=0.9)
  legend(x=8, y=6.5, legend=c("Short-IQR", "Short-95%", "Long-IQR", "Long-95%"), cex=1.1, pch=rep(15,4), col=c('#66666677','#66666633', '#0055AA77',  '#0055AA33'))


    #plots 2-4 =================================================
   for (i in 2:4) {
     plot(rep(0,2), rep(0,2), type='n', xaxs='r',yaxs='i',axes=F, xlab="", ylab="",  xlim=c(0,xmax.all), ylim=c(0, ymax.all) )  
     axis(side=1, at= seq(1,xmax.all), labels=rep("", length(seq(1,xmax.all))) , cex.axis=1.1)
     axis(side=2, at=seq(1,ymax.all), labels=rep("", length(seq(1,ymax.all))), cex.axis=1.1)
     box()
     
     #short term bagplot ===
     polygon(c(b1.short[[i]]$hull.loop[,1], b1.short[[i]]$hull.loop[1,1]), c(b1.short[[i]]$hull.loop[,2], b1.short[[i]]$hull.loop[1,2]),  col='#66666633', border='#666666cc' )
     polygon(c(b1.short[[i]]$hull.bag[,1], b1.short[[i]]$hull.bag[1,1]), c(b1.short[[i]]$hull.bag[,2], b1.short[[i]]$hull.bag[1,2]),  col='#66666677', border='#666666cc' )
     points(b1.short[[i]]$center[1], b1.short[[i]]$center[2], pch=16, col='black' )
     
     # long term bagplot ====
     polygon(c(b1.long[[i]]$hull.loop[,1], b1.long[[i]]$hull.loop[1,1]), c(b1.long[[i]]$hull.loop[,2], b1.long[[i]]$hull.loop[1,2]),  col='#0055AA33', border='#666666cc' )
     polygon(c(b1.long[[i]]$hull.bag[,1], b1.long[[i]]$hull.bag[1,1]), c(b1.long[[i]]$hull.bag[,2], b1.long[[i]]$hull.bag[1,2]),  col='#0055AA77' , border='#0055AAcc' )
     points(b1.long[[i]]$center[1], b1.long[[i]]$center[2], pch=16, col='blue' )
     
     
     # outliers ========
     if (plot.outliers==T){
       points(b1.short[[i]]$pxy.outlier[,1], b1.short[[i]]$pxy.outlier[,2], pch=1, col='#44444444')
       points(b1.long[[i]]$pxy.outlier[,1], b1.long[[i]]$pxy.outlier[,2], pch=1, col='#0055dd77')
     }
     
     #add kobe plot lines
     abline(v=1, col='red', lty=2)
     abline(h=1, col='red', lty=2)
     abline(v=0.5, col='orange', lty=4)
     
     title(main=method[i], line=-1, cex=0.9)

     
   }#end plots 2-4
  
  
 
  #plot 5 ==========================================================
  plot(rep(0,2), rep(0,2), type='n', xaxs='r',yaxs='i',axes=F, xlab="", ylab="",  xlim=c(0,xmax.all), ylim=c(0, ymax.all) )  
  axis(side=1, at= seq(1,xmax.all), labels=rep("", length(seq(1,xmax.all))) , cex.axis=1.1)
  axis(side=2, at=seq(1,ymax.all), labels=seq(1,ymax.all), cex.axis=1.1)
  box()
  
  #short term bagplot ===
  polygon(c(b1.short[[5]]$hull.loop[,1], b1.short[[5]]$hull.loop[1,1]), c(b1.short[[5]]$hull.loop[,2], b1.short[[5]]$hull.loop[1,2]),  col='#66666633', border='#666666cc' )
  polygon(c(b1.short[[5]]$hull.bag[,1], b1.short[[5]]$hull.bag[1,1]), c(b1.short[[5]]$hull.bag[,2], b1.short[[5]]$hull.bag[1,2]),  col='#66666677', border='#666666cc' )
  points(b1.short[[5]]$center[1], b1.short[[5]]$center[2], pch=16, col='black' )
  
  # long term bagplot ====
  polygon(c(b1.long[[5]]$hull.loop[,1], b1.long[[5]]$hull.loop[1,1]), c(b1.long[[5]]$hull.loop[,2], b1.long[[5]]$hull.loop[1,2]),  col='#0055AA33', border='#666666cc' )
  polygon(c(b1.long[[5]]$hull.bag[,1], b1.long[[5]]$hull.bag[1,1]), c(b1.long[[5]]$hull.bag[,2], b1.long[[5]]$hull.bag[1,2]),  col='#0055AA77' , border='#0055AAcc' )
  points(b1.long[[5]]$center[1], b1.long[[5]]$center[2], pch=16, col='blue' )
  
  
  # outliers ========
  if (plot.outliers==T){
    points(b1.short[[5]]$pxy.outlier[,1], b1.short[[5]]$pxy.outlier[,2], pch=1, col='#44444444')
    points(b1.long[[5]]$pxy.outlier[,1], b1.long[[5]]$pxy.outlier[,2], pch=1, col='#0055dd77')
  }
  
  #add kobe plot lines
  abline(v=1, col='red', lty=2)
  abline(h=1, col='red', lty=2)
  abline(v=0.5, col='orange', lty=4)
  
  title(main=method[5], line=-2, cex=0.9)


  
  #plots 6-8 =================================================
  for (i in 6:8) {
    plot(rep(0,2), rep(0,2), type='n', xaxs='r',yaxs='i',axes=F, xlab="", ylab="",  xlim=c(0,xmax.all), ylim=c(0, ymax.all) )  
    axis(side=1, at= seq(1,xmax.all), labels=rep("", length(seq(1,xmax.all))) , cex.axis=1.1)
    axis(side=2, at=seq(1,ymax.all), labels=rep("", length(seq(1,ymax.all))), cex.axis=1.1)
    box()
    
    #short term bagplot ===
    polygon(c(b1.short[[i]]$hull.loop[,1], b1.short[[i]]$hull.loop[1,1]), c(b1.short[[i]]$hull.loop[,2], b1.short[[i]]$hull.loop[1,2]),  col='#66666633', border='#666666cc' )
    polygon(c(b1.short[[i]]$hull.bag[,1], b1.short[[i]]$hull.bag[1,1]), c(b1.short[[i]]$hull.bag[,2], b1.short[[i]]$hull.bag[1,2]),  col='#66666677', border='#666666cc' )
    points(b1.short[[i]]$center[1], b1.short[[i]]$center[2], pch=16, col='black' )
    
    # long term bagplot ====
    polygon(c(b1.long[[i]]$hull.loop[,1], b1.long[[i]]$hull.loop[1,1]), c(b1.long[[i]]$hull.loop[,2], b1.long[[i]]$hull.loop[1,2]),  col='#0055AA33', border='#666666cc' )
    polygon(c(b1.long[[i]]$hull.bag[,1], b1.long[[i]]$hull.bag[1,1]), c(b1.long[[i]]$hull.bag[,2], b1.long[[i]]$hull.bag[1,2]),  col='#0055AA77' , border='#0055AAcc' )
    points(b1.long[[i]]$center[1], b1.long[[i]]$center[2], pch=16, col='blue' )
    
    
    
    # outliers ========
    if (plot.outliers==T){
      points(b1.short[[i]]$pxy.outlier[,1], b1.short[[i]]$pxy.outlier[,2], pch=1, col='#44444444')
      points(b1.long[[i]]$pxy.outlier[,1], b1.long[[i]]$pxy.outlier[,2], pch=1, col='#0055dd77')
    }
    
    #add kobe plot lines
    abline(v=1, col='red', lty=2)
    abline(h=1, col='red', lty=2)
    abline(v=0.5, col='orange', lty=4)
    
    title(main=method[i], line=-2, cex=0.9)
    
    
  }#end plots 6-8
  
  
  
  
  
  #plot 9 ==========================================================
  plot(rep(0,2), rep(0,2), type='n', xaxs='r',yaxs='i',axes=F, xlab="", ylab="",  xlim=c(0,xmax.all), ylim=c(0, ymax.all) )  
  axis(side=1, at= seq(1,xmax.all), labels=rep("", length(seq(1,xmax.all))) , cex.axis=1.1)
  axis(side=2, at=seq(1,ymax.all), labels=seq(1,ymax.all), cex.axis=1.1)
  box()
  
  #short term bagplot ===
  polygon(c(b1.short[[9]]$hull.loop[,1], b1.short[[9]]$hull.loop[1,1]), c(b1.short[[9]]$hull.loop[,2], b1.short[[9]]$hull.loop[1,2]),  col='#66666633', border='#666666cc' )
  polygon(c(b1.short[[9]]$hull.bag[,1], b1.short[[9]]$hull.bag[1,1]), c(b1.short[[9]]$hull.bag[,2], b1.short[[9]]$hull.bag[1,2]),  col='#66666677', border='#666666cc' )
  points(b1.short[[9]]$center[1], b1.short[[9]]$center[2], pch=16, col='black' )
  
  # long term bagplot ====
  polygon(c(b1.long[[9]]$hull.loop[,1], b1.long[[9]]$hull.loop[1,1]), c(b1.long[[9]]$hull.loop[,2], b1.long[[9]]$hull.loop[1,2]),  col='#0055AA33', border='#666666cc' )
  polygon(c(b1.long[[9]]$hull.bag[,1], b1.long[[9]]$hull.bag[1,1]), c(b1.long[[9]]$hull.bag[,2], b1.long[[9]]$hull.bag[1,2]),  col='#0055AA77' , border='#0055AAcc' )
  points(b1.long[[9]]$center[1], b1.long[[9]]$center[2], pch=16, col='blue' )
  
  
  # outliers ========
  if (plot.outliers==T){
    points(b1.short[[9]]$pxy.outlier[,1], b1.short[[9]]$pxy.outlier[,2], pch=1, col='#44444444')
    points(b1.long[[9]]$pxy.outlier[,1], b1.long[[9]]$pxy.outlier[,2], pch=1, col='#0055dd77')
  }
  
  #add kobe plot lines
  abline(v=1, col='red', lty=2)
  abline(h=1, col='red', lty=2)
  abline(v=0.5, col='orange', lty=4)
  
  title(main=method[9], line=-2, cex=0.9)
  
  
  
  #plots 10-12 =================================================
  for (i in 10:12) {
    plot(rep(0,2), rep(0,2), type='n', xaxs='r',yaxs='i',axes=F, xlab="", ylab="",  xlim=c(0,xmax.all), ylim=c(0, ymax.all) )  
    axis(side=1, at= seq(1,xmax.all), labels=seq(1,xmax.all) , cex.axis=1.1)
    axis(side=2, at=seq(1,ymax.all), labels=rep("", length(seq(1,ymax.all))), cex.axis=1.1)
    box()
    
    #short term bagplot ===
    polygon(c(b1.short[[i]]$hull.loop[,1], b1.short[[i]]$hull.loop[1,1]), c(b1.short[[i]]$hull.loop[,2], b1.short[[i]]$hull.loop[1,2]),  col='#66666633', border='#666666cc' )
    polygon(c(b1.short[[i]]$hull.bag[,1], b1.short[[i]]$hull.bag[1,1]), c(b1.short[[i]]$hull.bag[,2], b1.short[[i]]$hull.bag[1,2]),  col='#66666677', border='#666666cc' )
    points(b1.short[[i]]$center[1], b1.short[[i]]$center[2], pch=16, col='black' )
    
    # long term bagplot ====
    polygon(c(b1.long[[i]]$hull.loop[,1], b1.long[[i]]$hull.loop[1,1]), c(b1.long[[i]]$hull.loop[,2], b1.long[[i]]$hull.loop[1,2]),  col='#0055AA33', border='#666666cc' )
    polygon(c(b1.long[[i]]$hull.bag[,1], b1.long[[i]]$hull.bag[1,1]), c(b1.long[[i]]$hull.bag[,2], b1.long[[i]]$hull.bag[1,2]),  col='#0055AA77' , border='#0055AAcc' )
    points(b1.long[[i]]$center[1], b1.long[[i]]$center[2], pch=16, col='blue' )
    
    # outliers ========
    if (plot.outliers==T){
      points(b1.short[[i]]$pxy.outlier[,1], b1.short[[i]]$pxy.outlier[,2], pch=1, col='#44444444')
      points(b1.long[[i]]$pxy.outlier[,1], b1.long[[i]]$pxy.outlier[,2], pch=1, col='#0055dd77')
    }
    
    #add kobe plot lines
    abline(v=1, col='red', lty=2)
    abline(h=1, col='red', lty=2)
    abline(v=0.5, col='orange', lty=4)
    
    title(main=method[i], line=-2, cex=0.9)
    if (i==11) mtext("Avg_SSB_SSBmsy", side=1,line=3,outer=F, cex=1.1)
    
  }#end plots 10-12
 
  
  

  #plot 13 ==========================================================
  plot(rep(0,2), rep(0,2), type='n', xaxs='r',yaxs='i',axes=F, xlab="", ylab="",  xlim=c(0,xmax.all), ylim=c(0, ymax.all) )  
  axis(side=1, at= seq(1,xmax.all), labels=seq(1,xmax.all) , cex.axis=1.1)
  axis(side=2, at=seq(1,ymax.all), labels=seq(1,ymax.all), cex.axis=1.1)
  box()
  
  #short term bagplot ===
  polygon(c(b1.short[[13]]$hull.loop[,1], b1.short[[13]]$hull.loop[1,1]), c(b1.short[[13]]$hull.loop[,2], b1.short[[13]]$hull.loop[1,2]),  col='#66666633', border='#666666cc' )
  polygon(c(b1.short[[13]]$hull.bag[,1], b1.short[[13]]$hull.bag[1,1]), c(b1.short[[13]]$hull.bag[,2], b1.short[[13]]$hull.bag[1,2]),  col='#66666677', border='#666666cc' )
  points(b1.short[[13]]$center[1], b1.short[[13]]$center[2], pch=16, col='black' )
  
  # long term bagplot ====
  polygon(c(b1.long[[13]]$hull.loop[,1], b1.long[[13]]$hull.loop[1,1]), c(b1.long[[13]]$hull.loop[,2], b1.long[[13]]$hull.loop[1,2]),  col='#0055AA33', border='#666666cc' )
  polygon(c(b1.long[[13]]$hull.bag[,1], b1.long[[13]]$hull.bag[1,1]), c(b1.long[[13]]$hull.bag[,2], b1.long[[13]]$hull.bag[1,2]),  col='#0055AA77' , border='#0055AAcc' )
  points(b1.long[[13]]$center[1], b1.long[[13]]$center[2], pch=16, col='blue' )
  
  # outliers ========
  if (plot.outliers==T){
  points(b1.short[[13]]$pxy.outlier[,1], b1.short[[13]]$pxy.outlier[,2], pch=1, col='#44444444')
  points(b1.long[[13]]$pxy.outlier[,1], b1.long[[13]]$pxy.outlier[,2], pch=1, col='#0055dd77')
  }
  
  #add kobe plot lines
  abline(v=1, col='red', lty=2)
  abline(h=1, col='red', lty=2)
  abline(v=0.5, col='orange', lty=4)
  
  title(main=method[13], line=-2, cex=0.9)  
  
  mtext("Avg_SSB_SSBmsy", side=1,line=3,outer=F, cex=1.1)
  mtext("Avg_Catch_MSY", side=2,line=3,outer=T, cex=1.1)
  
  
  dev.off()
  
} # end function make.bagplot



make.bagplot(dataset=sims, max.axis="all", plot.outliers=T, out.label="Bagplots_smallsize.IBM", save.type="pdf", save.resolution=200)



#==== HEATMAP STUFF ====

ssb_mean_by_scenario <- ssb_mean_by_scenario %>%
  filter(grepl("_n_ge_bmsy", metric)) %>%
  mutate(term = ifelse(substr(metric, 1, 1) == "l", "long", "short")) %>%
  mutate(metric = paste0("ssb_", metric)) %>%
  mutate(metrictype = "ssb")

f_mean_by_scenario <- f_mean_by_scenario %>%
  filter(grepl("n_less_fmsy", metric)) %>%
  mutate(term = ifelse(substr(metric, 1, 1) == "l", "long", "short")) %>%
  mutate(metric = paste0("f_", metric)) %>%
  mutate(metrictype = "f")


catch_mean_by_scenario <- catch_mean_by_scenario %>%
  filter(grepl("avg_catch", metric)) %>%
  mutate(term = ifelse(substr(metric, 1, 1) == "l", "long", "short")) %>%
  mutate(metric = paste0("catch_", metric)) %>%
  mutate(metrictype = "catch")


alldata <- rbind(ssb_mean_by_scenario, f_mean_by_scenario, catch_mean_by_scenario)

all_IBM <- alldata %>%
  group_by(metrictype, IBMlab) %>%
  summarise(meanval = mean(value))
all_IBM
all <- cbind(SSB = all_IBM$meanval[all_IBM$metrictype=="ssb"], Catch = all_IBM$meanval[all_IBM$metrictype=="catch"], F= all_IBM$meanval[all_IBM$metrictype=="f"])
rownames(all) <- unique(all_IBM$IBMlab)


# heatmap specs

# choose colors from colorbrewer here:
#    https://colorbrewer2.org/#type=sequential&scheme=YlOrBr&n=9

#heat.cols <- c('#ffffd4','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#8c2d04')  # 7 colors
heat.cols <- c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')  # 9 colors

heat.all <-heatmap(all, scale="column", Colv=NA, margins=c(8,10), main = "IBM")
heat2.all <-heatmap.2(all, scale="column", keysize = 1, margins=c(7,7), col=heat.cols, main = "IBM")   #, keysize = 1, col=("heat.colors"), main = "IBM")  #margins=c(7,7),


all_IBM_term <- alldata %>%
  group_by(metrictype, IBMlab, term) %>%
  summarise(meanval = mean(value))
all_IBM_term
all.term <- cbind(SSB = all_IBM_term$meanval[all_IBM_term$metrictype=="ssb"], Catch = all_IBM_term$meanval[all_IBM_term$metrictype=="catch"], F= all_IBM_term$meanval[all_IBM_term$metrictype=="f"])
rownames(all.term) <- unique(paste0(all_IBM_term$IBMlab, "-", all_IBM_term$term))
heat.all.term <- heatmap(all.term, scale="column", Colv=NA, margins=c(8,10), main = "IBM by Time Horizon")
heat2.all.term <- heatmap.2(all.term, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Time Horizon")


all_IBM_retro <- alldata %>%
  group_by(metrictype, IBMlab, retro_type) %>%
  summarise(meanval = mean(value))
all_IBM_retro
all.retro <- cbind(SSB = all_IBM_retro$meanval[all_IBM_retro$metrictype=="ssb"], Catch = all_IBM_retro$meanval[all_IBM_retro$metrictype=="catch"], F= all_IBM_retro$meanval[all_IBM_retro$metrictype=="f"])
rownames(all.retro) <- unique(paste0(all_IBM_retro$IBMlab, "-", all_IBM_retro$retro_type))
heat.all.retro <- heatmap(all.retro, scale="column", Colv=NA, margins=c(8,10), main = "IBM by Retro Source")
heat2.all.retro <- heatmap.2(all.retro, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by Retro Source")



all_IBM_cmult <- alldata %>%
  group_by(metrictype, IBMlab, catch.mult) %>%
  summarise(meanval = mean(value))
all_IBM_cmult
all.cmult <- cbind(SSB = all_IBM_cmult$meanval[all_IBM_cmult$metrictype=="ssb"], Catch = all_IBM_cmult$meanval[all_IBM_cmult$metrictype=="catch"], F= all_IBM_cmult$meanval[all_IBM_cmult$metrictype=="f"])
rownames(all.cmult) <- unique(paste0(all_IBM_cmult$IBMlab, "-", all_IBM_cmult$catch.mult))
heat.all.cmult <- heatmap(all.cmult, scale="column", Colv=NA, margins=c(8,10), main = "IBM by Catch Multiplier")
heat2.all.cmult <- heatmap.2(all.cmult, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by Catch Multiplier")



all_IBM_term_retro <- alldata %>%
  group_by(metrictype, IBMlab, term, retro_type) %>%
  summarise(meanval = mean(value))
all_IBM_term_retro
all.term.retro <- cbind(SSB = all_IBM_term_retro$meanval[all_IBM_term_retro$metrictype=="ssb"], Catch = all_IBM_term_retro$meanval[all_IBM_term_retro$metrictype=="catch"], F= all_IBM_term_retro$meanval[all_IBM_term_retro$metrictype=="f"])
rownames(all.term.retro) <- unique(paste0(all_IBM_term_retro$IBMlab, "-", all_IBM_term_retro$retro_type, "-", all_IBM_term_retro$term))
heat.all.term.retro <- heatmap(all.term.retro, scale="column",  margins=c(8,10), main = "IBM by Retro Source and Time Horizon")
heat2.all.term.retro <- heatmap.2(all.term.retro, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source and Time Horizon")


# --- make pdf files ====

pdf(file = "heatmaps.pdf", onefile = T) 
heat.all <-heatmap(all, scale="column", Colv=NA, margins=c(8,10), main = "IBM")
heat.all.cmult <- heatmap(all.cmult, scale="column", Colv=NA, margins=c(8,10), main = "IBM by Catch Multiplier")
heat.all.retro <- heatmap(all.retro, scale="column", Colv=NA, margins=c(8,10), main = "IBM by Retro Source")
heat.all.term <- heatmap(all.term, scale="column", Colv=NA, margins=c(8,10), main = "IBM by Time Horizon")
heat.all.term.retro <- heatmap(all.term.retro, scale="column", Colv=NA, margins=c(8,10), main = "IBM by Retro Source and Time Horizon")

dev.off()

pdf(file = "heatmaps_ohhh_fancier.pdf")
heat2.all <-heatmap.2(all, scale="column", margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM")
heat2.all.retro <- heatmap.2(all.retro, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Retro Source")
heat2.all.term <- heatmap.2(all.term, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Time Horizon")
heat2.all.cmult <- heatmap.2(all.cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by Catch Multiplier")
heat2.all.term.retro <- heatmap.2(all.term.retro, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source and Time Horizon")
dev.off()


# --- make png files ====
#   png specs 
png.res <- 500
png.h <- 10
png.w <- 8

png(file="heatmap.ibm.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM")
dev.off()


png(file="heatmap.ibm.retro.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.retro, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source")
dev.off()


png(file="heatmap.ibm.time.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.term, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Time Horizon")
dev.off()


png(file="heatmap.ibm.cmult.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Catch Multiplier")
dev.off()


png(file="heatmap.ibm.retro.time.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.term.retro, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source and Time Horizon")
dev.off()


# --- write csv files for table of MEAN heatmap results ====

nrows <- dim(t(all))[1]
write.csv(t(heat2.all$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.csv")

nrows <- dim(t(all.retro))[1]
write.csv(t(heat2.all.retro$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.retro.csv")

nrows <- dim(t(all.term))[1]
write.csv(t(heat2.all.term$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.term.csv")

nrows <- dim(t(all.cmult))[1]
write.csv(t(heat2.all.cmult$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.cmult.csv")

nrows <- dim(t(heat2.all.term.retro$carpet))[1]
write.csv(t(heat2.all.term.retro$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.term.retro.csv")



#-- REPEAT for MEDIAN results
### compute mean for all metrics for each scenario
ssb_median_by_scenario <- ssb_results %>%
  group_by(iscen, metric) %>%
  summarise_all(median) %>%
  inner_join(defined, by = "iscen")

f_median_by_scenario <- f_results %>%
  group_by(iscen, metric) %>%
  summarise_all(median) %>%
  inner_join(defined, by = "iscen")

catch_median_by_scenario <- catch_results %>%
  group_by(iscen, metric) %>%
  summarise_all(median) %>%
  inner_join(defined, by = "iscen")




ssb_median_by_scenario <- ssb_median_by_scenario %>%
  filter(grepl("_n_ge_bmsy", metric)) %>%
  mutate(term = ifelse(substr(metric, 1, 1) == "l", "long", "short")) %>%
  mutate(metric = paste0("ssb_", metric)) %>%
  mutate(metrictype = "ssb")

f_median_by_scenario <- f_median_by_scenario %>%
  filter(grepl("n_less_fmsy", metric)) %>%
  mutate(term = ifelse(substr(metric, 1, 1) == "l", "long", "short")) %>%
  mutate(metric = paste0("f_", metric)) %>%
  mutate(metrictype = "f")


catch_median_by_scenario <- catch_median_by_scenario %>%
  filter(grepl("avg_catch", metric)) %>%
  mutate(term = ifelse(substr(metric, 1, 1) == "l", "long", "short")) %>%
  mutate(metric = paste0("catch_", metric)) %>%
  mutate(metrictype = "catch")


alldata.med <- rbind(ssb_median_by_scenario, f_median_by_scenario, catch_median_by_scenario)

all_IBM.med <- alldata %>%
  group_by(metrictype, IBMlab) %>%
  summarise(medianval = median(value))
all_IBM.med
all.med <- cbind(SSB = all_IBM.med$medianval[all_IBM.med$metrictype=="ssb"], Catch = all_IBM.med$medianval[all_IBM.med$metrictype=="catch"], F= all_IBM.med$medianval[all_IBM.med$metrictype=="f"])
rownames(all.med) <- unique(all_IBM.med$IBMlab)


all_IBM_term.med <- alldata.med %>%
  group_by(metrictype, IBMlab, term) %>%
  summarise(medianval = median(value))
all_IBM_term.med
all.term.med <- cbind(SSB = all_IBM_term.med$medianval[all_IBM_term.med$metrictype=="ssb"], Catch = all_IBM_term.med$medianval[all_IBM_term.med$metrictype=="catch"], F= all_IBM_term.med$medianval[all_IBM_term.med$metrictype=="f"])
rownames(all.term.med) <- unique(paste0(all_IBM_term.med$IBMlab, "-", all_IBM_term.med$term))


all_IBM_retro.med <- alldata.med %>%
  group_by(metrictype, IBMlab, retro_type) %>%
  summarise(medianval = median(value))
all_IBM_retro.med
all.retro.med <- cbind(SSB = all_IBM_retro.med$medianval[all_IBM_retro.med$metrictype=="ssb"], Catch = all_IBM_retro.med$medianval[all_IBM_retro.med$metrictype=="catch"], F= all_IBM_retro.med$medianval[all_IBM_retro.med$metrictype=="f"])
rownames(all.retro.med) <- unique(paste0(all_IBM_retro.med$IBMlab, "-", all_IBM_retro.med$retro_type))



all_IBM_cmult.med <- alldata.med %>%
  group_by(metrictype, IBMlab, catch.mult) %>%
  summarise(medianval = median(value))
all_IBM_cmult.med
all.cmult.med <- cbind(SSB = all_IBM_cmult.med$medianval[all_IBM_cmult.med$metrictype=="ssb"], Catch = all_IBM_cmult.med$medianval[all_IBM_cmult.med$metrictype=="catch"], F= all_IBM_cmult.med$medianval[all_IBM_cmult.med$metrictype=="f"])
rownames(all.cmult.med) <- unique(paste0(all_IBM_cmult.med$IBMlab, "-", all_IBM_cmult.med$catch.mult))



all_IBM_term_retro.med <- alldata.med %>%
  group_by(metrictype, IBMlab, term, retro_type) %>%
  summarise(medianval = median(value))
all_IBM_term_retro.med
all.term.retro.med <- cbind(SSB = all_IBM_term_retro.med$medianval[all_IBM_term_retro.med$metrictype=="ssb"], Catch = all_IBM_term_retro.med$medianval[all_IBM_term_retro.med$metrictype=="catch"], F= all_IBM_term_retro.med$medianval[all_IBM_term_retro.med$metrictype=="f"])
rownames(all.term.retro.med) <- unique(paste0(all_IBM_term_retro.med$IBMlab, "-", all_IBM_term_retro.med$retro_type, "-", all_IBM_term_retro.med$term))


# --- make pdf files ====

pdf(file = "heatmaps_median_ohhh_fancier.pdf")
heat2.all.med <-heatmap.2(all.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM")
heat2.all.retro.med <- heatmap.2(all.retro.med, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Retro Source")
heat2.all.term.med <- heatmap.2(all.term.med, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Time Horizon")
heat2.all.cmult.med <- heatmap.2(all.cmult.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by Catch Multiplier")
heat2.all.term.retro.med <- heatmap.2(all.term.retro.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source and Time Horizon")
dev.off()


# --- make png files ====
#   png specs 
png.res <- 500
png.h <- 10
png.w <- 8

png(file="heatmap.ibm_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM")
dev.off()


png(file="heatmap.ibm.retro_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.retro.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source")
dev.off()


png(file="heatmap.ibm.time_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.term.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Time Horizon")
dev.off()


png(file="heatmap.ibm.cmult_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.cmult.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Catch Multiplier")
dev.off()


png(file="heatmap.ibm.retro.time_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all.term.retro.med, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source and Time Horizon")
dev.off()


# --- write csv files for table of MEAN heatmap results ====

nrows <- dim(t(all.med))[1]
write.csv(t(heat2.all.med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm_median.csv")

nrows <- dim(t(all.retro.med))[1]
write.csv(t(heat2.all.retro.med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.retro_median.csv")

nrows <- dim(t(all.term.med))[1]
write.csv(t(heat2.all.term.med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.term_median.csv")

nrows <- dim(t(all.cmult.med))[1]
write.csv(t(heat2.all.cmult.med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.cmult_median.csv")

nrows <- dim(t(heat2.all.term.retro.med$carpet))[1]
write.csv(t(heat2.all.term.retro.med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.term.retro_median.csv")



# --- ONE MORE TIME, Take mean of the medians_by_scenario


# hist(ssb_median_by_scenario$value)
# hist(ssb_mean_by_scenario$value)
# hist(f_median_by_scenario$value)
# hist(f_mean_by_scenario$value)
# hist(catch_median_by_scenario$value)
# hist(catch_mean_by_scenario$value)
# 
# summary(ssb_median_by_scenario$value)
# summary(ssb_mean_by_scenario$value)
# summary(f_median_by_scenario$value)
# summary(f_mean_by_scenario$value)
# summary(catch_median_by_scenario$value)
# summary(catch_mean_by_scenario$value)
# 
# 
# mean(ssb_median_by_scenario$value)
# median(ssb_mean_by_scenario$value)
# mean(f_median_by_scenario$value)
# median(f_mean_by_scenario$value)
# mean(catch_median_by_scenario$value)
# median(catch_mean_by_scenario$value)


# ssb_sims.time
# test.sims <- rbind(ssb_sims.time, f_sims.time, catch_sims.time)
# 
# test.sims <- cbind(SSB=median(ssb_sims.time$value), F=median(f_sims.time$value), Catch=median(catch_sims.time$value) )
# 
# hist(ssb_sims.time$value)
# summary(ssb_sims.time$value)

# 1. by IBM
ssb_mean_by_ibm <- ssb_sims.time %>%
  group_by(IBMlab) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm <- ssb_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value)) 

f_mean_by_ibm <- f_sims.time %>%
  group_by(IBMlab) %>%
  summarise(meanval=mean(value)) 
f_median_by_ibm <- f_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value)) 

catch_mean_by_ibm <- catch_sims.time %>%
  group_by(IBMlab) %>%
  summarise(meanval=mean(value))
catch_median_by_ibm <- catch_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value))

all_mean <- cbind(SSB=ssb_mean_by_ibm$meanval, F=f_mean_by_ibm$meanval, Catch=catch_mean_by_ibm$meanval)

all_median_by_ibm <- cbind(SSB=ssb_median_by_ibm$medianval, F=f_median_by_ibm$medianval, Catch=catch_median_by_ibm$medianval)
rownames(all_median_by_ibm) <- ssb_median_by_ibm$IBMlab


# 2. by IBM*retro type
ssb_mean_by_ibm_retro <- ssb_sims.time %>%
  group_by(IBMlab, retro_type) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_retro <- ssb_sims.time %>%
  group_by(IBMlab, retro_type) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_retro <- f_sims.time %>%
  group_by(IBMlab, retro_type) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_retro <- catch_sims.time %>%
  group_by(IBMlab, retro_type) %>%
  summarise(medianval=median(value)) 

all_median_by_ibm_retro <- cbind(SSB=ssb_median_by_ibm_retro$medianval, F=f_median_by_ibm_retro$medianval, Catch=catch_median_by_ibm_retro$medianval)
rownames(all_median_by_ibm_retro) <- paste(ssb_median_by_ibm_retro$IBMlab, ssb_median_by_ibm_retro$retro_type, sep= "-")


# 3. by IBM* time
ssb_mean_by_ibm_time <- ssb_sims.time %>%
  group_by(IBMlab, time.avg) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_time <- ssb_sims.time %>%
  group_by(IBMlab, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_time <- f_sims.time %>%
  group_by(IBMlab, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_time <- catch_sims.time %>%
  group_by(IBMlab, time.avg) %>%
  summarise(medianval=median(value)) 

all_median_by_ibm_time <- cbind(SSB=ssb_median_by_ibm_time$medianval, F=f_median_by_ibm_time$medianval, Catch=catch_median_by_ibm_time$medianval)
rownames(all_median_by_ibm_time) <- paste(ssb_median_by_ibm_time$IBMlab, ssb_median_by_ibm_time$time.avg, sep= "-")


# 4. by IBM* catch multiplier
ssb_mean_by_ibm_catch.mult <- ssb_sims.time %>%
  group_by(IBMlab, catch.mult) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_catch.mult <- ssb_sims.time %>%
  group_by(IBMlab, catch.mult) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_catch.mult <- f_sims.time %>%
  group_by(IBMlab, catch.mult) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_catch.mult <- catch_sims.time %>%
  group_by(IBMlab, catch.mult) %>%
  summarise(medianval=median(value)) 


all_median_by_ibm_cmult <- cbind(SSB=ssb_median_by_ibm_catch.mult$medianval, F=f_median_by_ibm_catch.mult$medianval, Catch=catch_median_by_ibm_catch.mult$medianval)
rownames(all_median_by_ibm_cmult) <- paste(ssb_median_by_ibm_catch.mult$IBMlab, ssb_median_by_ibm_catch.mult$catch.mult, sep= "-")


# 5. by IBM* Fhistory
ssb_mean_by_ibm_Fhist <- ssb_sims.time %>%
  group_by(IBMlab, Fhist) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_Fhist <- ssb_sims.time %>%
  group_by(IBMlab, Fhist) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_Fhist <- f_sims.time %>%
  group_by(IBMlab, Fhist) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_Fhist <- catch_sims.time %>%
  group_by(IBMlab, Fhist) %>%
  summarise(medianval=median(value)) 


all_median_by_ibm_Fhist <- cbind(SSB=ssb_median_by_ibm_Fhist$medianval, F=f_median_by_ibm_Fhist$medianval, Catch=catch_median_by_ibm_Fhist$medianval)
rownames(all_median_by_ibm_Fhist) <- paste(ssb_median_by_ibm_Fhist$IBMlab, ssb_median_by_ibm_Fhist$Fhist, sep= "-")


# 6. by IBM* Cmult * time
ssb_mean_by_ibm_cmult_time <- ssb_sims.time %>%
  group_by(IBMlab, catch.mult, time.avg) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_cmult_time <- ssb_sims.time %>%
  group_by(IBMlab, catch.mult, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_cmult_time <- f_sims.time %>%
  group_by(IBMlab, catch.mult, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_cmult_time <- catch_sims.time %>%
  group_by(IBMlab, catch.mult, time.avg) %>%
  summarise(medianval=median(value)) 


all_median_by_ibm_Cmult_time <- cbind(SSB=ssb_median_by_ibm_cmult_time$medianval, F=f_median_by_ibm_cmult_time$medianval, Catch=catch_median_by_ibm_cmult_time$medianval)
rownames(all_median_by_ibm_Cmult_time) <- paste(ssb_median_by_ibm_cmult_time$IBMlab, ssb_median_by_ibm_cmult_time$time.avg, ssb_median_by_ibm_cmult_time$catch.mult, sep= "-")



# 7. by IBM* Fhist * time
ssb_mean_by_ibm_Fhist_time <- ssb_sims.time %>%
  group_by(IBMlab, Fhist, time.avg) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_Fhist_time <- ssb_sims.time %>%
  group_by(IBMlab, Fhist, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_Fhist_time <- f_sims.time %>%
  group_by(IBMlab, Fhist, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_Fhist_time <- catch_sims.time %>%
  group_by(IBMlab, Fhist, time.avg) %>%
  summarise(medianval=median(value)) 

all_median_by_ibm_Fhist_time <- cbind(SSB=ssb_median_by_ibm_Fhist_time$medianval, F=f_median_by_ibm_Fhist_time$medianval, Catch=catch_median_by_ibm_Fhist_time$medianval)
rownames(all_median_by_ibm_Fhist_time) <- paste(ssb_median_by_ibm_Fhist_time$IBMlab, ssb_median_by_ibm_Fhist_time$Fhist, ssb_median_by_ibm_Fhist_time$time.avg, sep= "-")


# 8. by IBM* retro * Fhist
ssb_mean_by_ibm_retro_Fhist <- ssb_sims.time %>%
  group_by(IBMlab, retro_type, Fhist) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_retro_Fhist <- ssb_sims.time %>%
  group_by(IBMlab, retro_type, Fhist) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_retro_Fhist <- f_sims.time %>%
  group_by(IBMlab, retro_type, Fhist) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_retro_Fhist <- catch_sims.time %>%
  group_by(IBMlab, retro_type, Fhist) %>%
  summarise(medianval=median(value)) 


all_median_by_ibm_retro_Fhist <- cbind(SSB=ssb_median_by_ibm_retro_Fhist$medianval, F=f_median_by_ibm_retro_Fhist$medianval, Catch=catch_median_by_ibm_retro_Fhist$medianval)
rownames(all_median_by_ibm_retro_Fhist) <- paste(ssb_median_by_ibm_retro_Fhist$IBMlab, ssb_median_by_ibm_retro_Fhist$retro_type, ssb_median_by_ibm_retro_Fhist$Fhist, sep= "-")


# 9. by IBM* retro * time
ssb_mean_by_ibm_retro_time <- ssb_sims.time %>%
  group_by(IBMlab, retro_type, time.avg) %>%
  summarise(meanval=mean(value)) 

ssb_median_by_ibm_retro_time <- ssb_sims.time %>%
  group_by(IBMlab, retro_type, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_retro_time <- f_sims.time %>%
  group_by(IBMlab, retro_type, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_retro_time <- catch_sims.time %>%
  group_by(IBMlab, retro_type, time.avg) %>%
  summarise(medianval=median(value))

all_median_by_ibm_retro_time <- cbind(SSB=ssb_median_by_ibm_retro_time$medianval, F=f_median_by_ibm_retro_time$medianval, Catch=catch_median_by_ibm_retro_time$medianval)
rownames(all_median_by_ibm_retro_time) <- paste(ssb_median_by_ibm_retro_time$IBMlab, ssb_median_by_ibm_retro_time$retro_type, ssb_median_by_ibm_retro_time$time.avg, sep= "-")


# 10. Fhist: catch.mult 
ssb_median_by_ibm_Fhist_Cmult <- ssb_sims.time %>%
  group_by(IBMlab, Fhist, catch.mult) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_Fhist_Cmult <- f_sims.time %>%
  group_by(IBMlab, Fhist, catch.mult) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_Fhist_Cmult <- catch_sims.time %>%
  group_by(IBMlab, Fhist, catch.mult) %>%
  summarise(medianval=median(value)) 


all_median_by_ibm_Fhist_Cmult <- cbind(SSB=ssb_median_by_ibm_Fhist_Cmult$medianval, F=f_median_by_ibm_Fhist_Cmult$medianval, Catch=catch_median_by_ibm_Fhist_Cmult$medianval)
rownames(all_median_by_ibm_Fhist_Cmult) <- paste(ssb_median_by_ibm_Fhist_Cmult$IBMlab, ssb_median_by_ibm_Fhist_Cmult$Fhist, ssb_median_by_ibm_Fhist_Cmult$catch.mult, sep= "-")




# --- make pdf files ====

pdf(file = "heatmaps_median_ohhh_fancier.pdf")
#  by IBM
heat2.all_med <-heatmap.2(all_median_by_ibm, scale="column", margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM")

# 1 factor by IBM
heat2.all.retro_med <- heatmap.2(all_median_by_ibm_retro, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Retro Source")
heat2.all.time_med <- heatmap.2(all_median_by_ibm_time, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Time Horizon")
heat2.all.cmult_med <- heatmap.2(all_median_by_ibm_cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by Catch Multiplier")
heat2.all.Fhist_med <- heatmap.2(all_median_by_ibm_Fhist, scale="column", margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by F history")

# 2 factors by IBM
heat2.all.time.cmult.time_med <- heatmap.2(all_median_by_ibm_Cmult_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Catch Mult and Time Horizon")
heat2.all.fhist.time_med <- heatmap.2(all_median_by_ibm_Fhist_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by F History and Time Horizon")
heat2.all.retro.fhist_med <- heatmap.2(all_median_by_ibm_retro_Fhist, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Type and F History")
heat2.all.retro.time_med <- heatmap.2(all_median_by_ibm_retro_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Type and Time Horizon")
heat2.all.Fhist.cmult_med <- heatmap.2(all_median_by_ibm_Fhist_Cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by F History and Catch Multiplier")

dev.off()


# --- make png files ====
#   png specs 
png.res <- 500
png.h <- 10
png.w <- 8

# 1
png(file="heatmap.ibm_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM")
dev.off()

# 2
png(file="heatmap.ibm.retro_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_retro, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Source")
dev.off()

# 3
png(file="heatmap.ibm.time_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Time Horizon")
dev.off()

# 4
png(file="heatmap.ibm.cmult_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Catch Multiplier")
dev.off()

# 5
png(file="heatmap.ibm.Fhist_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_Fhist, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by F history")
dev.off()

# 6
png(file="heatmap.ibm.cmult.time_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_Cmult_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Catch Mult and Time Horizon")
dev.off()


# 7
png(file="heatmap.ibm.fhist.time_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_Fhist_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by F history and Time Horizon")
dev.off()


# 8
png(file="heatmap.ibm.retro.fhist_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_retro_Fhist, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Type and F history")
dev.off()


# 9
png(file="heatmap.ibm.retro.time_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_retro_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Type and Time Horizon")
dev.off()


# 10
png(file="heatmap.ibm.fhist.cmult_median.png", height=png.h, width=png.w, units="in",
    res=png.res, type="cairo"  )
heatmap.2(all_median_by_ibm_Fhist_Cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by F history and Catch Mult")
dev.off()


# --- write csv files for table of MEAN heatmap results ====

#1 
nrows <- dim(t(all_median_by_ibm))[2]
write.csv(t(heat2.all_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm_median_normalized.csv")  #this gives table of standardized values
write.csv(all_median_by_ibm[rev(heat2.all_med$rowInd),heat2.all_med$colInd], file="table.heatmap.ibm_median.csv") #this gives table of values


# 2
nrows <- dim(t(all_median_by_ibm_retro))[2]
write.csv(t(heat2.all.retro_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.retro_median_normalized.csv")
write.csv(all_median_by_ibm_retro[rev(heat2.all.retro_med$rowInd),heat2.all.retro_med$colInd], file="table.heatmap.ibm.retro_median.csv")

# 3
nrows <- dim(t(all_median_by_ibm_time))[2]
write.csv(t(heat2.all.time_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.time_median_normalized.csv")
write.csv(all_median_by_ibm_time[rev(heat2.all.time_med$rowInd),heat2.all.time_med$colInd], file="table.heatmap.ibm.time_median.csv")

# 4
nrows <- dim(t(all_median_by_ibm_cmult))[2]
write.csv(t(heat2.all.cmult_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.cmult_median_normalized.csv")
write.csv( all_median_by_ibm_cmult[rev(heat2.all.cmult_med$rowInd),heat2.all.cmult_med$colInd], file="table.heatmap.ibm.cmult_median.csv")

# 5
nrows <- dim(t(all_median_by_ibm_Fhist))[2]
write.csv(t(heat2.all.Fhist_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.Fhist_median_normalized.csv")
write.csv(all_median_by_ibm_Fhist[rev(heat2.all.Fhist_med$rowInd),heat2.all.Fhist_med$colInd] , file="table.heatmap.ibm.Fhist_median.csv")

# 6
nrows <- dim(t(all_median_by_ibm_Cmult_time))[2]
write.csv(t(heat2.all.time.cmult.time_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.cmul.time_median_normalized.csv")
write.csv(all_median_by_ibm_Cmult_time[rev(heat2.all.time.cmult.time_med$rowInd),heat2.all.time.cmult.time_med$colInd] , file="table.heatmap.ibm.cmul.time_median.csv")

# 7
nrows <- dim(t(all_median_by_ibm_Fhist_time))[2]
write.csv(t(heat2.all.fhist.time_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.fhist.time_median_normalized.csv")
write.csv( all_median_by_ibm_Fhist_time[rev(heat2.all.fhist.time_med$rowInd),heat2.all.fhist.time_med$colInd], file="table.heatmap.ibm.fhist.time_median.csv")

# 8
nrows <- dim(t(all_median_by_ibm_retro_Fhist))[2]
write.csv(t(heat2.all.retro.fhist_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.retro.fhist_median_normalized.csv")
write.csv(all_median_by_ibm_retro_Fhist[rev(heat2.all.retro.fhist_med$rowInd),heat2.all.retro.fhist_med$colInd] , file="table.heatmap.ibm.retro.fhist_median.csv")

# 9
nrows <- dim(t(all_median_by_ibm_retro_time))[2]
write.csv(t(heat2.all.retro.time_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.retro.time_median_normalized.csv")
write.csv(all_median_by_ibm_retro_time[rev(heat2.all.retro.time_med$rowInd),heat2.all.retro.time_med$colInd] , file="table.heatmap.ibm.retro.time_median.csv")

# 10
nrows <- dim(t(all_median_by_ibm_Fhist_Cmult))[2]
write.csv(t(heat2.all.Fhist.cmult_med$carpet)[rev(seq(1,nrows)),], file="table.heatmap.ibm.fhist.cmult_median_normalized.csv")
write.csv(all_median_by_ibm_Fhist_Cmult[rev(heat2.all.Fhist.cmult_med$rowInd),heat2.all.Fhist.cmult_med$colInd]  , file="table.heatmap.ibm.fhist.cmult_median.csv")



##--- End of Heatmap Stuff (Medians) ====

