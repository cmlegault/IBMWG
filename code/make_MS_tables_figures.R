# make_MS_tables_figures.R
# create tables and figures for primary literature publication
# uses cleaned performance metrics from base, full SCAA, and no retro  
# output goes to Manuscript/tables_figs directory



#library(vctrs, lib.loc="C:/R/R-3.6.1/library") # (liz needs this line)
library(tidyverse)
library(aplpack)
library(pracma)
library(car)
library(gplots)
#install.packages('Cairo')
library('Cairo')

# get reference points
# note: we used year 50 for reporting refpts in WG report
# to get alternative refpts using year 1, multiply by ratio reftpt50/refpt1
refpts <- read.csv("demonstrations/chris/base_sims/refpts.csv", 
                   stringsAsFactors = FALSE) %>%
  mutate(ssbmsyratio = ssbmsy50 / ssbmsy1,
         fmsyratio = fmsy50 / fmsy1,
         msyratio = msy50 / msy1) %>%
  mutate(Fhist = ifelse(Fhist == "F", 1, 2))

# get resuls and simulation set up information
mse_results <- readRDS("results/perform-metrics_clean_full_DLM.rds")
scaa_results <- readRDS("results/perform-metrics_full_scaa.rds") # note full
mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")

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
    IBM == "planBsmoothfxn" ~ "Islope",
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
  filter(IBMlab == "Ensemble") %>%
  mutate(IBM = "SCAA",
         IBMlab = "SCAA")

# counting scenarios and simulations
countIBM <- defined %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

countIBM_scaa <- defined_scaa %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

nscentab <- rbind(countIBM, countIBM_scaa) %>%
  select(IBMlab, Scenlab, nscenarios) %>%
  pivot_wider(names_from = Scenlab, values_from = nscenarios)
nscentab

nsimtab <- rbind(countIBM, countIBM_scaa) %>%
  select(IBMlab, Scenlab, nsim) %>%
  pivot_wider(names_from = Scenlab, values_from = nsim)
nsimtab
#write.csv(nsimtab, file = "Manuscript/tables_figs/nsimtab.csv", row.names = FALSE)

nsim_plot <- ggplot(rbind(countIBM, countIBM_scaa), 
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

### compute median for all metrics for each scenario
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

ssb_median_by_scenario_scaa <- ssb_results_scaa %>%
  group_by(iscen, metric) %>%
  summarise_all(median) %>%
  inner_join(defined_scaa, by = "iscen")

f_median_by_scenario_scaa <- f_results_scaa %>%
  group_by(iscen, metric) %>%
  summarise_all(median) %>%
  inner_join(defined_scaa, by = "iscen")

catch_median_by_scenario_scaa <- catch_results_scaa %>%
  group_by(iscen, metric) %>%
  summarise_all(median) %>%
  inner_join(defined_scaa, by = "iscen")

# combine scaa and base scenarios
ssb_mean_by_scenario <- rbind(ssb_mean_by_scenario, ssb_mean_by_scenario_scaa)

f_mean_by_scenario <- rbind(f_mean_by_scenario, f_mean_by_scenario_scaa)

catch_mean_by_scenario <- rbind(catch_mean_by_scenario, catch_mean_by_scenario_scaa)

ssb_median_by_scenario <- rbind(ssb_median_by_scenario, ssb_median_by_scenario_scaa)

f_median_by_scenario <- rbind(f_median_by_scenario, f_median_by_scenario_scaa)

catch_median_by_scenario <- rbind(catch_median_by_scenario, catch_median_by_scenario_scaa)

### save mean and median by_scenario results for easier modeling and ranking
saveRDS(ssb_mean_by_scenario, 
        file = "Manuscript/tables_figs/ssb_mean_by_scenario.rds")

saveRDS(f_mean_by_scenario,
        file = "Manuscript/tables_figs/f_mean_by_scenario.rds")

saveRDS(catch_mean_by_scenario, 
        file = "Manuscript/tables_figs/catch_mean_by_scenario.rds")

saveRDS(ssb_median_by_scenario, 
        file = "Manuscript/tables_figs/ssb_median_by_scenario.rds")

saveRDS(f_median_by_scenario,
        file = "Manuscript/tables_figs/f_median_by_scenario.rds")

saveRDS(catch_median_by_scenario, 
        file = "Manuscript/tables_figs/catch_median_by_scenario.rds")

### trade-off plots
make_td_plot <- function(mytib, myxlab, myylab, mytitle){
  myplot <- ggplot(mytib, aes(x=x_value, y=y_value, color=retro_type)) +
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

ssb_temp_med <- ssb_median_by_scenario %>%
  mutate(metric = paste0("ssb_", metric))
f_temp_med <- f_median_by_scenario %>%
  mutate(metric = paste0("f_", metric))
catch_temp_med <- catch_median_by_scenario %>%
  mutate(metric = paste0("catch_", metric))
td_med <- rbind(ssb_temp_med, f_temp_med, catch_temp_med)

# td1_l <- td %>%
#   filter(metric %in% c("ssb_l_is_ge_bmsy", "catch_l_avg_catch_msy")) %>%
#   distinct() %>%
#   pivot_wider(names_from = metric, values_from = value) %>%
#   rename(x_value = ssb_l_is_ge_bmsy, y_value = catch_l_avg_catch_msy)
# 
# td1_s <- td %>%
#   filter(metric %in% c("ssb_s_is_ge_bmsy", "catch_s_avg_catch_msy")) %>%
#   distinct() %>%
#   pivot_wider(names_from = metric, values_from = value) %>%
#   rename(x_value = ssb_s_is_ge_bmsy, y_value = catch_s_avg_catch_msy)
# 
# td1_l_plot <- make_td_plot(td1_l, "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Long Term")
# 
# td1_s_plot <- make_td_plot(td1_s, "Prob(SSB>=SSBmsy)", "Mean(Catch/MSY)", "Short Term")
# 
# td2_l <- td %>%
#   filter(metric %in% c("ssb_l_is_less_05_bmsy", "f_l_is_gr_fmsy")) %>%
#   distinct() %>%
#   pivot_wider(names_from = metric, values_from = value) %>%
#   rename(x_value = ssb_l_is_less_05_bmsy, y_value = f_l_is_gr_fmsy)
# 
# td2_s <- td %>%
#   filter(metric %in% c("ssb_s_is_less_05_bmsy", "f_s_is_gr_fmsy")) %>%
#   distinct() %>%
#   pivot_wider(names_from = metric, values_from = value) %>%
#   rename(x_value = ssb_s_is_less_05_bmsy, y_value = f_s_is_gr_fmsy)
# 
# td2_l_plot <- make_td_plot(td2_l, "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Long Term")
# 
# td2_s_plot <- make_td_plot(td2_s, "Prob(SSB<0.5SSBmsy)", "Prob(F>Fmsy)", "Short Term")

td3_l <- td %>%
  filter(metric %in% c("ssb_l_avg_ssb_ssbmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_avg_ssb_ssbmsy, y_value = catch_l_avg_catch_msy)

td3_l_alt <- td3_l %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(x_value = x_value * ssbmsyratio, y_value = y_value * msyratio)

# td3_s <- td %>%
#   filter(metric %in% c("ssb_s_avg_ssb_ssbmsy", "catch_s_avg_catch_msy")) %>%
#   distinct() %>%
#   pivot_wider(names_from = metric, values_from = value) %>%
#   rename(x_value = ssb_s_avg_ssb_ssbmsy, y_value = catch_s_avg_catch_msy)

td3_l_plot <- make_td_plot(td3_l, "SSB/SSBmsy", "Catch/MSY", "Long Term (means)")

td3_l_alt_plot <- make_td_plot(td3_l_alt, "SSB/SSBmsy", "Catch/MSY", "Long Term (means) Alternative RefPts")

# td3_s_plot <- make_td_plot(td3_s, "SSB/SSBmsy", "Catch/MSY", "Short Term")

td3_l_med <- td_med %>%
  filter(metric %in% c("ssb_l_avg_ssb_ssbmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_avg_ssb_ssbmsy, y_value = catch_l_avg_catch_msy)

td3_l_med_plot <- make_td_plot(td3_l_med, "SSB/SSBmsy", "Catch/MSY", "Long Term (medians)")

td3_l_med_alt <- td3_l_med %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(x_value = x_value * ssbmsyratio, y_value = y_value * msyratio)

td3_l_med_alt_plot <- make_td_plot(td3_l_med_alt, "SSB/SSBmsy", "Catch/MSY", "Long Term (medians) Alternative RefPts")

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

ssb_sims_scaa <- ssb_results_scaa %>%
  filter(grepl("avg_ssb_ssbmsy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

f_sims_scaa <- f_results_scaa %>%
  filter(grepl("avg_f_fmsy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

catch_sims_scaa <- catch_results_scaa %>%
  filter(grepl("avg_catch_msy", metric)) %>%
  inner_join(defined_scaa, by = c("iscen"))

sims <- rbind(ssb_sims_scaa, f_sims_scaa, catch_sims_scaa,
              ssb_sims, f_sims, catch_sims) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value)

myscenlabs <- sort(unique(sims$Scenlab))
nscenlabs <- length(myscenlabs)
myibmlabs <- sort(unique(sims$IBMlab))
nibmlabs <- length(myibmlabs)

mysmax_l <- max(sims$l_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_l <- max(sims$l_avg_f_fmsy, na.rm = TRUE)
mycmax_l <- max(sims$l_avg_catch_msy, na.rm = TRUE)
mysmax_s <- max(sims$s_avg_ssb_ssbmsy, na.rm = TRUE)
myfmax_s <- max(sims$s_avg_f_fmsy, na.rm = TRUE)
mycmax_s <- max(sims$s_avg_catch_msy, na.rm = TRUE)

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

# compare metrics plot
# bb controls whether largest mean value at top (TRUE) or smallest (FALSE)
compare_all_plot <- function(mytib, myxlab, mytitle, bb){
  mysum <- mytib %>%
    group_by(IBMlab) %>%
    summarize(meanval = mean(value)) %>%
    inner_join(mytib, by = "IBMlab")
  if (bb == FALSE){ 
    mysum$meanval = -1.0 * mysum$meanval
  }
  myplot <- ggplot(mysum, aes(x=value, y=reorder(IBMlab, meanval), color=factor(catch.mult))) +
    geom_point() +
    geom_vline(xintercept = 1, linetype = "dashed") +
    facet_grid(retro_type~Fhistlab[Fhist]+n_selblocks) +
    labs(x=myxlab, y="", color="Catch Mult", title=mytitle) +
    theme_bw()
  return(myplot)
}

ssb_ssbmsy_l <- compare_all_plot(filter(ssb_mean_by_scenario, 
                                        metric == "l_avg_ssb_ssbmsy"), 
                                 "SSB/SSBmsy", "Long Term (means)", TRUE)

f_fmsy_l <- compare_all_plot(filter(f_mean_by_scenario, 
                                    metric == "l_avg_f_fmsy"), 
                             "F/Fmsy", "Long Term (means)", FALSE)

catch_msy_l <- compare_all_plot(filter(catch_mean_by_scenario, 
                                       metric == "l_avg_catch_msy"), 
                                "Catch/MSY", "Long Term (means)", TRUE)

ssb_alt_tmp <- ssb_mean_by_scenario %>%
  filter(metric == "l_avg_ssb_ssbmsy") %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(value = value * ssbmsyratio)

f_alt_tmp <- f_mean_by_scenario %>%
  filter(metric == "l_avg_f_fmsy") %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(value = value * fmsyratio)

catch_alt_tmp <- catch_mean_by_scenario %>%
  filter(metric == "l_avg_catch_msy") %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(value = value * msyratio)

ssb_ssbmsy_l_alt <- compare_all_plot(ssb_alt_tmp, 
                                 "SSB/SSBmsy", 
                                 "Long Term (means) Alternative RefPts", TRUE)

f_fmsy_l_alt <- compare_all_plot(f_alt_tmp, 
                             "F/Fmsy", 
                             "Long Term (means) Alternative RefPts", FALSE)

catch_msy_l_alt <- compare_all_plot(catch_alt_tmp, 
                                "Catch/MSY", 
                                "Long Term (means) Alternative Refpts", TRUE)

catch_iav_a <- compare_all_plot(filter(catch_mean_by_scenario,
                                       metric == "a_iav_catch"),
                                "IAV Catch", "All Years (means)", TRUE)
catch_iav_a$layers[[2]] <- NULL # fudge to remove vertical line at 1.0

# ssb_ssbmsy_s <- compare_all_plot(filter(ssb_mean_by_scenario, 
#                                         metric == "s_avg_ssb_ssbmsy"), 
#                                  "SSB/SSBmsy", "Short Term", TRUE)
# 
# f_fmsy_s <- compare_all_plot(filter(f_mean_by_scenario, 
#                                     metric == "s_avg_f_fmsy"), 
#                              "F/Fmsy", "Short Term", FALSE)
# 
# catch_msy_s <- compare_all_plot(filter(catch_mean_by_scenario, 
#                                        metric == "s_avg_catch_msy"), 
#                                 "Catch/MSY", "Short Term", TRUE)

ssb_ssbmsy_l_med <- compare_all_plot(filter(ssb_median_by_scenario, 
                                            metric == "l_avg_ssb_ssbmsy"), 
                                     "SSB/SSBmsy", "Long Term (medians)", TRUE)

f_fmsy_l_med <- compare_all_plot(filter(f_median_by_scenario, 
                                        metric == "l_avg_f_fmsy"), 
                                 "F/Fmsy", "Long Term (medians)", FALSE)

catch_msy_l_med <- compare_all_plot(filter(catch_median_by_scenario, 
                                           metric == "l_avg_catch_msy"), 
                                    "Catch/MSY", "Long Term (medians)", TRUE)

ssb_alt_tmp_med <- ssb_median_by_scenario %>%
  filter(metric == "l_avg_ssb_ssbmsy") %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(value = value * ssbmsyratio)

f_alt_tmp_med <- f_median_by_scenario %>%
  filter(metric == "l_avg_f_fmsy") %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(value = value * fmsyratio)

catch_alt_tmp_med <- catch_median_by_scenario %>%
  filter(metric == "l_avg_catch_msy") %>%
  left_join(refpts, by = c("retro_type", "Fhist", "n_selblocks")) %>%
  mutate(value = value * msyratio)

ssb_ssbmsy_l_alt_med <- compare_all_plot(ssb_alt_tmp_med, 
                                     "SSB/SSBmsy", 
                                     "Long Term (medians) Alternative RefPts", TRUE)

f_fmsy_l_alt_med <- compare_all_plot(f_alt_tmp_med, 
                                 "F/Fmsy", 
                                 "Long Term (medians) Alternative RefPts", FALSE)

catch_msy_l_alt_med <- compare_all_plot(catch_alt_tmp_med, 
                                    "Catch/MSY", 
                                    "Long Term (medians) Alternative Refpts", TRUE)

catch_iav_a_med <- compare_all_plot(filter(catch_median_by_scenario,
                                       metric == "a_iav_catch"),
                                "IAV Catch", "All Years (medians)", TRUE)
catch_iav_a_med$layers[[2]] <- NULL # fudge to remove vertical line at 1.0

# probabilty and nyears overfished overfishing 
# note: using alternative refpts requires going back to original data and computing whether or not each year is above/below the alternative refpt
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
    summarise(meanval = mean(value), medianval = median(value)) %>%
    mutate(sdc = "Overfished")
  
  nyrs_overfishing <- filter(myftib,
                             metric %in% c("l_n_gr_fmsy", "s_n_gr_fmsy")) %>%
    mutate(period = ifelse(substr(metric, 1, 1) == "l", "Long Term", "Short Term")) %>%
    group_by(IBMlab, period) %>%
    summarise(meanval = mean(value), medianval = median(value)) %>%
    mutate(sdc = "Overfishing")
  
  nyrs_status <- rbind(nyrs_overfished, nyrs_overfishing)

  mystatus <- list(prob_status = prob_status,
                   nyrs_status = nyrs_status)

  return(mystatus)
}

make_status_plot <- function(mytib, myxlab, mygridscales){
  myplot <- ggplot(filter(mytib, period == "Long Term"), aes(x=meanval, y=IBMlab)) +
    geom_point() +
    facet_grid(~sdc, scales = mygridscales) +
    expand_limits(x=c(0, 1)) +
    labs(x=myxlab, y="", title = "(Long Term means)") +
    theme_bw()
  return(myplot)
}

mystatus <- get_status_data(ssb_mean_by_scenario, f_mean_by_scenario)

prob_status_plot <- make_status_plot(mystatus[[1]], "Probability", "fixed")
#nyrs_status_plot <- make_status_plot(mystatus[[2]], "Number of Years", "free_x")

### put plots into pdf

outfile <- "Manuscript/tables_figs/tables_figures.pdf"

pdf(file = outfile)

#print(nsim_plot)
#print(scenlab_plot)

# print(td1_l_plot)
# print(td1_s_plot)
# print(td2_l_plot)
# print(td2_s_plot)
print(td3_l_plot)
print(td3_l_med_plot)
print(td3_l_alt_plot)
print(td3_l_med_alt_plot)
# print(td3_s_plot)

print(ssb_ssbmsy_l)
print(ssb_ssbmsy_l_med)
print(ssb_ssbmsy_l_alt)
print(ssb_ssbmsy_l_alt_med)
print(f_fmsy_l) 
print(f_fmsy_l_med) 
print(f_fmsy_l_alt)
print(f_fmsy_l_alt_med)
print(catch_msy_l)
print(catch_msy_l_med)
print(catch_msy_l_alt)
print(catch_msy_l_alt_med)
print(catch_iav_a)
print(catch_iav_a_med)
# print(ssb_ssbmsy_s)
# print(f_fmsy_s) 
# print(catch_msy_s) 

print(prob_status_plot)
# print(nyrs_status_plot)

# uncomment the walk lines to see all the plots
# walk(td4_l_IBM_plot, print)
# walk(td4_s_IBM_plot, print)
# walk(td4_l_Scen_plot, print)
# walk(td4_s_Scen_plot, print)
print(td4_l_IBM_plot[[1]]) # this shows just one example

dev.off()


## make ecdf plots to show distributions of results (LIZ)  ====

color_order <- c("Catch.F1.Cmult1.Sel1", "Catch.F2.Cmult1.Sel1", "Catch.F1.Cmult0.75.Sel1", "Catch.F2.Cmult0.75.Sel1",
                 "Catch.F1.Cmult1.Sel2", "Catch.F2.Cmult1.Sel2", "Catch.F1.Cmult0.75.Sel2", "Catch.F2.Cmult0.75.Sel2", 
                 "M.F1.Cmult1.Sel1", "M.F2.Cmult1.Sel1", "M.F1.Cmult0.75.Sel1", "M.F2.Cmult0.75.Sel1", 
                 "M.F1.Cmult1.Sel2", "M.F2.Cmult1.Sel2", "M.F1.Cmult0.75.Sel2", "M.F2.Cmult0.75.Sel2" )
color_vector <- c( "#00AA11" , "#00AA11" , "#aacc00", "#aacc00", 
                   "#0055ee", "#0055ee",  "#00ddff",  "#00ddff", 
                   "#CC1100", "#CC1100", "#CC7722", "#CC7722", 
                   "#BB11AA", "#BB11AA", "#FF88CC" , "#FF88CC"  )

linetype_vector <- c( "solid" , "dashed" ,   "solid" , "dashed" ,   "solid" , "dashed" ,   "solid" , "dashed" ,
                     "solid", "dashed",   "solid", "dashed",   "solid", "dashed",   "solid", "dashed" )
linetype_order <- c("1", "2")


#  create Scenario labels for the legend and color & linetype info
sims.time <- sims %>% 
  mutate(Scenario = factor(paste0(retro_type, ".", "F", Fhist, ".", "Cmult", catch.mult, ".", "Sel", n_selblocks),  levels=color_order) ) %>%
  mutate(lty = factor(if_else(Fhist==1, 1, 2)) ) %>%
  mutate(lcol = case_when(
    Scenario=="Catch.F1.Cmult1.Sel1" ~ "#00AA11" ,
    Scenario=="Catch.F2.Cmult1.Sel1" ~ "#00AA12",
    Scenario=="M.F1.Cmult1.Sel1" ~ "#0055CC" ,
    Scenario=="M.F2.Cmult1.Sel1" ~ "#0055CD" ,

    Scenario=="Catch.F1.Cmult0.75.Sel1" ~ "#addd8e" ,
    Scenario=="Catch.F2.Cmult0.75.Sel1" ~ "#addd8f",
    Scenario=="M.F1.Cmult0.75.Sel1" ~ "#00BBCC" ,
    Scenario=="M.F2.Cmult0.75.Sel1" ~ "#00BBCD" ,

    Scenario=="Catch.F1.Cmult1.Sel2" ~ "#CC1100" ,
    Scenario=="Catch.F2.Cmult1.Sel2" ~ "#CC1101",
    Scenario=="M.F1.Cmult1.Sel2" ~ "#BB11AA" ,
    Scenario=="M.F2.Cmult1.Sel2" ~ "#BB11AB" ,

    Scenario=="Catch.F1.Cmult0.75.Sel2" ~ "#CC9900" ,
    Scenario=="Catch.F2.Cmult0.75.Sel2" ~ "#CC9901",
    Scenario=="M.F1.Cmult0.75.Sel2" ~ "#FF88CC" ,
    Scenario=="M.F2.Cmult0.75.Sel2" ~ "#FF88CD" ,



  ))



## function to make ecdf plot ====

make.ecdf.plot <- function(df, xlim1,  xlim2, xtxt, ytxt, plot.title)
{
  this.plot <- ggplot(data=df, 
                      aes(x=xval, col=Scenario, linetype = Scenario )) +
    facet_wrap(~IBMlab, ncol=2, dir='v') +
    # set up some reference lines and areas ====
    geom_rect(aes(xmin = xlim1, xmax = xlim2, ymin = 0, ymax = 0.5),
              fill = "#ddddddaa",
              inherit.aes = FALSE, linetype=0) +
    geom_vline(xintercept=0.5, linetype="dashed", 
               color = "black", size=1) +
    geom_vline(xintercept=1.0, 
               color = "black", size=1) +
    # create ecdf ====
    stat_ecdf(data=df, 
              aes(x=xval, col=Scenario, linetype = Scenario )) +
    # set theme specs ====
    theme_light() +
    theme(
      strip.background = element_rect(
        color="grey60", fill="#dddddd", size=1.5, linetype="solid"
      ),
      strip.text.x = element_text(
        size = 9, color = "black")
    )  +
    scale_color_manual(name = "Scenario", 
                       values=color_vector) + 
    scale_linetype_manual(name = "Scenario", 
                          values = linetype_vector ) +
    # axis and title formatting ====
    #xlim(c(xlim1, xlim2)) +
    coord_cartesian(xlim = c(xlim1, xlim2) ) +
    ylab(ytxt) +
    xlab(xtxt) +
    ggtitle(plot.title)

  return(this.plot)
}


sims.ssb.short <- sims.time %>% 
  mutate(xval=s_avg_ssb_ssbmsy)
sims.ssb.long <- sims.time %>% 
  mutate(xval=l_avg_ssb_ssbmsy)
sims.f.short <- sims.time %>% 
  mutate(xval=s_avg_f_fmsy)
sims.f.long <- sims.time %>% 
  mutate(xval=l_avg_f_fmsy)
sims.catch.short <- sims.time %>% 
  mutate(xval=s_avg_catch_msy)
sims.catch.long <- sims.time %>% 
  mutate(xval=l_avg_catch_msy)

ssb.short.plot <- make.ecdf.plot(df=sims.ssb.short ,  xlim1=0, xlim2=3, xtxt = "SSB / SSBmsy (short term average)", ytxt= "Probability <= SSB / SSBmsy" , plot.title="")

ssb.long.plot <- make.ecdf.plot(df=sims.ssb.long ,  xlim1=0, xlim2=3, xtxt = "SSB / SSBmsy (long term average)", ytxt= "Probability <= SSB / SSBmsy", plot.title="" )

f.short.plot <- make.ecdf.plot(df=sims.f.short ,  xlim1=0, xlim2=3, xtxt = "F / Fmsy (short term average)", ytxt= "Probability <= F / Fmsy" , plot.title="")

f.long.plot <- make.ecdf.plot(df=sims.f.long ,  xlim1=0, xlim2=3, xtxt = "F / Fmsy (long term average)", ytxt= "Probability <= F / Fmsy" , plot.title="" )

catch.short.plot <- make.ecdf.plot(df=sims.catch.short ,  xlim1=0, xlim2=3, xtxt = "Catch / MSY (short term average)", ytxt= "Probability <= Catch / MSY" , plot.title="" )

catch.long.plot <- make.ecdf.plot(df=sims.catch.long ,  xlim1=0, xlim2=3, xtxt = "Catch / MSY (long term average)", ytxt= "Probability <= Catch / MSY" , plot.title="" )



# # create a single pdf of the ecdf plots 
## !! note the lines in this pdf have heavy aliasing and look crappy-- any suggestions for anti-aliasing pdfs?

# outfile2 <- "Manuscript/tables_figs/tables_figures_ECDF_cairo.pdf"
# 
# #pdf(file = outfile2, height=11, width=9, dpi = 300, type = 'cairo')
# grDevices::cairo_pdf(onefile=T, filename = outfile2, height=11, width=9, antialias="gray")  #  antialias="subpixel"
# 
# print(ssb.short.plot)
# print(ssb.long.plot)
# print(f.short.plot)
# print(f.long.plot)
# print(catch.short.plot)
# print(catch.long.plot)
# 
# dev.off()

# saving as png because the pdf lines look like crap 
ggsave(ssb.short.plot, file="Manuscript/tables_figs/ssb.short.ecdf.cairo.png", dpi = 300, type="cairo", height=11, width=9, units="in") 
ggsave(ssb.long.plot, file="Manuscript/tables_figs/ssb.long.ecdf.cairo.png", dpi = 300, type="cairo", height=11, width=9) 
ggsave(f.short.plot, file="Manuscript/tables_figs/f.short.ecdf.cairo.png", dpi = 300, type="cairo", height=11, width=9) 
ggsave(f.long.plot, file="Manuscript/tables_figs/f.long.ecdf.cairo.png", dpi = 300, type="cairo", height=11, width=9) 
ggsave(catch.short.plot, file="Manuscript/tables_figs/catch.short.ecdf.cairo.png", dpi = 300, type="cairo", height=11, width=9) 
ggsave(catch.long.plot, file="Manuscript/tables_figs/catch.long.ecdf.cairo.png", dpi = 300, type="cairo", height=11, width=9) 


##--- HEATMAPS ====
## add heatmap plots and save csv files in Manuscript/tables_figs/heatmap directory

# make time variable for use in heatmaps
ssb_sims.time <- rbind(ssb_sims, ssb_sims_scaa) %>% 
  select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult, metric) %>%
  mutate(time.avg = ifelse(substr(metric,1,1)=="l", "L", "S"))

f_sims.time <- rbind(f_sims, f_sims_scaa) %>% 
  select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult, metric) %>%
  mutate(time.avg = ifelse(substr(metric,1,1)=="l", "L", "S"))

catch_sims.time <- rbind(catch_sims, catch_sims_scaa) %>% 
  select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult, metric) %>%
  mutate(time.avg = ifelse(substr(metric,1,1)=="l", "L", "S"))

heat.cols <- c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')  # 9 colors

# 1. by IBM
ssb_median_by_ibm <- ssb_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm <- f_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm <- catch_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value))
all_median_by_ibm <- cbind(SSB=ssb_median_by_ibm$medianval, F=f_median_by_ibm$medianval, Catch=catch_median_by_ibm$medianval)
rownames(all_median_by_ibm) <- ssb_median_by_ibm$IBMlab


# 2. by IBM*retro type
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

# function to make the png
make_heatmap_png <- function(myfile,
                             mydata,
                             mymain,
                             mydir="Manuscript/tables_figs/heatmap/",
                             png.h = 10,
                             png.w = 8,
                             myunits = "in",
                             png.res = 500,
                             mytype = "cairo",
                             mycols = heat.cols){
  
  png(file=paste0(mydir,myfile), height=png.h, width=png.w, units=myunits,
      res=png.res, type=mytype)
  
  heatmap.2(mydata, scale="column", margins=c(7,10), keysize = 1, col=mycols, main = mymain)
  
  dev.off()
}

# 1
make_heatmap_png(myfile = "heatmap.ibm_median.png",
                 mydata = all_median_by_ibm,
                 mymain = "IBM")
# 2
make_heatmap_png(myfile = "heatmap.ibm.retro_median.png",
                 mydata = all_median_by_ibm_retro,
                 mymain = "IBM by Retro Source")
# 3
make_heatmap_png(myfile = "heatmap.ibm.time_median.png",
                 mydata = all_median_by_ibm_time,
                 mymain = "IBM by Time Horizon")
# 4
make_heatmap_png(myfile = "heatmap.ibm.cmult_median.png",
                 mydata = all_median_by_ibm_cmult,
                 mymain = "IBM by Catch Multiplier")
# 5
make_heatmap_png(myfile = "heatmap.ibm.Fhist_median.png",
                 mydata = all_median_by_ibm_Fhist,
                 mymain = "IBM by F history")
# 6
make_heatmap_png(myfile = "heatmap.ibm.cmult.time_median.png",
                 mydata = all_median_by_ibm_Cmult_time,
                 mymain = "IBM by Catch Mult and Time Horizon")
# 7
make_heatmap_png(myfile = "heatmap.ibm.fhist.time_median.png",
                 mydata = all_median_by_ibm_Fhist_time,
                 mymain = "IBM by F history and Time Horizon")
# 8
make_heatmap_png(myfile = "heatmap.ibm.retro.fhist_median.png",
                 mydata = all_median_by_ibm_retro_Fhist,
                 mymain = "IBM by Retro Type and F history")
# 9
make_heatmap_png(myfile = "heatmap.ibm.retro.time_median.png",
                 mydata = all_median_by_ibm_retro_time,
                 mymain = "IBM by Retro Type and Time Horizon")
# 10
make_heatmap_png(myfile = "heatmap.ibm.fhist.cmult_median.png",
                 mydata = all_median_by_ibm_Fhist_Cmult,
                 mymain = "IBM by F history and Catch Mult")



# --- write csv files for table of MEAN heatmap results ====

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


#1 
nrows <- dim(t(all_median_by_ibm))[2]
#write.csv(t(heat2.all_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm_median_normalized.csv")  #this gives table of standardized values
write.csv(all_median_by_ibm[rev(heat2.all_med$rowInd),heat2.all_med$colInd], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm_median.csv") #this gives table of values


# 2
nrows <- dim(t(all_median_by_ibm_retro))[2]
#write.csv(t(heat2.all.retro_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.retro_median_normalized.csv")
write.csv(all_median_by_ibm_retro[rev(heat2.all.retro_med$rowInd),heat2.all.retro_med$colInd], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.retro_median.csv")

# 3
nrows <- dim(t(all_median_by_ibm_time))[2]
#write.csv(t(heat2.all.time_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.time_median_normalized.csv")
write.csv(all_median_by_ibm_time[rev(heat2.all.time_med$rowInd),heat2.all.time_med$colInd], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.time_median.csv")

# 4
nrows <- dim(t(all_median_by_ibm_cmult))[2]
#write.csv(t(heat2.all.cmult_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.cmult_median_normalized.csv")
write.csv( all_median_by_ibm_cmult[rev(heat2.all.cmult_med$rowInd),heat2.all.cmult_med$colInd], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.cmult_median.csv")

# 5
nrows <- dim(t(all_median_by_ibm_Fhist))[2]
#write.csv(t(heat2.all.Fhist_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.Fhist_median_normalized.csv")
write.csv(all_median_by_ibm_Fhist[rev(heat2.all.Fhist_med$rowInd),heat2.all.Fhist_med$colInd] , file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.Fhist_median.csv")

# 6
nrows <- dim(t(all_median_by_ibm_Cmult_time))[2]
#write.csv(t(heat2.all.time.cmult.time_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.cmul.time_median_normalized.csv")
write.csv(all_median_by_ibm_Cmult_time[rev(heat2.all.time.cmult.time_med$rowInd),heat2.all.time.cmult.time_med$colInd] , file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.cmul.time_median.csv")

# 7
nrows <- dim(t(all_median_by_ibm_Fhist_time))[2]
#write.csv(t(heat2.all.fhist.time_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.fhist.time_median_normalized.csv")
write.csv( all_median_by_ibm_Fhist_time[rev(heat2.all.fhist.time_med$rowInd),heat2.all.fhist.time_med$colInd], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.fhist.time_median.csv")

# 8
nrows <- dim(t(all_median_by_ibm_retro_Fhist))[2]
#write.csv(t(heat2.all.retro.fhist_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.retro.fhist_median_normalized.csv")
write.csv(all_median_by_ibm_retro_Fhist[rev(heat2.all.retro.fhist_med$rowInd),heat2.all.retro.fhist_med$colInd] , file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.retro.fhist_median.csv")

# 9
nrows <- dim(t(all_median_by_ibm_retro_time))[2]
#write.csv(t(heat2.all.retro.time_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.retro.time_median_normalized.csv")
write.csv(all_median_by_ibm_retro_time[rev(heat2.all.retro.time_med$rowInd),heat2.all.retro.time_med$colInd] , file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.retro.time_median.csv")

# 10
nrows <- dim(t(all_median_by_ibm_Fhist_Cmult))[2]
#write.csv(t(heat2.all.Fhist.cmult_med$carpet)[rev(seq(1,nrows)),], file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.fhist.cmult_median_normalized.csv")
write.csv(all_median_by_ibm_Fhist_Cmult[rev(heat2.all.Fhist.cmult_med$rowInd),heat2.all.Fhist.cmult_med$colInd]  , file="Manuscript/tables_figs/heatmap/table.heatmap.ibm.fhist.cmult_median.csv")



##--- End of Heatmap Stuff (Medians) ====

## also summarize probabilities of overfished and overfishing as well as interannual variability in catch (across full feedback period, so cannot break out by time period)
# ped = probability overfished
# ping = probability overfishing
# replace probabilities with proportion of years
# proped = number of years overfished
# proping = number of years overfishing

# ped_sims <- ssb_results %>%
#   filter(metric %in% c("l_is_less_05_bmsy", 
#                        "s_is_less_05_bmsy")) %>%
#   inner_join(defined, by = c("iscen"))

proped_sims <- ssb_results %>%
  filter(metric %in% c("l_n_less_05_bmsy", 
                       "s_n_less_05_bmsy")) %>%
  inner_join(defined, by = c("iscen"))

proped_sims_scaa <- ssb_results_scaa %>%
  filter(metric %in% c("l_n_less_05_bmsy", 
                       "s_n_less_05_bmsy")) %>%
  inner_join(defined_scaa, by = c("iscen"))

proped_sims.time <- rbind(proped_sims, proped_sims_scaa) %>% 
  select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult, metric) %>%
  mutate(time.avg = ifelse(substr(metric,1,1)=="l", "L", "S"))

proping_sims <- f_results %>%
  filter(metric %in% c("l_n_gr_fmsy", "s_n_gr_fmsy")) %>%
  inner_join(defined, by = c("iscen"))

proping_sims_scaa <- f_results_scaa %>%
  filter(metric %in% c("l_n_gr_fmsy", "s_n_gr_fmsy")) %>%
  inner_join(defined_scaa, by = c("iscen"))

proping_sims.time <- rbind(proping_sims, proping_sims_scaa) %>% 
  select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult, metric) %>%
  mutate(time.avg = ifelse(substr(metric,1,1)=="l", "L", "S"))

# convert number of years to proportions (6 years short, 20 years long)
proped_sims.time <- proped_sims.time %>%
  mutate(value = ifelse(time.avg == "S", value/6, value/20))
proping_sims.time <- proping_sims.time %>%
  mutate(value = ifelse(time.avg == "S", value/6, value/20))

iav_sims <- catch_results %>%
  filter(metric == "a_iav_catch") %>%
  inner_join(defined, by = c("iscen"))

iav_sims_scaa <- catch_results_scaa %>%
  filter(metric == "a_iav_catch") %>%
  inner_join(defined_scaa, by = c("iscen"))

iav_sims.time <- rbind(iav_sims, iav_sims_scaa) %>% 
  select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult, metric) %>%
  mutate(time.avg = "A") # note using full time series for IAV

# function to handle summarizing by user supplied variable(s)
my.summarize <- function(.data, ...){
  .data %>%
    group_by(...) %>%
    summarize(meanval=mean(value), medianval=median(value))
}

# wrapper function to get all three variables
wrap3 <- function(proped_sims.time, proping_sims.time, iav_sims.time, ...){
  
  # ped_by_group <- my.summarize(ped_sims.time, ...) %>%
  #   select(meanval, ...) %>%
  #   rename(PED = meanval)
  # 
  # ping_by_group <- my.summarize(ping_sims.time, ...) %>%
  #   select(meanval, ...) %>%
  #   rename(PING = meanval)
  
  proped_by_group <- my.summarize(proped_sims.time, ...) %>%
    select(medianval, ...) %>%
    rename(PROPED = medianval)
  
  proping_by_group <- my.summarize(proping_sims.time, ...) %>%
    select(medianval, ...) %>%
    rename(PROPING = medianval)
  
  if(!is.null(iav_sims.time)){
    iav_by_group <- my.summarize(iav_sims.time, ...) %>%
      select(medianval, ...) %>%
      rename(IAV = medianval)
  }

  sdc <- inner_join(proped_by_group, proping_by_group)
  if(is.null(iav_sims.time)){
    all3 <- sdc %>%
      mutate(IAV = NA)
  }else{
    all3 <- inner_join(sdc, iav_by_group)
  }
  return(all3)
}

x1 <- wrap3(proped_sims.time, proping_sims.time, iav_sims.time, IBMlab) %>% select(PROPED, PROPING, IAV, IBMlab)
x2 <- wrap3(proped_sims.time, proping_sims.time, iav_sims.time, IBMlab, retro_type) %>% select(PROPED, PROPING, IAV, IBMlab, retro_type)
x3 <- wrap3(proped_sims.time, proping_sims.time, NULL, IBMlab, time.avg) %>% select(PROPED, PROPING, IAV, IBMlab, time.avg)
x4 <- wrap3(proped_sims.time, proping_sims.time, iav_sims.time, IBMlab, catch.mult) %>% select(PROPED, PROPING, IAV, IBMlab, catch.mult)
x5 <- wrap3(proped_sims.time, proping_sims.time, iav_sims.time, IBMlab, Fhist) %>% select(PROPED, PROPING, IAV, IBMlab, Fhist)
x6 <- wrap3(proped_sims.time, proping_sims.time, NULL, IBMlab, catch.mult, time.avg) %>% select(PROPED, PROPING, IAV, IBMlab, catch.mult, time.avg)
x7 <- wrap3(proped_sims.time, proping_sims.time, NULL, IBMlab, Fhist, time.avg) %>% select(PROPED, PROPING, IAV, IBMlab, Fhist, time.avg)
x8 <- wrap3(proped_sims.time, proping_sims.time, iav_sims.time, IBMlab, retro_type, Fhist) %>% select(PROPED, PROPING, IAV, IBMlab, retro_type, Fhist)
x9 <- wrap3(proped_sims.time, proping_sims.time, NULL, IBMlab, retro_type, time.avg) %>% select(PROPED, PROPING, IAV, IBMlab, retro_type, time.avg)
x10 <- wrap3(proped_sims.time, proping_sims.time, iav_sims.time, IBMlab, Fhist, catch.mult) %>% select(PROPED, PROPING, IAV, IBMlab, Fhist, catch.mult)

write.csv(x1, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.csv", row.names = FALSE)
write.csv(x2, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.retro.csv", row.names = FALSE)
write.csv(x3, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.time.csv", row.names = FALSE)
write.csv(x4, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.cmult.csv", row.names = FALSE)
write.csv(x5, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.Fhist.csv", row.names = FALSE)
write.csv(x6, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.cmult.time.csv", row.names = FALSE)
write.csv(x7, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.Fhist.time.csv", row.names = FALSE)
write.csv(x8, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.retro.Fhist.csv", row.names = FALSE)
write.csv(x9, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.retro.time.csv", row.names = FALSE)
write.csv(x10, file = "Manuscript/tables_figs/heatmap/table.sdciav.ibm.Fhist.cmult.csv", row.names = FALSE)
