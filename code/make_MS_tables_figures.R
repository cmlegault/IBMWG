# make_MS_tables_figures.R
# create tables and figures for primary literature publication
# uses cleaned performance metrics from base, full SCAA, and no retro  
# output goes to Manuscript/tables_figs directory

library(tidyverse)

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

