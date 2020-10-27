# demo_make_tables_figures.R
# uses demo_perform-metrics.rds results to create summary tables and figures

library(tidyverse)

### read in the performance metrics results
mse_results <- readRDS("demonstrations/chris/demo_plots/demo-perform-metrics.rds")
startdim <- dim(mse_results)

# remove any duplicate rows
mse_results <- mse_results %>%
  distinct()
enddim <- dim(mse_results)
startdim - enddim # if greater than zero, then there were duplicates
names(mse_results)
head(mse_results)

# find and remove duplicate scenarios
mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]
mse_results <- mse_results %>%
  filter(rowid %in% not_dupes)

# counts
count_table <- mse_results %>%
  group_by(iscen) %>%
  summarise(n = n())
count_table$n 

### join with setup to figure out what's in each scenario
Fhistlab <- c("O","F") # O = always overfishing, F = Fmsy in last year of base
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

# counting scenarios and simulations
countIBM <- defined %>%
  group_by(IBMlab, Scenlab) %>%
  summarise(nscenarios = n(), nsim = sum(n)) 

nscentab <- countIBM %>%
  select(IBMlab, Scenlab, nscenarios) %>%
  pivot_wider(names_from = Scenlab, values_from = nscenarios)
nscentab

nsimtab <- countIBM %>%
  select(IBMlab, Scenlab, nsim) %>%
  pivot_wider(names_from = Scenlab, values_from = nsim)
nsimtab

nsim_plot <- ggplot(countIBM, aes(x=Scenlab, y=nsim)) +
  geom_bar(stat = "identity") +
  facet_wrap(~IBMlab) +
  labs(x="Scenario", y="Number of Simulations") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

scendf <- data.frame(x=rep(1, 5),
                     y=seq(5, 1, -1),
                     mytext=c("CF1A to MO2R explained",
                              "Retrotype: C=catch, M=natural mortality",
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

### save mean_by_scenario results for easier modeling and ranking
saveRDS(ssb_mean_by_scenario, 
        file = "demonstrations/chris/demo_plots/ssb_mean_by_scenario.rds")

saveRDS(f_mean_by_scenario, 
        file = "demonstrations/chris/demo_plots/f_mean_by_scenario.rds")

saveRDS(catch_mean_by_scenario, 
        file = "demonstrations/chris/demo_plots/catch_mean_by_scenario.rds")

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
  filter(metric %in% c("l_iav_catch", "l_prop_g_msy_2_of_3"))

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

f_probs_plot <- confetti_plot(f_probs, iscen, value, "Scenario", "Probablity", "F")

f_ns_plot <- confetti_plot(f_ns, iscen, value, "Scenario", "Mean Number of Years", "F")

f_ratios_plot <- confetti_plot(f_ratios, iscen, value, "Scenario", "Mean F/Fmsy", "F")

f_box_probs_IBM <- make_box_plot(f_probs, IBMlab, value, "IBM", "Probability", "F")

f_box_probs_Scen <- make_box_plot(f_probs, Scenlab, value, "Scenario", "Probability", "F")

f_box_ns_IBM <- make_box_plot(f_ns, IBMlab, value, "IBM", "Mean Number of Years", "F")

f_box_ns_Scen <- make_box_plot(f_ns, Scenlab, value, "Scenario", "Mean Number of Years", "F")

f_box_ratios_IBM <- make_box_plot(f_ratios, IBMlab, value, "IBM", "Mean F/Fmsy", "F")

f_box_ratios_Scen <- make_box_plot(f_ratios, Scenlab, value, "Scenario", "Mean F/Fmsy", "F")

catch_means_plot <- confetti_plot(catch_means, iscen, value, "Scenario", "Mean of Metric", "Catch")

catch_ratios_plot <- confetti_plot(catch_ratios, iscen, value, "Scenario", "Mean Catch/MSY", "Catch")

catch_other_plot <- confetti_plot(catch_other, iscen, value, "Scenario", "Mean of Metric", "Catch")

catch_box_means_IBM <- make_box_plot(catch_means, IBMlab, value, "IBM", "Mean of Metric", "Catch")

catch_box_means_Scen <- make_box_plot(catch_means, Scenlab, value, "Scenario", "Mean of Metric", "Catch")

catch_box_ratios_IBM <- make_box_plot(catch_ratios, IBMlab, value, "IBM", "Mean Catch/MSY", "Catch")

catch_box_ratios_Scen <- make_box_plot(catch_ratios, Scenlab, value, "Scenario", "Mean Catch/MSY", "Catch")

catch_box_other_IBM <- make_box_plot(catch_other, IBMlab, value, "IBM", "Mean of Metric", "Catch")

catch_box_other_Scen <- make_box_plot(catch_other, Scenlab, value, "Scenario", "Mean of Metric", "Catch")

### examine what factors led to good and bad outcomes
which_rebuild <- ssb_probs %>%
  filter(metric == "l_is_ge_bmsy", value >= 0.9) 
which_rebuild$retro_type
which_rebuild$IBMlab
which_rebuild$Scenlab

which_crash <- ssb_probs %>%
  filter(metric == "l_is_less_01_bmsy", value >= 0.90) 
which_crash$retro_type
which_crash$IBMlab
which_crash$Scenlab

### trade-off plots
names(ssb_probs)
names(f_probs)
names(catch_ratios)
ssb_temp <- ssb_probs %>%
  rename(ssb_metric = metric, ssb_value = value)
f_temp <- f_probs %>%
  rename(f_metric = metric, f_value = value)
catch_temp <- catch_ratios %>%
  rename(catch_metric = metric, catch_value = value)
temp <- inner_join(ssb_temp, f_temp)
td <- inner_join(temp, catch_temp)
names(td)
td

td1 <- td %>%
  filter(ssb_metric == "l_is_ge_bmsy",
         catch_metric == "l_avg_catch_msy")

td1_plot <- ggplot(td1, aes(x=ssb_value, y=catch_value, color=retro_type)) +
  geom_point() +
  facet_wrap(~IBMlab) +
  labs(x="Prob(SSB>=SSBmsy)", y="Mean(Catch/MSY)") +
  theme_bw()

td2 <- td %>%
  filter(ssb_metric == "l_is_less_05_bmsy",
         f_metric == "l_is_gr_fmsy")

td2_plot <- ggplot(td2, aes(x=ssb_value, y=f_value, color=retro_type)) +
  geom_point() +
  facet_wrap(~IBMlab) +
  labs(x="Prob(SSB<0.5SSBmsy)", y="Prob(F>Fmsy)") +
  theme_bw()

names(ssb_ratios)
names(catch_ratios)
ssb2_temp <- ssb_ratios %>%
  filter(metric == "l_avg_ssb_ssbmsy") %>%
  rename(ssb_metric = metric, ssb_value = value)
catch2_temp <- catch_ratios %>%
  filter(metric == "l_avg_catch_msy") %>%
  rename(catch_metric = metric, catch_value = value)
td3 <- inner_join(ssb2_temp, catch2_temp)
td3_plot <- ggplot(td3, aes(x=ssb_value, y=catch_value)) +
  geom_point() +
  facet_wrap(~IBMlab) +
  labs(x="SSB/SSBmsy", y="Catch/MSY", title="Longterm") +
  theme_bw()

# tradeoffs by sim
ssb_sims_l <- ssb_results %>%
  filter(metric == "l_avg_ssb_ssbmsy") %>%
  rename(ssb_metric_l = metric, ssb_value_l = value) %>%
  inner_join(defined, by = "iscen")

ssb_sims_s <- ssb_results %>%
  filter(metric == "s_avg_ssb_ssbmsy") %>%
  rename(ssb_metric_s = metric, ssb_value_s = value) %>%
  inner_join(defined, by = "iscen")

ssb_sims <- inner_join(ssb_sims_l, ssb_sims_s, by = c("iscen", "isim", "retro_type", "Fhist", "n_selblocks", "IBM", "adv.yr", "Fmsy_scale", "catch.mult", "expand_method", "M_CC_method", "nprojyrs", "n", "IBMlab", "Scenlab"))

f_sims_l <- f_results %>%
  filter(metric == "l_avg_f_fmsy") %>%
  rename(f_metric_l = metric, f_value_l = value) %>%
  inner_join(defined, by = "iscen")

f_sims_s <- f_results %>%
  filter(metric == "s_avg_f_fmsy") %>%
  rename(f_metric_s = metric, f_value_s = value) %>%
  inner_join(defined, by = "iscen")

f_sims <- inner_join(f_sims_l, f_sims_s, by = c("iscen", "isim", "retro_type", "Fhist", "n_selblocks", "IBM", "adv.yr", "Fmsy_scale", "catch.mult", "expand_method", "M_CC_method", "nprojyrs", "n", "IBMlab", "Scenlab"))

catch_sims_l <- catch_results %>%
  filter(metric == "l_avg_catch_msy") %>%
  rename(catch_metric_l = metric, catch_value_l = value) %>%
  inner_join(defined, by = "iscen")

catch_sims_s <- catch_results %>%
  filter(metric == "s_avg_catch_msy") %>%
  rename(catch_metric_s = metric, catch_value_s = value) %>%
  inner_join(defined, by = "iscen")

catch_sims <- inner_join(catch_sims_l, catch_sims_s, by = c("iscen", "isim", "retro_type", "Fhist", "n_selblocks", "IBM", "adv.yr", "Fmsy_scale", "catch.mult", "expand_method", "M_CC_method", "nprojyrs", "n", "IBMlab", "Scenlab"))

tempdf <- inner_join(ssb_sims, f_sims, by = c("iscen", "isim", "retro_type", "Fhist", "n_selblocks", "IBM", "adv.yr", "Fmsy_scale", "catch.mult", "expand_method", "M_CC_method", "nprojyrs", "n", "IBMlab", "Scenlab"))
 
simdf <- inner_join(tempdf, catch_sims, by = c("iscen", "isim", "retro_type", "Fhist", "n_selblocks", "IBM", "adv.yr", "Fmsy_scale", "catch.mult", "expand_method", "M_CC_method", "nprojyrs", "n", "IBMlab", "Scenlab"))

myscenlabs <- sort(unique(simdf$Scenlab))
nscenlabs <- length(myscenlabs)
mysmax_l <- max(simdf$ssb_value_l, na.rm = TRUE)
myfmax_l <- max(simdf$f_value_l, na.rm = TRUE)
mycmax_l <- max(simdf$catch_value_l, na.rm = TRUE)
mysmax_s <- max(simdf$ssb_value_s, na.rm = TRUE)
myfmax_s <- max(simdf$f_value_s, na.rm = TRUE)
mycmax_s <- max(simdf$catch_value_s, na.rm = TRUE)

td4_l_plot <- list()
for (i in 1:nscenlabs){
  tmpdf <- filter(simdf, Scenlab == myscenlabs[i])
  td4_l_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_l, y=catch_value_l)) +
    geom_point() +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~IBMlab) +
    labs(x="SSB/SSBmsy", y="Catch/MSY", title=paste(myscenlabs[i], "Long Term")) +
    expand_limits(x=mysmax_l, y=mycmax_l) +
    theme_bw()
#  print(td4_l_plot[[i]])
}

td4_s_plot <- list()
for (i in 1:nscenlabs){
  tmpdf <- filter(simdf, Scenlab == myscenlabs[i])
  td4_s_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_s, y=catch_value_s)) +
    geom_point() +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~IBMlab) +
    labs(x="SSB/SSBmsy", y="Catch/MSY", title=paste(myscenlabs[i], "Short Term")) +
    expand_limits(x=mysmax_s, y=mycmax_s) +
    theme_bw()
#  print(td4_s_plot[[i]])
}

myibmlabs <- sort(unique(simdf$IBMlab))

td5_l_plot <- list()
for (i in 1:length(myibmlabs)){
  tmpdf <- filter(simdf, IBMlab == myibmlabs[i])
  td5_l_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_l, y=catch_value_l)) +
    geom_point(color="blue") +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~Scenlab) +
    labs(x="SSB/SSBmsy", y="Catch/MSY", title=paste(myibmlabs[i], "Long Term")) +
    expand_limits(x=mysmax_l, y=mycmax_l) +
    theme_bw()
#  print(td5_l_plot[[i]])
}

td5_s_plot <- list()
for (i in 1:length(myibmlabs)){
  tmpdf <- filter(simdf, IBMlab == myibmlabs[i])
  td5_s_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_s, y=catch_value_s)) +
    geom_point(color="blue") +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~Scenlab) +
    labs(x="SSB/SSBmsy", y="Catch/MSY", title=paste(myibmlabs[i], "Short Term")) +
    expand_limits(x=mysmax_s, y=mycmax_s) +
    theme_bw()
#  print(td5_s_plot[[i]])
}

td6_l_plot <- list()
for (i in 1:nscenlabs){
  tmpdf <- filter(simdf, Scenlab == myscenlabs[i])
  td6_l_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_l, y=f_value_l)) +
    geom_point() +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~IBMlab) +
    labs(x="SSB/SSBmsy", y="F/Fmsy", title=paste(myscenlabs[i], "Long Term")) +
    expand_limits(x=mysmax_l, y=myfmax_l) +
    theme_bw()
#  print(td6_l_plot[[i]])
}

td6_s_plot <- list()
for (i in 1:nscenlabs){
  tmpdf <- filter(simdf, Scenlab == myscenlabs[i])
  td6_s_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_s, y=f_value_s)) +
    geom_point() +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~IBMlab) +
    labs(x="SSB/SSBmsy", y="F/Fmsy", title=paste(myscenlabs[i], "Short Term")) +
    expand_limits(x=mysmax_s, y=myfmax_s) +
    theme_bw()
#  print(td6_s_plot[[i]])
}

td7_l_plot <- list()
for (i in 1:length(myibmlabs)){
  tmpdf <- filter(simdf, IBMlab == myibmlabs[i])
  td7_l_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_l, y=f_value_l)) +
    geom_point(color="blue") +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~Scenlab) +
    labs(x="SSB/SSBmsy", y="F/Fmsy", title=paste(myibmlabs[i], "Long Term")) +
    expand_limits(x=mysmax_l, y=myfmax_l) +
    theme_bw()
#  print(td7_l_plot[[i]])
}

td7_s_plot <- list()
for (i in 1:length(myibmlabs)){
  tmpdf <- filter(simdf, IBMlab == myibmlabs[i])
  td7_s_plot[[i]] <- ggplot(tmpdf, aes(x=ssb_value_s, y=f_value_s)) +
    geom_point(color="blue") +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~Scenlab) +
    labs(x="SSB/SSBmsy", y="F/Fmsy", title=paste(myibmlabs[i], "Short Term")) +
    expand_limits(x=mysmax_s, y=myfmax_s) +
    theme_bw()
#  print(td7_s_plot[[i]])
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
catch_means_plot 
colorize_confetti(catch_means_plot)
catch_ratios_plot 
colorize_confetti(catch_ratios_plot)
catch_other_plot 
colorize_confetti(catch_other_plot)

td1_plot
td2_plot
td3_plot
for (i in 1:length(td4_l_plot)){
  print(td4_l_plot[[i]])
}
for (i in 1:length(td5_l_plot)){
  print(td5_l_plot[[i]])
}
for (i in 1:length(td4_s_plot)){
  print(td4_s_plot[[i]])
}
for (i in 1:length(td5_s_plot)){
  print(td5_s_plot[[i]])
}
for (i in 1:length(td6_l_plot)){
  print(td6_l_plot[[i]])
}
for (i in 1:length(td7_l_plot)){
  print(td7_l_plot[[i]])
}
for (i in 1:length(td6_s_plot)){
  print(td6_s_plot[[i]])
}
for (i in 1:length(td7_s_plot)){
  print(td7_s_plot[[i]])
}

dev.off()




