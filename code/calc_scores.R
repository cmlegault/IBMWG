# calc_scores.R
# calculate mean rank and residual scores by scenario results across metrics
# see shiny app in demonstrations/chris/scorer_app for interactive plots

library(tidyverse)

# read in mean by scenario results
ssb_mean_by_scenario <- readRDS(file = "tables_figs/ssb_mean_by_scenario.rds")

f_mean_by_scenario <- readRDS(file = "tables_figs/f_mean_by_scenario.rds")

catch_mean_by_scenario <- readRDS(file = "tables_figs/catch_mean_by_scenario.rds")

ssb_mean_by_scenario_noretro <- readRDS(file = "tables_figs/ssb_mean_by_scenario_noretro.rds")

f_mean_by_scenario_noretro <- readRDS(file = "tables_figs/f_mean_by_scenario_noretro.rds")

catch_mean_by_scenario_noretro <- readRDS(file = "tables_figs/catch_mean_by_scenario_noretro.rds")

ssb_mean_by_scenario_scaa <- readRDS(file = "tables_figs/ssb_mean_by_scenario_scaa.rds")

f_mean_by_scenario_scaa <- readRDS(file = "tables_figs/f_mean_by_scenario_scaa.rds")

catch_mean_by_scenario_scaa <- readRDS(file = "tables_figs/catch_mean_by_scenario_scaa.rds")

# apply bigger better
apply_bb <- function(myres, biggerbetter){
  abb <- myres %>%
    group_by(IBMlab, metric) %>%
    summarise(meanval = mean(value)) %>%
    inner_join(biggerbetter, by = "metric") %>%
    mutate(bb = meanval * bbmult) %>%
    arrange(metric)
  return(abb)
}

# function to rank IBMs across scenarios
# higher score is better
calc_scores <- function(abb){
  myscores <- abb %>%
    group_by(metric) %>%
    mutate(score = rank(bb)) %>%
    mutate(resid = (bb - mean(bb)) / sd(bb)) %>%
    arrange(metric)
  return(myscores)
}

ssb_bigger_better <- data.frame(metric = unique(ssb_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("less", metric), -1, 1))

f_bigger_better <- data.frame(metric = unique(f_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("less", metric), 1, -1))

catch_bigger_better <- data.frame(metric = unique(catch_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("avg", metric), 1, -1))

# create base scores
ssb_abb <- apply_bb(ssb_mean_by_scenario, ssb_bigger_better) %>%
  mutate(metric = paste0("ssb_", metric))

f_abb <- apply_bb(f_mean_by_scenario, f_bigger_better) %>%
  mutate(metric = paste0("f_", metric))

catch_abb <- apply_bb(catch_mean_by_scenario, catch_bigger_better) %>%
  mutate(metric = paste0("catch_", metric))

all_abb <- rbind(ssb_abb, f_abb, catch_abb) %>%
  arrange(metric)

all_scores <- calc_scores(all_abb) 

# save output tables
all_abb_table <- all_scores %>%
  select(IBMlab, metric, bb) %>%
  pivot_wider(names_from = "IBMlab", values_from = "bb") 

write.csv(all_abb_table, file = "tables_figs/all_abb.csv")

all_scores_table <- all_scores %>%
  select(IBMlab, metric, score) %>%
  pivot_wider(names_from = "IBMlab", values_from = "score") 

write.csv(all_scores_table, file="demonstrations/chris/scorer_app/all_scores.csv")

all_resids_table <- all_scores %>%
  select(IBMlab, metric, resid) %>%
  pivot_wider(names_from = "IBMlab", values_from = "resid")

write.csv(all_resids_table, file="demonstrations/chris/scorer_app/all_resids.csv")

# create noretro scores
ssb_abb_noretro <- apply_bb(ssb_mean_by_scenario_noretro, ssb_bigger_better) %>%
  mutate(metric = paste0("ssb_", metric))

f_abb_noretro <- apply_bb(f_mean_by_scenario_noretro, f_bigger_better) %>%
  mutate(metric = paste0("f_", metric))

catch_abb_noretro <- apply_bb(catch_mean_by_scenario_noretro, catch_bigger_better) %>%
  mutate(metric = paste0("catch_", metric))

all_abb_noretro <- rbind(ssb_abb_noretro, f_abb_noretro, catch_abb_noretro) %>%
  arrange(metric)

all_scores_noretro <- calc_scores(all_abb_noretro) 

# save noretro output tables
all_abb_table_noretro <- all_scores_noretro %>%
  select(IBMlab, metric, bb) %>%
  pivot_wider(names_from = "IBMlab", values_from = "bb") 

write.csv(all_abb_table_noretro, file = "tables_figs/all_abb_noretro.csv")

all_scores_table_noretro <- all_scores_noretro %>%
  select(IBMlab, metric, score) %>%
  pivot_wider(names_from = "IBMlab", values_from = "score") 

write.csv(all_scores_table_noretro, file="demonstrations/chris/scorer_app/all_scores_noretro.csv")

all_resids_table_noretro <- all_scores_noretro %>%
  select(IBMlab, metric, resid) %>%
  pivot_wider(names_from = "IBMlab", values_from = "resid")

write.csv(all_resids_table_noretro, file="demonstrations/chris/scorer_app/all_resids_noretro.csv")

# create scaa scores
ssb_abb_scaa <- apply_bb(ssb_mean_by_scenario_scaa, ssb_bigger_better) %>%
  mutate(metric = paste0("ssb_", metric))

f_abb_scaa <- apply_bb(f_mean_by_scenario_scaa, f_bigger_better) %>%
  mutate(metric = paste0("f_", metric))

catch_abb_scaa <- apply_bb(catch_mean_by_scenario_scaa, catch_bigger_better) %>%
  mutate(metric = paste0("catch_", metric))

all_abb_scaa <- rbind(ssb_abb_scaa, f_abb_scaa, catch_abb_scaa) %>%
  arrange(metric)

all_scores_scaa <- calc_scores(all_abb_scaa) 

# save scaa output tables
all_abb_table_scaa <- all_scores_scaa %>%
  select(IBMlab, metric, bb) %>%
  pivot_wider(names_from = "IBMlab", values_from = "bb") 

write.csv(all_abb_table_scaa, file = "tables_figs/all_abb_scaa.csv")

all_scores_table_scaa <- all_scores_scaa %>%
  select(IBMlab, metric, score) %>%
  pivot_wider(names_from = "IBMlab", values_from = "score") 

write.csv(all_scores_table_scaa, file="demonstrations/chris/scorer_app/all_scores_scaa.csv")

all_resids_table_scaa <- all_scores_scaa %>%
  select(IBMlab, metric, resid) %>%
  pivot_wider(names_from = "IBMlab", values_from = "resid")

write.csv(all_resids_table_scaa, file="demonstrations/chris/scorer_app/all_resids_scaa.csv")

# compute overall scores by IBM
get_mean_scores <- function(mytibble){
  mymeans <- mytibble %>%
    group_by(IBMlab) %>%
    summarize(Rank = mean(score), Resid = mean(resid))
  return(mymeans)
}

filter_scores <- function(mytibble, mymetrics){
  myfiltered <- mytibble %>%
    filter(metric %in% mymetrics) %>%
    get_mean_scores() %>%
    pivot_longer(cols = !IBMlab, names_to = "source", values_to = "value")
  return(myfiltered)
}

# assumes list of 3 tibbles input: base, noretro, scaa
plot_scores <- function(mytib, mymetrics, mytitle){
  mytitleext <- c("", "(No retro scenarios)", "(SCAA scenarios)")
  myplot <- list()
  for (i in 1:3){
  myf <- filter_scores(mytib[[i]], mymetrics)
  myplot[[i]] <- ggplot(myf, aes(x=reorder(IBMlab, value), y=value)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~source, scales = "free_x") +
    labs(x="", y="Score (bigger is better)", title=paste(mytitle, mytitleext[i])) +
    theme_bw()
  }
  return(myplot)
}

all_scores_list <- list(all_scores, all_scores_noretro, all_scores_scaa)

# define plots
mymetrics <- c("ssb_l_avg_ssb_ssbmsy", "f_l_avg_f_fmsy", "catch_l_avg_catch_msy")
mytitle <- "X/Xmsy Long Term"
xmsy_l_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("ssb_l_avg_ssb_ssbmsy")
mytitle <- "SSB/SSBmsy Long Term"
ssbmsy_l_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("f_l_avg_f_fmsy")
mytitle <- "F/Fmsy Long Term"
fmsy_l_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("catch_l_avg_catch_msy")
mytitle <- "Catch/MSY Long Term"
catchmsy_l_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("ssb_s_avg_ssb_ssbmsy", "f_s_avg_f_fmsy", "catch_s_avg_catch_msy")
mytitle <- "X/Xmsy Short Term"
xmsy_s_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("ssb_s_avg_ssb_ssbmsy")
mytitle <- "SSB/SSBmsy Short Term"
ssbmsy_s_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("f_s_avg_f_fmsy")
mytitle <- "F/Fmsy Short Term"
fmsy_s_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("catch_s_avg_catch_msy")
mytitle <- "Catch/MSY Short Term"
catchmsy_s_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("ssb_l_avg_ssb_ssbmsy", "f_l_avg_f_fmsy", "catch_l_avg_catch_msy", "ssb_s_avg_ssb_ssbmsy", "f_s_avg_f_fmsy", "catch_s_avg_catch_msy")
mytitle <- "X/Xmsy Both Long and Short Term"
xmsy_b_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("ssb_l_avg_ssb_ssbmsy", "ssb_s_avg_ssb_ssbmsy")
mytitle <- "SSB/SSBmsy Both Long and Short Term"
ssbmsy_b_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("f_l_avg_f_fmsy", "f_s_avg_f_fmsy")
mytitle <- "F/Fmsy Both Long and Short Term"
fmsy_b_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("catch_l_avg_catch_msy", "catch_s_avg_catch_msy")
mytitle <- "Catch/MSY Both Long and Short Term"
catchmsy_b_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

mymetrics <- c("catch_a_iav_catch", "catch_s_avg_catch_msy")
mytitle <- "Catch iav (all years) and short term C/MSY"
c_only_plot <- plot_scores(all_scores_list, mymetrics, mytitle)

### put plots into pdf
pdf(file = "tables_figs/scores.pdf")

for (i in 1:3){

  print(xmsy_l_plot[[i]])
  print(ssbmsy_l_plot[[i]])
  print(fmsy_l_plot[[i]])
  print(catchmsy_l_plot[[i]])

  print(xmsy_s_plot[[i]])
  print(ssbmsy_s_plot[[i]])
  print(fmsy_s_plot[[i]])
  print(catchmsy_s_plot[[i]])

  print(xmsy_b_plot[[i]])
  print(ssbmsy_b_plot[[i]])
  print(fmsy_b_plot[[i]])
  print(catchmsy_b_plot[[i]])

  print(c_only_plot[[i]])

}

dev.off()

