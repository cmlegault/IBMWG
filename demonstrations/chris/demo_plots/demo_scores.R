# demo_scores.R
# explore ranking mean by scenario results across metrics

library(tidyverse)

# read in mean by scenario results
ssb_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/ssb_mean_by_scenario.rds")

f_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/f_mean_by_scenario.rds")

catch_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/catch_mean_by_scenario.rds")

# function to rank IBMs across scenarios
# higher score is better
calc_scores <- function(myres, biggerbetter){
  myscores <- myres %>%
    group_by(IBMlab, metric) %>%
    summarise(meanval = mean(value)) %>%
    inner_join(biggerbetter, by = "metric") %>%
    mutate(bb = meanval * bbmult) %>%
    group_by(metric) %>%
    mutate(score = rank(bb)) %>%
    arrange(metric)
  return(myscores)
}

ssb_bigger_better <- data.frame(metric = unique(ssb_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("less", metric), -1, 1))

f_bigger_better <- data.frame(metric = unique(f_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("less", metric), 1, -1))

catch_bigger_better <- data.frame(metric = unique(catch_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("avg", metric), 1, -1))

ssb_scores <- calc_scores(ssb_mean_by_scenario, ssb_bigger_better) %>%
  select(IBMlab, metric, score) %>%
  pivot_wider(names_from = "IBMlab", values_from = "score") %>%
  mutate(metric = paste0("ssb_", metric))

f_scores <- calc_scores(f_mean_by_scenario, f_bigger_better) %>%
  select(IBMlab, metric, score) %>%
  pivot_wider(names_from = "IBMlab", values_from = "score") %>%
  mutate(metric = paste0("f_", metric))

catch_scores <- calc_scores(catch_mean_by_scenario, catch_bigger_better) %>%
  select(IBMlab, metric, score) %>%
  pivot_wider(names_from = "IBMlab", values_from = "score") %>%
  mutate(metric = paste0("catch_", metric))

all_scores <- rbind(ssb_scores, f_scores, catch_scores)

write.csv(all_scores, file="demonstrations/chris/demo_plots/all_scores.csv")
