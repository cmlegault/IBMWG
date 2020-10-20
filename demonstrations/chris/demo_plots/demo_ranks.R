# demo_ranks.R
# explore ranking mean by scenario results across metrics

library(tidyverse)

# read in mean by scenario results
ssb_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/ssb_mean_by_scenario.rds")

f_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/f_mean_by_scenario.rds")

catch_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/catch_mean_by_scenario.rds")

# function to rank IBMs across scenarios
calc_ranks <- function(myres, biggerbetter){
  myranks <- myres %>%
    group_by(IBMlab, metric) %>%
    summarise(meanval = mean(value)) %>%
    inner_join(biggerbetter, by = "metric") %>%
    mutate(bb = meanval * bbmult) %>%
    mutate(r = rank(meanval))
  return(myranks)
}

ssb_bigger_better <- data.frame(metric = unique(ssb_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("less", metric), -1, 1))

f_bigger_better <- data.frame(metric = unique(f_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("less", metric), 1, -1))

catch_bigger_better <- data.frame(metric = unique(catch_mean_by_scenario$metric)) %>%
  mutate(bbmult = ifelse(grepl("avg", metric), 1, -1))

ssb_ranks <- calc_ranks(ssb_mean_by_scenario, ssb_bigger_better) %>%
  select(IBMlab, metric, r) %>%
  pivot_wider(names_from = "IBMlab", values_from = "r") %>%
  mutate(metric = paste0("ssb_", metric))

f_ranks <- calc_ranks(f_mean_by_scenario, f_bigger_better) %>%
  select(IBMlab, metric, r) %>%
  pivot_wider(names_from = "IBMlab", values_from = "r") %>%
  mutate(metric = paste0("f_", metric))

catch_ranks <- calc_ranks(catch_mean_by_scenario, catch_bigger_better) %>%
  select(IBMlab, metric, r) %>%
  pivot_wider(names_from = "IBMlab", values_from = "r") %>%
  mutate(metric = paste0("catch_", metric))

all_ranks <- rbind(ssb_ranks, f_ranks, catch_ranks)

write.csv(all_ranks, file="demonstrations/chris/demo_plots/all_ranks.csv")
