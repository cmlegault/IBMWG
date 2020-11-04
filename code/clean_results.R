# clean_results.R
# remove duplicate scenarios and runs from performance_metrics.rds

library(tidyverse)

# starting files
orig <- readRDS("results/perform-metrics.rds")

# remove duplicate rowid results (runs done multiple times)
dupes <- duplicated(orig$rowid)
clean <- orig[!dupes, ]

# find and remove duplicate scenarios
mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]

clean <- clean %>%
  filter(rowid %in% not_dupes)

# check to see no more than 1,000 simulations per scenario
count_table_clean <- clean %>%
  group_by(iscen) %>%
  summarise(n = n())
count_table_clean$n 

# save cleaned file
saveRDS(clean, file = "results/perform-metrics_clean.rds")
