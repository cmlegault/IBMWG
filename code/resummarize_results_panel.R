# correlation plot for the metrics

## install needed libraries & scripts
#library(wham)
library(tidyverse)
library(furrr)
library(ggcorrplot)

mse_results <- readRDS("results/perform-metrics_clean.rds")
#plan(multisession)
#profvis::profvis(
catch_results <- mse_results %>% 
  #slice(1:100) %>% 
  #filter(isim < 100) %>% 
  select(iscen, isim, catch_metrics, ssb_metrics, f_metrics) %>% 
  # mutate(catch_metrics = map(catch_metrics, enframe),
  #        ssb_metrics = map(ssb_metrics, enframe),
  #        f_metrics = map(f_metrics, enframe)) %>% 
  pivot_longer(cols = c(catch_metrics, ssb_metrics, f_metrics),
               names_to = "type") %>% 
  mutate(value = map(value, enframe)) %>% 
  unnest(cols = c(value)) %>% 
  mutate(value = as.numeric(value)) %>% #map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  select(-type) %>% 
  pivot_wider(names_from = metric,
              values_from = value) %>% 
  select(-(1:2)) %>% 
  I()
#)
catch_results
metrics_use <- names(map_int(catch_results, ~length(unique(.)))>3)
corr <- round(cor(catch_results %>% select(metrics_use)), 2)

p1 <- ggcorrplot(
  corr,
  hc.order = TRUE,
  type = "lower",
  lab=FALSE, #TRUE,
  outline.color = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726")
)
ggsave("tables_figs/corr_plot.png", p1, scale = 2)
