# demo_make_tables_figures.R
# uses demo_perform-metrics.rds results to create summary tables and figures

library(tidyverse)

# read in the performance metrics results
mse_results <- readRDS("demonstrations/chris/demo_plots/demo-perform-metrics.rds")
startdim <- dim(mse_results)

# remove any duplicate rows
mse_results <- mse_results %>%
  distinct()
enddim <- dim(mse_results)
enddim - startdim # if greater than zero, then there were duplicates
names(mse_results)
head(mse_results)

# counts
count_table <- mse_results %>%
  group_by(iscen) %>%
  summarise(n = n())
count_table$n  

# join with setup to figure out what's in each scenario
defined <- readRDS("settings/mse_sim_setup.rds") %>%
  filter(isim == 1) %>%
  select(iscen, specs) %>%
  unnest(cols = specs) %>%
  inner_join(count_table, input, by="iscen") %>%
  mutate(IBMlab = paste0(IBM, expand_method, M_CC_method))
defined
names(defined)
unique(defined$IBMlab)

# another summary
countIBM <- defined %>%
  group_by(IBMlab) %>%
  summarise(nscenarios = n(), nsim = sum(n))
countIBM

#pull out the ssb metrics
ssb_results <- mse_results %>% 
  #select(rowid, ssb_metrics) %>% 
  select(iscen, isim, ssb_metrics) %>% 
  mutate(ssb_metrics = map(ssb_metrics, enframe)) %>% 
  unnest(cols = c(ssb_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
ssb_results
#```

ssb_probs <- ssb_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  filter(grepl("_is_", metric)) %>%
  inner_join(., defined)

ssb_probs_plot <- ggplot(ssb_probs, aes(x=iscen, y=value)) +
  geom_point() +
  facet_wrap(~metric) +
  labs(x="Scenario", y="Probability", title = "SSB")
  theme_bw()
ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs.png", ssb_probs_plot)

# color code to show factors
p1 <- ssb_probs_plot + geom_point(aes(color = retro_type))
ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_retro_type.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = IBMlab))
ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_IBMlab.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = factor(Fhist)))
ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_Fhist.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = factor(n_selblocks)))
ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_n_selblocks.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = factor(catch.mult)))
ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_catch.mults.png", p1)

which_rebuild <- ssb_probs %>%
  filter(metric == "l_is_ge_bmsy", value >= 0.9) 
which_rebuild$retro_type
which_rebuild$IBMlab

which_crash <- ssb_probs %>%
  filter(metric == "l_is_less_01_bmsy", value >= 0.90) 
which_crash$retro_type
which_crash$IBMlab
