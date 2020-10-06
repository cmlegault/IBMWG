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
  labs(x="Scenario", y="Probability", title = "SSB") +
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

#pull out the f metrics
f_results <- mse_results %>% 
  #select(rowid, f_metrics) %>% 
  select(iscen, isim, f_metrics) %>% 
  mutate(f_metrics = map(f_metrics, enframe)) %>% 
  unnest(cols = c(f_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
f_results
#```

f_probs <- f_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  filter(grepl("_is_", metric)) %>%
  inner_join(., defined)

f_probs_plot <- ggplot(f_probs, aes(x=iscen, y=value)) +
  geom_point() +
  facet_wrap(~metric) +
  labs(x="Scenario", y="Probability", title = "F") +
  theme_bw()
ggsave(filename = "demonstrations/chris/demo_plots/f_probs.png", f_probs_plot)

# color code to show factors
p1 <- f_probs_plot + geom_point(aes(color = retro_type))
ggsave(filename = "demonstrations/chris/demo_plots/f_probs_retro_type.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = IBMlab))
ggsave(filename = "demonstrations/chris/demo_plots/f_probs_IBMlab.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = factor(Fhist)))
ggsave(filename = "demonstrations/chris/demo_plots/f_probs_Fhist.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = factor(n_selblocks)))
ggsave(filename = "demonstrations/chris/demo_plots/f_probs_n_selblocks.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = factor(catch.mult)))
ggsave(filename = "demonstrations/chris/demo_plots/f_probs_catch.mults.png", p1)

#pull out the catch metrics
catch_results <- mse_results %>% 
  #select(rowid, catch_metrics) %>% 
  select(iscen, isim, catch_metrics) %>% 
  mutate(catch_metrics = map(catch_metrics, enframe)) %>% 
  unnest(cols = c(catch_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
catch_results
#```

catch_means <- catch_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  filter(grepl("_avg_catch_msy", metric)) %>%
  inner_join(., defined)

catch_msy_plot <- ggplot(catch_means, aes(x=iscen, y=value)) +
  geom_point() +
  facet_wrap(~metric) +
  labs(x="Scenario", y="Mean Catch Relative to MSY") +
  theme_bw()
ggsave(filename = "demonstrations/chris/demo_plots/catch_msy.png", catch_msy_plot)

# color code to show factors
p1 <- catch_msy_plot + geom_point(aes(color = retro_type))
ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_retro_type.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = IBMlab))
ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_IBMlab.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = factor(Fhist)))
ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_Fhist.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = factor(n_selblocks)))
ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_n_selblocks.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = factor(catch.mult)))
ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_catch.mults.png", p1)

# trade-off plots
names(ssb_probs)
names(f_probs)
names(catch_means)
ssb_temp <- ssb_probs %>%
  rename(ssb_metric = metric, ssb_value = value)
f_temp <- f_probs %>%
  rename(f_metric = metric, f_value = value)
catch_temp <- catch_means %>%
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
ggsave(filename = "demonstrations/chris/demo_plots/tradeoff1.png", td1_plot)

td2 <- td %>%
  filter(ssb_metric == "l_is_less_05_bmsy",
         f_metric == "l_is_gr_fmsy")

td2_plot <- ggplot(td2, aes(x=ssb_value, y=f_value, color=retro_type)) +
  geom_point() +
  facet_wrap(~IBMlab) +
  labs(x="Prob(SSB<0.5SSBmsy)", y="Prob(F>Fmsy)") +
  theme_bw()
ggsave(filename = "demonstrations/chris/demo_plots/tradeoff2.png", td2_plot)
