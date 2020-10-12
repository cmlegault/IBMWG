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
startdim - enddim # if greater than zero, then there were duplicates
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
  mutate(IBMlab = paste0(IBM, expand_method, M_CC_method)) %>%
  mutate(nonIBMlab = paste(substr(retro_type, 1, 1), Fhist, n_selblocks, catch.mult, sep = "_"))
defined
names(defined)
unique(defined$IBMlab)
unique(defined$nonIBMlab)

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

ssb_means <- ssb_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  filter(grepl("_avg_ssb_ssbmsy", metric)) %>%
  inner_join(., defined)

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
##ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs.png", ssb_probs_plot)

# color code to show factors
p1 <- ssb_probs_plot + geom_point(aes(color = retro_type))
#ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_retro_type.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = IBMlab))
#ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_IBMlab.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = factor(Fhist)))
#ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_Fhist.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = factor(n_selblocks)))
#ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_n_selblocks.png", p1)
p1 <- ssb_probs_plot + geom_point(aes(color = factor(catch.mult)))
#ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_catch.mults.png", p1)

#a boxplot of SSB metrics for each IBM among all scenarios ##do any always suck? do any always suck in the other direction?
#windows(width = 10,height = 10)
box_ssb1 <- ggplot(ssb_probs, aes(x=IBMlab, y=value)) + 
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="IBM", y="Probability", title = "SSB") 
#ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_box_IBM.png", box_ssb1)

box_ssb2 <- ggplot(ssb_probs, aes(x=nonIBMlab, y=value)) + 
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Non-IBM Scenario", y="Probability", title = "SSB") 
#ggsave(filename = "demonstrations/chris/demo_plots/ssb_probs_box_nonIBM.png", box_ssb2)

box_ssb3 <- ggplot(ssb_means, aes(x=IBMlab, y=value)) +
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="IBM", y="SSB/SSBmsy") 

box_ssb4 <- ggplot(ssb_means, aes(x=nonIBMlab, y=value)) +
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Non-IBM Scenario", y="SSB/SSBmsy") 

which_rebuild <- ssb_probs %>%
  filter(metric == "l_is_ge_bmsy", value >= 0.9) 
which_rebuild$retro_type
which_rebuild$IBMlab
which_rebuild$nonIBMlab

which_crash <- ssb_probs %>%
  filter(metric == "l_is_less_01_bmsy", value >= 0.90) 
which_crash$retro_type
which_crash$IBMlab
which_crash$nonIBMlab

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

f_means <- f_results %>%
  group_by(iscen, metric) %>%
  summarise_all(mean) %>%
  filter(grepl("_avg_f_fmsy", metric)) %>%
  inner_join(., defined)

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
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs.png", f_probs_plot)

# color code to show factors
p1 <- f_probs_plot + geom_point(aes(color = retro_type))
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs_retro_type.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = IBMlab))
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs_IBMlab.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = factor(Fhist)))
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs_Fhist.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = factor(n_selblocks)))
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs_n_selblocks.png", p1)
p1 <- f_probs_plot + geom_point(aes(color = factor(catch.mult)))
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs_catch.mults.png", p1)

box_f1 <- ggplot(f_probs, aes(x=IBMlab, y=value)) + 
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="IBM", y="Probability", title = "F") 
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs_box_IBM.png", box_f1)

box_f2 <- ggplot(f_probs, aes(x=nonIBMlab, y=value)) + 
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Non-IBM Scenario", y="Probability", title = "F") 
#ggsave(filename = "demonstrations/chris/demo_plots/f_probs_box_nonIBM.png", box_f2)

box_f3 <- ggplot(f_means, aes(x=IBMlab, y=value)) +
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="IBM", y="F/Fmsy") 

box_f4 <- ggplot(f_means, aes(x=nonIBMlab, y=value)) +
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Non-IBM Scenario", y="F/Fmsy") 


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
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy.png", catch_msy_plot)

# color code to show factors
p1 <- catch_msy_plot + geom_point(aes(color = retro_type))
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_retro_type.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = IBMlab))
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_IBMlab.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = factor(Fhist)))
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_Fhist.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = factor(n_selblocks)))
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_n_selblocks.png", p1)
p1 <- catch_msy_plot + geom_point(aes(color = factor(catch.mult)))
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_catch.mults.png", p1)

box_catch1 <- ggplot(catch_means, aes(x=IBMlab, y=value)) + 
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="IBM", y="Ratio", title = "Catch/MSY") 
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_IBM.png", box_catch1)

box_catch2 <- ggplot(catch_means, aes(x=nonIBMlab, y=value)) + 
  geom_boxplot() +
  facet_wrap(~metric) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="Non-IBM Scenario", y="Ratio", title = "Catch/MSY") 
#ggsave(filename = "demonstrations/chris/demo_plots/catch_msy_nonIBM.png", box_catch2)


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
#ggsave(filename = "demonstrations/chris/demo_plots/tradeoff1.png", td1_plot)

td2 <- td %>%
  filter(ssb_metric == "l_is_less_05_bmsy",
         f_metric == "l_is_gr_fmsy")

td2_plot <- ggplot(td2, aes(x=ssb_value, y=f_value, color=retro_type)) +
  geom_point() +
  facet_wrap(~IBMlab) +
  labs(x="Prob(SSB<0.5SSBmsy)", y="Prob(F>Fmsy)") +
  theme_bw()
#ggsave(filename = "demonstrations/chris/demo_plots/tradeoff2.png", td2_plot)

names(ssb_means)
names(catch_means)
ssb2_temp <- ssb_means %>%
  filter(metric == "l_avg_ssb_ssbmsy") %>%
  rename(ssb_metric = metric, ssb_value = value)
catch2_temp <- catch_temp %>%
  filter(catch_metric == "l_avg_catch_msy")
td3 <- inner_join(ssb2_temp, catch2_temp)
td3_plot <- ggplot(td3, aes(x=ssb_value, y=catch_value)) +
  geom_point() +
  facet_wrap(~IBM) +
  labs(x="SSB/SSBmsy", y="Catch/MSY", title="Longterm") +
  theme_bw()

# put plots so far into pdf
pdf(file = "demonstrations/chris/demo_plots/demo_make_tables_figures.pdf")
box_ssb1
box_ssb2
box_ssb3
box_ssb4
ssb_probs_plot
ssb_probs_plot + geom_point(aes(color = retro_type))
ssb_probs_plot + geom_point(aes(color = IBMlab))
ssb_probs_plot + geom_point(aes(color = factor(Fhist)))
ssb_probs_plot + geom_point(aes(color = factor(n_selblocks)))
ssb_probs_plot + geom_point(aes(color = factor(catch.mult)))
box_f1
box_f2
box_f3
box_f4
f_probs_plot
f_probs_plot + geom_point(aes(color = retro_type))
f_probs_plot + geom_point(aes(color = IBMlab))
f_probs_plot + geom_point(aes(color = factor(Fhist)))
f_probs_plot + geom_point(aes(color = factor(n_selblocks)))
f_probs_plot + geom_point(aes(color = factor(catch.mult)))
box_catch1
box_catch2
catch_msy_plot
catch_msy_plot + geom_point(aes(color = retro_type))
catch_msy_plot + geom_point(aes(color = IBMlab))
catch_msy_plot + geom_point(aes(color = factor(Fhist)))
catch_msy_plot + geom_point(aes(color = factor(n_selblocks)))
catch_msy_plot + geom_point(aes(color = factor(catch.mult)))
td1_plot
td2_plot
td3_plot
dev.off()




