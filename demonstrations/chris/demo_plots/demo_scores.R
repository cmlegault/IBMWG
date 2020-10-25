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

get_mean_scores <- function(mywide){
  mymeans <- mywide %>%
    pivot_longer(cols = !metric, names_to = "IBM", values_to = "score") %>%
    group_by(IBM) %>%
    summarize(meanscore = mean(score))
  return(mymeans)
}

ssb_means <- get_mean_scores(ssb_scores) %>%
  rename("ssb" = "meanscore")

ssb_means_l <- get_mean_scores(filter(ssb_scores, substr(metric, 5, 5) == "l")) %>%
  rename("ssb_l" = "meanscore")

ssb_means_s <- get_mean_scores(filter(ssb_scores, substr(metric, 5, 5) == "s")) %>%
  rename("ssb_s" = "meanscore")

f_means <- get_mean_scores(f_scores) %>%
  rename("f" = "meanscore")

f_means_l <- get_mean_scores(filter(f_scores, substr(metric, 3, 3) == "l")) %>%
  rename("f_l" = "meanscore")

f_means_s <- get_mean_scores(filter(f_scores, substr(metric, 3, 3) == "s")) %>%
  rename("f_s" = "meanscore")

catch_means <- get_mean_scores(catch_scores) %>%
  rename("catch" = "meanscore")

catch_means_l <- get_mean_scores(filter(catch_scores, substr(metric, 7, 7) == "l")) %>%
  rename("catch_l" = "meanscore")

catch_means_s <- get_mean_scores(filter(catch_scores, substr(metric, 7, 7) == "s")) %>%
  rename("catch_s" = "meanscore")

all_means <- inner_join(ssb_means, f_means, by = "IBM") %>%
  inner_join(catch_means, by = "IBM") %>%
  mutate(mean_of_means = (ssb + f + catch) / 3) %>%
  pivot_longer(cols = !IBM, names_to = "source", values_to = "score") 

all_means_l <- inner_join(ssb_means_l, f_means_l, by = "IBM") %>%
  inner_join(catch_means_l, by = "IBM") %>%
  mutate(mean_of_means_l = (ssb_l + f_l + catch_l) / 3) %>%
  pivot_longer(cols = !IBM, names_to = "source", values_to = "score") 

all_means_s <- inner_join(ssb_means_s, f_means_s, by = "IBM") %>%
  inner_join(catch_means_s, by = "IBM") %>%
  mutate(mean_of_means_s = (ssb_s + f_s + catch_s) / 3) %>%
  pivot_longer(cols = !IBM, names_to = "source", values_to = "score") 

score_plot <- ggplot(all_means, aes(x=reorder(IBM, score), y=score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~source) +
  labs(x="IBM", y="Score (bigger is better)", title="All Metrics") +
  theme_bw()
print(score_plot)
#ggsave(filename = "demonstrations/chris/demo_plots/score_plot.png", score_plot)

score_plot_l <- ggplot(all_means_l, aes(x=reorder(IBM, score), y=score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~source) +
  labs(x="IBM", y="Score (bigger is better)", title="Long Term Metrics Only") +
  theme_bw()
print(score_plot_l)
#ggsave(filename = "demonstrations/chris/demo_plots/score_plot_l.png", score_plot_l)

score_plot_s <- ggplot(all_means_s, aes(x=reorder(IBM, score), y=score)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~source) +
  labs(x="IBM", y="Score (bigger is better)", title="Short Term Metrics Only") +
  theme_bw()
print(score_plot_s)
#ggsave(filename = "demonstrations/chris/demo_plots/score_plot_s.png", score_plot_s)

### put plots into pdf
pdf(file = "demonstrations/chris/demo_plots/demo_scores.pdf")
score_plot
score_plot_l
dev.off()

score_plot_s