# demo_plots.R
# demonstration plots for summary of IBMWG work using fake data

# set working directory to source file location to begin

library(ggplot2)
library(dplyr)
library(tidyr)
library(umap)

# make repeatable
set.seed(14)

# design matrix has 13 IBMs, 4 retro sources, 2 fishing history, 2 fishery selectivity,  and 2 catch multipliers for a total of 416 scenarios
# there are an additional 24 one off analyses
nibm <- 13
nretro <- 4
nfh <- 2
nfs <- 2
ncm <- 2

# compare 13 IBMs for each of the 32 scenarios
fd <- data.frame(IBM = factor(1:nibm),
                 period = rep(c("short-term", "long-term"), each = nibm),
                 catch = c(rnorm(nibm, 100, 10), rnorm(nibm, 400, 40)),
                 ssb = c(rnorm(nibm, 1000, 100), rnorm(nibm, 2000, 200))) %>%
  transform(period = factor(period, levels = c("short-term", "long-term")))
fd

p <- ggplot(fd, aes(x=catch, y=ssb, color=IBM)) +
  geom_point() +
  facet_wrap(~period) +
  expand_limits(x=0, y=0) +
  labs(title = "Specific Scenario", subtitle = paste(nretro*nfh*nfs*ncm," of these plots")) +
  theme_bw()
print(p)
ggsave(filename = "catch_ssb_for_scenario.png", p)

# compare performance of one IBM across scenarios
retrosource <- c("Missing Catch", 
                 "Increased M", 
                 "Changed Survey q", 
                 "Selectivity Changes") 

fhistory <- c("always overfishing", 
              "overfishing then fmsy")

fisheryselectivity <- c("Constant", 
                        "Time-varying")

catchmultiplier <- c(1.00, 
                     0.75)
nrows <- nretro * nfh * nfs * ncm

fd <- expand.grid(retrosource, fhistory, fisheryselectivity, catchmultiplier) %>%
  mutate(retrosource = factor(Var1),
         fhistory = factor(Var2),
         selectivity = factor(Var3),
         Cmult = factor(Var4))
fd <- rbind(fd, fd) 
fd <- fd %>%
  mutate(period = rep(c("short-term", "long-term"), each = nrows),
         catch = c(rnorm(nrows, 100, 10), rnorm(nrows, 400, 40)),
         ssb = c(rnorm(nrows, 1000, 100), rnorm(nrows, 2000, 200))) 
fd <- fd %>%
  transform(period = factor(period, levels = c("short-term", "long-term")))
fd

p <- ggplot(fd, aes(x=catch, y=ssb, color=Cmult, size=fhistory, shape=selectivity)) +
  geom_point() +
  facet_grid(period~retrosource) +
  scale_shape_manual(values=c(1, 2)) +  
  expand_limits(x=0, y=0) +
  labs(title = "Specific IBM", subtitle = paste(nibm, "of these plots")) +
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +  
  theme_bw()
print(p)
ggsave(filename = "catch_ssb_for_IBM.png", p)

# compare probability of overfishing and overfished by IBM for each scenario
ibms <- c("Islope", 
          "Itarget", 
          "true_Skate_CR", 
          "M_CC", 
          "PlanBsmooth", 
          "ExpandSurvey_recentavgrelF", 
          "ExpandSurvey_F%SPR",
          "ExpandSurvey_stableperiodF",
          "AIM", 
          "CatchCurve_Fspr",
          "CatchCurve_stableperiod",
          "JoeDLM",
          "Ensemble")
fd <- data.frame(IBM = ibms,
                 period = rep(c("short-term", "long-term"), each = nibm),
                 metric = rep(c("overfished", "overfishing"), each = nibm * 2),
                 value = c(runif(nibm, 0.8, 1.0), runif(nibm, 0.1, 0.8), 
                           runif(nibm, 0.4, 1.0), runif(nibm, 0.1, 1.0))) %>%
  transform(period = factor(period, levels = c("short-term", "long-term")))
fd 
p <- ggplot(fd, aes(x=value, y=IBM)) +
  geom_point() +
  facet_grid(period~metric) +
  expand_limits(x=0) +
  labs(title = "Specific Scenario", subtitle = paste(nretro*nfh*nfs*ncm," of these plots")) +
  theme_bw()
print(p)
ggsave(filename = "status_for_scenario.png", p)

# compare two scenarios including realization uncertainty
# could be 2 IBMs, or two retrosources, or catch multiplier 1 vs 0.75, etc.
ny <- 40
ssb1 <- rep(1000, ny)
ssb2 <- ssb1
f1 <- rep(0.5, ny)
f2 <- f1
for (i in 2:ny){
  ssb1[i] <- ssb1[i-1] * exp(rnorm(1, 0, 0.1))
  ssb2[i] <- ssb2[i-1] * exp(rnorm(1, 0.05, 0.1))
  f1[i] <- f1[i-1] * exp(rnorm(1, 0, 0.02))
  f2[i] <- f2[i-1] * exp(rnorm(1, -0.01, 0.02))
}
c1 <- ssb1 * f1
c2 <- ssb2 * f2
fd <- data.frame(Year = 2021:2060,
                 scenario = factor(rep(c(1,2), each = ny)),
                 metric = rep(c("Catch", "SSB", "F"), each = ny * 2),
                 value = c(c1, c2, ssb1, ssb2, f1, f2)) %>%
  mutate(upperCI = value * (1 + 1.96 * runif(40 * 2 * 3, 0.08, 0.12)),
         lowerCI = value * (1 - 1.96 * runif(40 * 2 * 3, 0.08, 0.12)))
fd
p <- ggplot(fd, aes(x=Year, y=value, color=scenario)) +
  geom_line() +
  geom_ribbon(aes(ymin = lowerCI, ymax = upperCI, fill=scenario), alpha = 0.2) +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  expand_limits(y=0) +
  theme_bw()
print(p)
ggsave(filename = "compare_two_scenarios.png", p)


# approach from Andy Jones using Uniform Manifold Approximation and Projection package (umap)

#converting the data to a wide format
fd_wide <- fd %>% pivot_wider(names_from = metric, values_from = c(value,upperCI,lowerCI))

#trying out a generic UMAP on three response variables (catch,SSB,F)
umap_fd <- umap(fd_wide[,3:5] %>% scale())

#plotting out the results
#facting by scenario and adding a color for year
fd_wide %>%
  mutate(id = row_number()) %>%
  bind_cols(umap_fd$layout %>% as_tibble()) %>%
  ggplot(.,aes(x=V1,y=V2,colour=Year)) + geom_point() + facet_wrap(~scenario)
ggsave(filename = "umap_idea.png")
