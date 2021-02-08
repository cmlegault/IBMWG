# branch_sims.R
# make time series plots of SSB, F, R, and catch for all IBMs for a given simulation of a given scenario

library(tidyverse)

# rds file available in Google Drive IBMWG/ResultFiles folder
mydir <- "C:/Users/chris.legault/Desktop/myIBMWG/branchsims"
#dat <- readRDS(file.path(mydir, "branch_res.rds"))
dat <- readRDS(file.path(mydir, "branch_res2.rds"))
dat

# define runs
Fhistlab <- c("F","O") # O = always overfishing, F = Fmsy in last year of base
Sellab <- c("1", "2") # just whether 1 or 2 blocks for selectivity
CMlab <- c("A", "R") # A = catch advice applied, R = reduced (mult=0.75)
EMlab <- c("FSPR", "Fstable", "FM", "Frecent")
CClab <- c("FSPR", NA, "FM")
defined <- dat %>%
  select(iscen, specs) %>%
  unnest(cols = specs) %>%
  mutate(IBMlab = case_when(
    IBM == "true_Skate_CR" ~ "Skate",
    IBM == "M_CC" ~ paste("CC", CClab[M_CC_method], sep="-"),
    IBM == "planBsmoothfxn" ~ "PBS",
    IBM == "ExpandSurvey_modified" ~ paste("ES", EMlab[expand_method], sep="-"),
    IBM == "run.aim" ~ "AIM",
    IBM == "JoeDLM" ~ "DLM",
    IBM == "ensemble" ~ "Ensemble",
    TRUE ~ IBM),
    Scenlab = paste(substr(retro_type, 1, 1), Fhistlab[Fhist], Sellab[n_selblocks], ifelse(catch.mult == 1, CMlab[1], CMlab[2]), sep = ""))
defined
defined$IBMlab

yy <- dat %>%
  mutate(ssb = map(wham, ~pluck(.x$result$true_sim$SSB)),
         f = map(wham, ~pluck(.x$result$true_sim$FAA_tot[,10])),
         rec = map(wham, ~pluck(.x$result$true_sim$NAA[,1])),
         catch = map(wham, ~pluck(.x$result$true_sim$pred_catch[,1]))) %>%
  select(iscen, isim, seed, ssb, f, rec, catch) %>%
  inner_join(defined, by = "iscen")
yy

# find duplicated runs
dupes <- duplicated(yy[,-1])
zz <- yy[!dupes, ]

# pull out the time series
ssb_ts <- zz %>% 
  select(iscen, isim, IBMlab, ssb) %>% 
  mutate(ssb = map(ssb, enframe)) %>% 
  unnest(cols = c(ssb)) %>% 
  mutate(year = name + 1969,
         ts = "ssb") %>%
  I()
ssb_ts

f_ts <- zz %>% 
  select(iscen, isim, IBMlab, f) %>% 
  mutate(f = map(f, enframe)) %>% 
  unnest(cols = c(f)) %>% 
  mutate(year = name + 1969,
         ts = "f") %>%
  I()
f_ts

rec_ts <- zz %>% 
  select(iscen, isim, IBMlab, rec) %>% 
  mutate(rec = map(rec, enframe)) %>% 
  unnest(cols = c(rec)) %>% 
  mutate(year = name + 1969,
         ts = "rec") %>%
  I()
rec_ts

catch_ts <- zz %>% 
  select(iscen, isim, IBMlab, catch) %>% 
  mutate(catch = map(catch, enframe)) %>% 
  unnest(cols = c(catch)) %>% 
  mutate(year = name + 1969,
         ts = "catch") %>%
  I()
catch_ts

all_ts <- rbind(ssb_ts, f_ts, rec_ts, catch_ts)

# make plot
ts_plot <- ggplot(all_ts, aes(x=year, y=value, group=IBMlab)) +
  geom_line(color = "grey75") +
  facet_wrap(~ts, scales = "free_y") +
  expand_limits(y = 0) +
  labs(x="Year", y="Value") +
  theme_bw()
ts_plot

ibms <- sort(unique(all_ts$IBMlab))
nibm <-  length(ibms)
mult_ts_plot <- list()
for (i in 1:nibm){
  sub_ts <- filter(all_ts, IBMlab == ibms[i])
  mult_ts_plot[[i]] <- ts_plot +
    geom_line(data=sub_ts, aes(x=year, y=value), color = "blue") +
    ggtitle(ibms[i]) +
    theme(plot.title = element_text(colour = "blue"))
}

#pdf(file = "demonstrations/tim/branch_sims/branch_sims.pdf")
pdf(file = "demonstrations/tim/branch_sims/branch_sims2.pdf")
walk(mult_ts_plot, print)
dev.off()

# save png files to allow animation
for (i in 1:length(mult_ts_plot)){
  png(filename = paste0(file.path(mydir, paste0("mult_ts_plot", i, ".png"))))
  print(mult_ts_plot[i])
  dev.off()
}
