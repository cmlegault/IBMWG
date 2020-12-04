# get_catch_curve_example.R

library(tidyverse)

dbfile <- readRDS("results/dbfile.rds")
dbfile
dbrowid <- readRDS("results/dbrowid.rds")
dbrowid

mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")

# remove duplicate runs
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]

Fhistlab <- c("F","O") # O = always overfishing, F = Fmsy in last year of base
Sellab <- c("1", "2") # just whether 1 or 2 blocks for selectivity
CMlab <- c("A", "R") # A = catch advice applied, R = reduced (mult=0.75)
EMlab <- c("FSPR", "Fstable", "FM", "Frecent")
CClab <- c("FSPR", NA, "FM")
defined <- mse_sim_setup %>%
  filter(rowid %in% not_dupes) %>% 
  filter(isim == 1) %>%
  select(iscen, specs) %>%
  unnest(cols = specs) %>%
  mutate(IBMlab = case_when(
    IBM == "true_Skate_CR" ~ "Skate",
    IBM == "M_CC" ~ paste("CC", CClab[M_CC_method], sep="-"),
    IBM == "planBsmoothfxn" ~ "PlanB",
    IBM == "ExpandSurvey_modified" ~ paste("ES", EMlab[expand_method], sep="-"),
    IBM == "run.aim" ~ "AIM",
    IBM == "JoeDLM" ~ "DLM",
    IBM == "ensemble" ~ "Ensemble",
    TRUE ~ IBM),
    Scenlab = paste(substr(retro_type, 1, 1), Fhistlab[Fhist], Sellab[n_selblocks], ifelse(catch.mult == 1, CMlab[1], CMlab[2]), sep = ""))
defined

# get catch curve models
ccruns <- defined %>%
  filter(IBMlab == "CC-FSPR", Scenlab == "CF1A")
ccruns$iscen

myruns <- dbrowid %>%
  filter(iscen == ccruns$iscen, isim == 14)

mylink <- dbfile %>%
  filter(link %in% myruns$link)

unique(mylink$location)

myruns %>%
  filter(link == mylink$link[1])

# need to convert "/net" to "//net.nefsc.noaa.gov"
mytemp <- "//net.nefsc.noaa.gov/home0/pdy/lbrooks/Git/IBMWG/output"

mydat <- readRDS(file.path(mytemp, mylink$file[1]))
mydat

wham <- mydat %>%
  filter(iscen == ccruns$iscen, isim == 14) %>%
  select(wham)

res <- wham$wham[[1]]$result

res$advice
ind2 <- res$observed_sim$index_naa[48:50,,2]
ind2s <- apply(ind2, 2, sum)
ccdat <- data.frame(age = 1:10,
                    lnobs = log(ind2s),
                    use = c(F, F, rep(T, 8)))
ccdat

ccuse <- ccdat %>%
  filter(use == TRUE)
mylm <- lm(lnobs ~ age, data=ccuse)

ccdat <- ccdat %>%
  mutate(pred = mylm$coefficients[1] + mylm$coefficients[2] * age)

ccplot <- ggplot(ccdat, aes(x=age, y=lnobs, color=use)) +
  geom_point() +
  geom_line(aes(x=age, y=pred)) +
  labs(x="Age", y="Log(Survey Index at Age)", title = paste("Z =", round(-1 * mylm$coefficients[2], 2))) +
  scale_x_continuous(breaks = seq(2, 10, 2), labels = c("2", "4", "6", "8", "10+")) +
  theme_bw()
ggsave(filename = "demonstrations/chris/basics/cc.png", ccplot, width=5, height = 4, units = "in")
