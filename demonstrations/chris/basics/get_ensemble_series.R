# get_ensemble_series.R

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
    IBM == "planBsmoothfxn" ~ "PBS",
    IBM == "ExpandSurvey_modified" ~ paste("ES", EMlab[expand_method], sep="-"),
    IBM == "run.aim" ~ "AIM",
    IBM == "JoeDLM" ~ "DLM",
    IBM == "ensemble" ~ "Ensemble",
    TRUE ~ IBM),
    Scenlab = paste(substr(retro_type, 1, 1), Fhistlab[Fhist], Sellab[n_selblocks], ifelse(catch.mult == 1, CMlab[1], CMlab[2]), sep = ""))
defined

# get Ensemble models
ensembleruns <- defined %>%
  filter(IBMlab == "Ensemble", Scenlab == "CF1A")
ensembleruns$iscen

myruns <- dbrowid %>%
  filter(iscen == 107, isim <= 10)

mylink <- dbfile %>%
  filter(link %in% myruns$link)

myruns %>%
  filter(link == mylink$link[6])

mydat <- readRDS(file.path(mylink$location[6], mylink$file[6]))
mydat

wham <- mydat %>%
  filter(iscen == 107, isim <= 10) %>%
  select(wham)

res <- wham$wham[[5]]$result

res$advice

advice <- matrix(NA, nrow = 21, ncol = 9)
for (i in 1:21){
  thisadvice <- res$advice[[i]][[1]]
  advice[i, 1] <- thisadvice
  if (thisadvice != 2){
    advice[i, 2:9] <- res$advice[[i]][[2]]
  }
}
advice
colnames(advice) <- c("Ensemble", "Islope", "Itarget", "Skate", "PBS", "AIM", "CC-FSPR", "ES-FSPR", "ES-Frecent")
advice <- data.frame(advice)
advice$Year <- seq(-1, 40, 2)
advicelong <- advice %>%
  pivot_longer(cols = -Year, names_to = "Method", values_to = "value") %>%
  filter(Year >= 20)
advicelong

ensembleplot <- ggplot(advicelong, aes(x=Year, y=value, color=Method)) +
  geom_point() +
  geom_line(data=filter(advicelong, Method == "Ensemble"), aes(x=Year, y=value)) +
  labs(x="Projection Year", y="Catch Advice", title="CF1A sim 5") +
  theme_bw()
ggsave(filename = "demonstrations/chris/basics/ensemble.png", ensembleplot, width = 5, height = 4, units = "in")
