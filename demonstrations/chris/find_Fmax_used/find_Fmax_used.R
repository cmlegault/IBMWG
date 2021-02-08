# find_Fmax_used.R
# don't have time to look at all simulations, so just use ones readily available for now

library(tidyverse)

mydir <- "C:\\Users\\chris.legault\\Desktop\\myIBMWGruns"
myfiles <- list.files(mydir) 
mymse <- myfiles[substr(myfiles, 1, 4) == "mse-"]
nmse <- length(mymse)

myres2 <- tibble(iscen = integer(),
                 isim = integer(),
                 nFmax = integer(),
                 Fmaxinvoked = logical())

myids <- seq(51, 65)
nfiles <- length(myids)
for (i in myids){
  mydat <- readRDS(file.path(mydir, mymse[i]))
  myres <- mydat %>%
    mutate(advice = map(wham, ~pluck(.x$result$advice))) %>%
    select(iscen, isim, advice)
  
  for (k in 1:dim(myres)[1]){
    myad <- rep(NA, 21)
    for (j in 1:21){
      myad[j] <- myres$advice[[k]][[j]][[1]]
    }
    myFmax <- myad[myad == 2.0]
    nFmax <- length(myFmax)
    Fmaxinvoked <- ifelse(nFmax == 0, FALSE, TRUE)
    thisres <- tibble(iscen = myres$iscen[k],
                      isim = myres$isim[k],
                      nFmax = nFmax,
                      Fmaxinvoked = Fmaxinvoked)
    myres2 <- rbind(myres2, thisres)
  }
  print(paste("file", mymse[i], i, "had", dim(myres)[1], "simulations"))
  print(table(myres2$Fmaxinvoked))
}
myres2

myres3 <- myres2 %>%
  distinct() %>%
  group_by(iscen, Fmaxinvoked) %>%
  summarise(n = n()) %>%
  mutate(Fmaxinv = paste0("is", Fmaxinvoked)) %>%
  select(iscen, Fmaxinv, n) %>%
  pivot_wider(names_from = Fmaxinv, values_from = n, values_fill = 0)
myres3

# get labels for scenarios
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
def3 <- defined %>%
  select(iscen, IBMlab, Scenlab)

myres4 <- inner_join(myres3, def3, by = "iscen") %>%
  mutate(total = isFALSE + isTRUE,
         fracTRUE = isTRUE / (isFALSE + isTRUE)) %>%
  filter(total > 10)

Fmaxusedplot <- ggplot(myres4, aes(x = fracTRUE, y=Scenlab, color=IBMlab)) +
  geom_point() +
  facet_wrap(~IBMlab, nrow = 2) +
  labs(x="Fraction of Simulations with maxF Invoked", y="") +
  theme_bw() + 
  theme(legend.position = "none")
ggsave(filename = "demonstrations/chris/find_Fmax_used/Fmaxused.png", Fmaxusedplot, width=8, height=6.5, units = "in")

nplot <- ggplot(myres4, aes(x = total, y=Scenlab, color=IBMlab)) +
  geom_point() +
  facet_wrap(~IBMlab, nrow = 2) +
  labs(x="Total Simulations Examined in This Analysis", y="") +
  theme_bw() + 
  theme(legend.position = "none")
ggsave(filename = "demonstrations/chris/find_Fmax_used/nused.png", nplot, width=8, height=6.5, units = "in")
