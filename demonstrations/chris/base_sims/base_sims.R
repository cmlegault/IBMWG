# base_sims.R
# make plots of SSB distributions in base period

library(tidyverse)

# rds file available in Google Drive IBMWG/ResultFiles folder
# copy from Google Drive and place in mydir
mydir <- "C:/Users/chris.legault/Desktop/myIBMWG/basesims"
dat <- readRDS(file.path(mydir, "base_sims.rds"))
dat
names(dat)
names(dat$base[[1]])
dat$base[[1]]$SSB

Fhistlab <- c("F","O") # F = Fmsy in second half of base period, O = overfishingthroughout entire base period 

dat1 <- filter(dat, isim == 1)
nres <- dim(dat1)[1]
for (i in 1:nres){
  thisdat <- tibble(iscen = dat1$iscen[i],
                    isim = dat1$isim[i],
                    retro_type = dat1$retro_type[i],
                    Fhist = Fhistlab[dat1$Fhist[i]],
                    n_selblocks = dat1$n_selblocks[i],
                    period = c(1, 50),
                    ssbmsy = c(exp(dat1$base[[i]]$log_SSB_MSY[1]),
                               exp(dat1$base[[i]]$log_SSB_MSY[50])),
                    fmsy = c(exp(dat1$base[[i]]$log_FMSY[1]),
                             exp(dat1$base[[i]]$log_FMSY[50])),
                    msy = c(exp(dat1$base[[i]]$log_MSY[1]),
                            exp(dat1$base[[i]]$log_MSY[50])))
  if (i == 1){
    refpts <- thisdat
  }else{
    refpts <- rbind(refpts, thisdat)
  }
}
refpts

yy <- dat %>%
  mutate(ssb = map(base, ~pluck(.x$SSB[1:50])),
         f = map(base, ~pluck(.x$Fbar[1:50])),
         catch = map(base, ~pluck(.x$pred_catch[1:50]))) %>%
  select(rowid, iscen, isim, IBM, Fhist, n_selblocks, retro_type, catch_mult, ssb, f, catch) 

myssb <- yy %>%
  select(rowid, iscen, isim, IBM, Fhist, n_selblocks, retro_type, catch_mult, ssb) %>%
  mutate(ssb = map(ssb, enframe)) %>%
  unnest(cols = c(ssb)) %>%
  rename(ssb = value) %>%
  rename(year = name) %>%
  mutate(year = year + 1969)

myf <- yy %>%
  select(rowid, iscen, isim, IBM, Fhist, n_selblocks, retro_type, catch_mult, f) %>%
  mutate(f = map(f, enframe)) %>%
  unnest(cols = c(f)) %>%
  rename(f = value) %>%
  rename(year = name) %>%
  mutate(year = year + 1969)

mycatch <- yy %>%
  select(rowid, iscen, isim, IBM, Fhist, n_selblocks, retro_type, catch_mult, catch) %>%
  mutate(catch = map(catch, enframe)) %>%
  unnest(cols = c(catch)) %>%
  rename(catch = value) %>%
  rename(year = name) %>%
  mutate(year = year + 1969)


quibble <- function(x, q = c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  tibble(x = quantile(x, q, na.rm = TRUE), q = q)
}

myssb_sum <- myssb %>%
  group_by(iscen, year) %>%
  summarise(y = list(quibble(ssb, c(0.05, 0.25, 0.5, 0.75, 0.95)))) %>% 
  unnest(y) %>% 
  I() %>%
  inner_join(filter(refpts, period == 1), by = "iscen") %>%
  mutate(refpt = ssbmsy)

myssb_sum_wide <- myssb_sum %>%
  mutate(q = paste0("q", q)) %>%
  pivot_wider(names_from = q, values_from = x) 
  
myf_sum <- myf %>%
  group_by(iscen, year) %>%
  summarise(y = list(quibble(f, c(0.05, 0.25, 0.5, 0.75, 0.95)))) %>% 
  unnest(y) %>% 
  I() %>%
  inner_join(filter(refpts, period == 1), by = "iscen") %>%
  mutate(refpt = fmsy)

myf_sum_wide <- myf_sum %>%
  mutate(q = paste0("q", q)) %>%
  pivot_wider(names_from = q, values_from = x) 

mycatch_sum <- mycatch %>%
  group_by(iscen, year) %>%
  summarise(y = list(quibble(catch, c(0.05, 0.25, 0.5, 0.75, 0.95)))) %>% 
  unnest(y) %>% 
  I() %>%
  inner_join(filter(refpts, period == 1), by = "iscen") %>%
  mutate(refpt = msy)

mycatch_sum_wide <- mycatch_sum %>%
  mutate(q = paste0("q", q)) %>%
  pivot_wider(names_from = q, values_from = x) 


descdf <- data.frame(x=rep(1, 5),
                     y=seq(5, 1, -1),
                     z=c("MSY reference point (blue line) from 1970 conditions",
                         "Fhist: F = Fmsy in second period, O = Overfishing throughout",
                         "Selbocks: 1, 2",
                         "Retro source: Catch, M",
                         "Dark grey fill 90% CI, light grey fill inner quartile, solid line median"))

descplot <- ggplot(descdf, aes(x=x, y=y)) +
  geom_text(aes(label = z)) +
  expand_limits(y=c(0,6)) +
  labs(title = "Plots explained") +
  theme_void()

make_base_plots <- function(mytibble, myylab, myylab2){
  myplot <- ggplot(mycatch_sum_wide, aes(x=year)) +
    geom_ribbon(aes(ymin = q0.05, ymax=q0.95), fill="grey25") +
    geom_ribbon(aes(ymin = q0.25, ymax=q0.75), fill="grey90") +
    geom_line(aes(y=q0.5), size=1.1) +
    geom_line(aes(y = refpt), color = "blue") +
    facet_grid(retro_type~Fhist+n_selblocks) +
    labs(x="Year", y=myylab) +
    expand_limits(y=0) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  
  myrelplot <- ggplot(mycatch_sum_wide, aes(x=year)) +
    geom_ribbon(aes(ymin = q0.05/refpt, ymax=q0.95/refpt), fill="grey25") +
    geom_ribbon(aes(ymin = q0.25/refpt, ymax=q0.75/refpt), fill="grey90") +
    geom_line(aes(y=q0.5/refpt), size=1.1) +
    geom_hline(yintercept = 1.0, color = "blue") +
    facet_grid(retro_type~Fhist+n_selblocks) +
    labs(x="Year", y=myylab2) +
    expand_limits(y=0) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  
  myplots <- list(myplot = myplot,
                  myrelplot = myrelplot)
  return(myplots)
}

ssb_plots <- make_base_plots(myssb_sum_wide, "SSB", "SSB/SSBmsy")

f_plots <- make_base_plots(myf_sum_wide, "F", "F/Fmsy")

catch_plots <- make_base_plots(mycatch_sum_wide, "Catch", "Catch/MSY")

pdf(file = "demonstrations/chris/base_sims/base_sims_plots.pdf")
descplot

ssb_plots$myplt
f_plots$myplot
catch_plots$myplot

ssb_plots$myrelplt
f_plots$myrelplot
catch_plots$myrelplot

dev.off()
