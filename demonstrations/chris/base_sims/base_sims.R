# base_sims.R
# make plots of SSB distributions in base period

library(tidyverse)

# rds file available in Google Drive IBMWG/ResultFiles folder
# copy from Google Drive and place in mydir
mydir <- "C:\\Users\\chris.legault\\Documents\\Working\\Index-based Research Track 2020\\myIBMWG\\basesims"
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
                    ssbmsy1 = exp(dat1$base[[i]]$log_SSB_MSY[1]),
                    ssbmsy50 = exp(dat1$base[[i]]$log_SSB_MSY[50]),
                    fmsy1 = exp(dat1$base[[i]]$log_FMSY[1]),
                    fmsy50 = exp(dat1$base[[i]]$log_FMSY[50]),
                    msy1 = exp(dat1$base[[i]]$log_MSY[1]),
                    msy50 = exp(dat1$base[[i]]$log_MSY[50]),
                    F50 = dat1$base[[i]]$F[50])
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
  inner_join(refpts, by = "iscen") %>%
  mutate(refpt1 = ssbmsy1,
         refpt50 = ssbmsy50)
  
myssb_sum_wide <- myssb_sum %>%
  mutate(q = paste0("q", q)) %>%
  pivot_wider(names_from = q, values_from = x) 
  
myf_sum <- myf %>%
  group_by(iscen, year) %>%
  summarise(y = list(quibble(f, c(0.05, 0.25, 0.5, 0.75, 0.95)))) %>% 
  unnest(y) %>% 
  I() %>%
  inner_join(refpts, by = "iscen") %>%
  mutate(refpt1 = fmsy1,
         refpt50 = fmsy50)

myf_sum_wide <- myf_sum %>%
  mutate(q = paste0("q", q)) %>%
  pivot_wider(names_from = q, values_from = x) 

mycatch_sum <- mycatch %>%
  group_by(iscen, year) %>%
  summarise(y = list(quibble(catch, c(0.05, 0.25, 0.5, 0.75, 0.95)))) %>% 
  unnest(y) %>% 
  I() %>%
  inner_join(refpts, by = "iscen") %>%
  mutate(refpt1 = msy1,
         refpt50 = msy50)

mycatch_sum_wide <- mycatch_sum %>%
  mutate(q = paste0("q", q)) %>%
  pivot_wider(names_from = q, values_from = x) 


descdf <- data.frame(x=rep(1, 5),
                     y=seq(5, 1, -1),
                     z=c("MSY reference point 1970 (blue line) or 2019 (red line) conditions",
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
  myplot <- ggplot(mytibble, aes(x=year)) +
    geom_ribbon(aes(ymin = q0.05, ymax=q0.95), fill="grey25") +
    geom_ribbon(aes(ymin = q0.25, ymax=q0.75), fill="grey90") +
    geom_line(aes(y=q0.5), size=1.1) +
    geom_line(aes(y = refpt1), color = "blue") +
    geom_line(aes(y = refpt50), color = "red", linetype = "dashed") +
    facet_grid(retro_type~Fhist+n_selblocks) +
    labs(x="Year", y=myylab) +
    expand_limits(y=0) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  
  myrelplot1 <- ggplot(mytibble, aes(x=year)) +
    geom_ribbon(aes(ymin = q0.05/refpt1, ymax=q0.95/refpt1), fill="grey25") +
    geom_ribbon(aes(ymin = q0.25/refpt1, ymax=q0.75/refpt1), fill="grey90") +
    geom_line(aes(y=q0.5/refpt1), size=1.1) +
    geom_hline(yintercept = 1.0, color = "blue") +
    facet_grid(retro_type~Fhist+n_selblocks) +
    labs(x="Year", y=paste0(myylab2, "1970")) +
    expand_limits(y=0) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  
  myrelplot50 <- ggplot(mytibble, aes(x=year)) +
    geom_ribbon(aes(ymin = q0.05/refpt50, ymax=q0.95/refpt50), fill="grey25") +
    geom_ribbon(aes(ymin = q0.25/refpt50, ymax=q0.75/refpt50), fill="grey90") +
    geom_line(aes(y=q0.5/refpt50), size=1.1) +
    geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") +
    facet_grid(retro_type~Fhist+n_selblocks) +
    labs(x="Year", y=paste0(myylab2, "2019")) +
    expand_limits(y=0) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
    theme_bw()
  
  myplots <- list(myplot = myplot,
                  myrelplot1 = myrelplot1,
                  myrelplot50 = myrelplot50)
  return(myplots)
}

ssb_plots <- make_base_plots(myssb_sum_wide, "SSB", "SSB/SSBmsy")

f_plots <- make_base_plots(myf_sum_wide, "F", "F/Fmsy")

catch_plots <- make_base_plots(mycatch_sum_wide, "Catch", "Catch/MSY")

pdf(file = "demonstrations/chris/base_sims/base_sims_plots.pdf")
descplot

ssb_plots$myplot
f_plots$myplot
catch_plots$myplot

ssb_plots$myrelplot1
f_plots$myrelplot1
catch_plots$myrelplot1

ssb_plots$myrelplot50
f_plots$myrelplot50
catch_plots$myrelplot50

dev.off()

# save pngs
ggsave(filename = "demonstrations/chris/base_sims/F.png", f_plots$myplot, width = 6.5, height = 6.5, units = "in")
ggsave(filename = "demonstrations/chris/base_sims/SSB.png", ssb_plots$myplot, width = 6.5, height = 6.5, units = "in")


ggsave(filename = "demonstrations/chris/base_sims/f_plots1.png", f_plots$myplot, width = 6.5, height = 6.5, units = "in")
ggsave(filename = "demonstrations/chris/base_sims/ssb_plots_rel50.png", ssb_plots$myrelplot50, width = 6.5, height = 6.5, units = "in")
ggsave(filename = "demonstrations/chris/base_sims/f_plots_rel50.png", f_plots$myrelplot50, width = 6.5, height = 6.5, units = "in")
ggsave(filename = "demonstrations/chris/base_sims/catch_plots_rel50.png", catch_plots$myrelplot50, width = 6.5, height = 6.5, units = "in")

# save csv of ref points
write.csv(refpts, file = "demonstrations/chris/base_sims/refpts.csv", row.names = FALSE)
