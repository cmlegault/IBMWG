# missing_sims.R

library(tidyverse)

mse_results <- readRDS("demonstrations/chris/demo_plots/demo-perform-metrics.rds")

done <- mse_results %>%
  select(rowid, iscen, isim)
donescen <- unique(done$iscen)
nscen <- length(donescen)

possible <-1:1000

todosims <- data.frame(rowid = integer(),
                       iscen = integer(),
                       isim = integer())
for (i in 1:nscen){
  thisscen <- donescen[i]
  donesim <- filter(done, iscen == thisscen) 
  missingsim <- data.frame(iscen = thisscen,
                           isim = possible[!(possible %in% donesim$isim)]) %>%
    mutate(rowid = (iscen - 1) * 1000 + isim)
  todosims <- rbind(todosims, missingsim)
}

ntodo <- todosims %>%
  group_by(iscen) %>%
  summarize(nmiss = n())
#view(ntodo)  

ggplot(todosims, aes(x=iscen, y=isim)) +
  geom_point() +
  theme_bw()

progtab <- readRDS("settings/progress_table.rds")

todo <- left_join(todosims, progtab, by = "rowid")

# three users and NA
todo %>%
  group_by(user) %>%
  summarize(n = n())

# NA user is for JoeDLM and M_CC, so OK
filter(todo, is.na(user)) %>%
  group_by(IBM) %>%
  summarise(n = n())

# almost all are not uploaded, are these missing results, bombed runs, both?
todo %>%
  group_by(uploaded) %>%
  summarize(n = n())

# the four individual dots on the plot, these look like they bombed during a run
filter(todo, uploaded == TRUE) 

# can the progress_table be modified to allow rowid in todo to be rerun?

