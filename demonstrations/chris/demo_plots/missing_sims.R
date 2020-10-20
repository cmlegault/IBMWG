# missing_sims.R

library(tidyverse)

mse_results <- readRDS("demonstrations/chris/demo_plots/demo-perform-metrics.rds")

done <- mse_results %>%
  select(iscen, isim)
donescen <- unique(done$iscen)
nscen <- length(donescen)

possible <-1:1000

todosims <- data.frame(iscen = integer(),
                       isim = integer())
for (i in 1:nscen){
  thisscen <- donescen[i]
  donesim <- filter(done, iscen == thisscen) 
  missingsim <- data.frame(iscen = thisscen,
                           isim = possible[!(possible %in% donesim$isim)])
  todosims <- rbind(todosims, missingsim)
}

ntodo <- todosims %>%
  group_by(iscen) %>%
  summarize(nmiss = n())
view(ntodo)  

ggplot(todosims, aes(x=iscen, y=isim)) +
  geom_point() +
  theme_bw()
