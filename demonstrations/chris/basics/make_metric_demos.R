# make_metric_demos.R

library(tidyverse)

set.seed(14)

rnd <- rnorm(40, 0, 0.4)

phi <- 0.4

randvals <- rnd
for (i in 2:40){
  randvals[i] <- randvals[i-1] * phi + rnd[i]
}
randvals
evals <- exp(randvals)
emin <- min(evals)
emax <- max(evals)
mymin <- 0.1
mymax <- 0.6

myvals <- ((evals - emin) * (mymax - mymin) / (emax - emin)) + mymin
myvals

df <- data.frame(Year = 1:40,
                 Fval = myvals,
                 Fmsy = 0.31) %>%
  mutate(gt = ifelse(Fval > Fmsy, TRUE, FALSE),
         period = ifelse((Year > 6.5 & Year < 20.5), FALSE, TRUE)) %>%
  mutate(use = gt * period)
df

fplot <- ggplot(df, aes(x=Year, y=Fval)) +
  geom_line() +
  geom_line(data=df, aes(x=Year, y=Fmsy), color="blue", linetype=2) +
  geom_point(data=filter(df, use == 1), aes(x=Year, y=Fval), color = "blue") +
  geom_vline(xintercept = c(6.5, 20.5)) +
  expand_limits(y=0) +
  labs(y="F", title = "F > Fmsy") +
  theme_bw()
ggsave(filename = "demonstrations/chris/basics/fmetrics.png", fplot, width=6, height=5, units = "in")

