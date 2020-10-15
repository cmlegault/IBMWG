# compare_C_F_ratios.R
library(tidyverse)
M <- 0.2
Fmsy <- 0.3
Wmsy <- 100
cmsy <- Wmsy * Fmsy * (1 - exp(-M - Fmsy)) / (M + Fmsy)
Fy <- seq(0.1, 0.9, 0.1)
W <- seq(10, 200, 10)
df <- expand.grid(Fy = Fy, W = W) %>%
  mutate(cy = W * Fy * (1 - exp(-M - Fy)) / (M + Fy),
         cFmsy = W * Fmsy * (1 - exp(-M - Fmsy)) / (M + Fmsy),
         Fratio = Fy/Fmsy, 
         Cratio1 = cy/cmsy,
         Cratio2 = cy/cFmsy) %>% 
  pivot_longer(cols = c(5, 6, 7), names_to = "metric", values_to = "value")
df
myplot <- ggplot(df, aes(x=W, y=value, color = metric)) +
  geom_line() +
  facet_wrap(~Fy) +
  theme_bw()
ggsave(filename = "demonstrations/chris/compare_C_F_ratios/compare_C_F_ratios.png", myplot)
