# reads in results files from googledrive and compiles
# probably want an intermediate save to file after extracting performance metrics
# then for viz/analysis can just work with that tibble

## install needed libraries & scripts
#library(wham)
library(tidyverse)
#library(furrr)
library(googledrive)

rscripts <- c("code/performance_metrics.R")
map(rscripts, source)

# 
# # download the output & read them in
# xx <- drive_ls("ibm-test/output")
# map(xx$name,drive_download, overwrite = TRUE)
# mse_output <- map_df(xx$name,readRDS)
# 
mse_output <- readRDS("dummy_output.rds")

#mse_output <- readRDS("output/mse-CML-20200930113704.rds")
#mse_output <- readRDS("~/Dropbox/mse_test_out.rds")

# calculate performance metrics
mse_results <- mse_output %>% 
  mutate(finished = map(wham, "result"),
         finished = map(finished, "finished"),
         size = map_dbl(wham, object.size),
         om_ssb = map(wham,
                      ~pluck(.x$result$true_sim$SSB)),
         catch = map(wham, 
                     ~pluck(.x$result$true_sim$pred_catch)), #true catch (without observation error)
         frate = map(wham, 
                     ~pluck(.x$result$true_sim$Fbar)), #Fbar is full F because it is averaged over just the plus group
         catch = map(catch, na_if, y = "NaN"),
         om_ssb = map(om_ssb, na_if, y = "NaN"),
         frate = map(frate, na_if, y = "NaN"),
         refpts = map(wham,
                     ~pluck(.x$result$refpts)),
         nprojyrs = map(specs, "nprojyrs"),
         ssb_metrics = pmap(list(om_ssb, refpts, nprojyrs), get_ssb_metrics),
         catch_metrics = pmap(list(catch, refpts, nprojyrs), get_catch_metrics),
         f_metrics = pmap(list(frate, refpts, nprojyrs), get_F_metrics)
         ) %>% 
#  select(rowid, iscen, isim, ssb_metrics, catch_metrics, f_metrics) %>% 
  I()

#save the performance metrics object
saveRDS(mse_results, file = "settings/perform-metrics.rds")


#`mse_results` is a tibble containing the lists of SSB & catch performance metrics

#pull out the ssb metrics
ssb_results <- mse_results %>% 
  #select(rowid, ssb_metrics) %>% 
  select(iscen, isim, ssb_metrics) %>% 
  mutate(ssb_metrics = map(ssb_metrics, enframe)) %>% 
  unnest(cols = c(ssb_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
ssb_results
#```

#summarize across simulations by scenario
#25%, 50%, 75% quantiles
quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
  tibble(x = quantile(x, q, na.rm = TRUE), q = q)
}

ssb_summary <- ssb_results %>% 
  group_by(metric, iscen) %>% 
  summarise(y = list(quibble(value, c(0.25, 0.5, 0.75)))) %>% 
  tidyr::unnest(y) %>% 
  I()
ssb_summary
