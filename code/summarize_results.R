# reads in results files from googledrive and compiles
# probably want an intermediate save to file after extracting performance metrics
# then for viz/analysis can just work with that tibble

## install needed libraries & scripts
library(wham)
library(tidyverse)
library(furrr)
library(googledrive)
#library(PlanBsmooth)
#library(RcppRoll)
rscripts <- list.files(path = "demonstrations/gavin/", pattern = "\\.R$")
rscripts <- str_subset(rscripts, "Run_MSE", negate = TRUE)
rscripts <- str_subset(rscripts, "do_mse", negate = TRUE)
rscripts <- str_subset(rscripts, "debug", negate = TRUE)
rscripts <- str_subset(rscripts, "setup_scenarios", negate = TRUE)
rscripts <- str_subset(rscripts, "summarize_results", negate = TRUE)
rscripts <- paste0("demonstrations/gavin/",rscripts)
map(rscripts, source)


# download the output & read them in
xx <- drive_ls("ibm-test/output")
map(xx$name,drive_download, overwrite = TRUE)
mse_output <- map_df(xx$name,readRDS)

nprojyrs <- 40  #this will be changed so it is part of the settings
# calculate performance metrics
mse_results <- mse_output %>% 
  mutate(om_ssb = map(wham,
                      ~pluck(.x$sim_data_series$SSB)),
         catch = map(wham, 
                     ~pluck(.x$sim_data_series$catch)),
         catch = map(catch, na_if, y = "NaN"),
         om_ssb = map(om_ssb, na_if, y = "NaN"),
         refpts = map(wham, "refpts"),
         ssb_metrics = pmap(list(om_ssb, refpts), get_ssb_metrics, nprojyrs = nprojyrs),
         catch_metrics = pmap(list(catch, refpts), get_catch_metrics, nprojyrs = nprojyrs)) %>% 
  select(rowid, base_scen, proj_scen, isim, ssb_metrics, catch_metrics) %>% 
  I()

#save the performance metrics object
saveRDS(mse_results, file = "settings/perform-metrics.rds")


#`mse_results` is a tibble containing the lists of SSB & catch performance metrics

#pull out the ssb metrics
ssb_results <- mse_results %>% 
  #select(rowid, ssb_metrics) %>% 
  select(base_scen, proj_scen, isim, ssb_metrics) %>% 
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
  group_by(metric, base_scen, proj_scen) %>% 
  summarise(y = list(quibble(value, c(0.25, 0.5, 0.75)))) %>% 
  tidyr::unnest(y) %>% 
  I()
ssb_summary
