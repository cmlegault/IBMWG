#devtools::install_github("timjmiller/wham", dependencies=TRUE)
library(wham)
library(tidyverse)
library(furrr)
library(purrr)
#library(googledrive)
library(dlm)
library(RandomFieldsUtils)
library(dplyr)

rscripts <- c("code/base_input.R",
              "code/change_input.R",
              "code/IBM_options.R",
              "code/wham_mse_functions.R",
              "code/wham_retro_functions.R",
              "demonstrations/tim/SCAA.R")
map(rscripts, source)
my_future_options = future_options()
my_future_options$globals = ls()
my_future_options$packages <- c("wham",
                                "tidyverse",
                                "dlm",
                                "RandomFieldsUtils")

future::plan(future::multisession)
# load in the scenario specifications
mse_sim_setup <- readRDS(file = "settings/mse_sim_setup.rds")
# load in the input objects
input_setup <- readRDS(file = "settings/input_setup.rds")

mse_sim_todo <- mse_sim_setup %>% filter(seed %in% unique(mse_sim_setup$seed)[1:2] & iscen %in% 1:2) %>%
#mse_sim_todo <- mse_sim_setup %>% filter(isim == 2) %>%
    mutate(IBM = rep("SCAA",nrow(.)),
         specs = map(specs, function(x) {x$IBM = "SCAA";return(x)}),#) %>% 
         Fhist = map_dbl(specs, "Fhist"),
         n_selblocks = map_dbl(specs, "n_selblocks"),
         retro_type = map_chr(specs, "retro_type"),
         catch_mult = map_dbl(specs, "catch.mult")) %>% 
  filter(catch_mult == 1 & Fhist == 2 & n_selblocks == 1) %>%
  left_join(input_setup) %>% 
  #modify the input object here
  mutate(input = pmap(list(input=input, change=specs), change_input)) %>% 
  I()

safe_wham_mse <- purrr::safely(do_wham_mse_sim, otherwise = NA_real_)
#do the MSE
start <- Sys.time()
mse_output <- mse_sim_todo %>% 
  group_by(IBM) %>% 
  ungroup() %>% 
   mutate(wham = furrr::future_pmap(list(seed = seed, input = input),
                                    safe_wham_mse, .options = my_future_options)) %>% 
  I()
stop <- Sys.time()
stop - start # how long runs took
saveRDS(mse_output, file = "demonstrations/tim/example_feedback_retros.rds")
#mse_output = readRDS("demonstrations/tim/example_feedback_retros.rds")

extract_rho = function(x, ind) sapply(x$result$advice, function(y) ifelse(length(y) == 4, y[[4]][ind], NA))

rho_res = mse_output %>%
  mutate(SSB_rho = pmap(list(ind = 1, x = wham), extract_rho)) %>%
  mutate(F_rho = pmap(list(ind = 2, x = wham), extract_rho)) %>%
  unnest(c(SSB_rho,F_rho)) %>%
  mutate(year = rep(2020+ 2*(0:20),4))

F_rho_plot <- 
  ggplot(rho_res, aes(x = year, y = F_rho)) +
  facet_grid(retro_type~seed) +
  geom_point() + 
  labs(x="Year", y="Mohn's rho(F)") +
  theme_bw()

SSB_rho_plot <- 
  ggplot(rho_res, aes(x = year, y = SSB_rho)) +
  facet_grid(retro_type~seed) +
  geom_point() + 
  labs(x="Year", y="Mohn's rho(SSB)") +
  theme_bw()

ggsave(F_rho_plot, filename = "demonstrations/tim/example_feedback_F_retro.png")
ggsave(SSB_rho_plot, filename = "demonstrations/tim/example_feedback_SSB_retro.png")


