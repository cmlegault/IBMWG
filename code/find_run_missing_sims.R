# find_run_missing_sims.R
# find and run the four missing simulations to examine what went wrong

library(tidyverse)

bad1 <- defined %>%
  filter(IBMlab == "AIM",
         retro_type == "Catch",
         Fhist == 1,
         n_selblocks == 1,
         catch.mult == 1)

bad1sims <- mse_results %>%
  filter(iscen == bad1$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad1$missing_isim <- setdiff(1:1000, bad1sims$isim)

bad2 <- defined %>%
  filter(IBMlab == "AIM",
         retro_type == "Catch",
         Fhist == 2,
         n_selblocks == 1,
         catch.mult == 1)

bad2sims <- mse_results %>%
  filter(iscen == bad2$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad2$missing_isim <- setdiff(1:1000, bad2sims$isim)

bad3 <- defined %>%
  filter(IBMlab == "ES-Fstable",
         retro_type == "Catch",
         Fhist == 2,
         n_selblocks == 1,
         catch.mult == 1)

bad3sims <- mse_results %>%
  filter(iscen == bad3$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad3$missing_isim <- setdiff(1:1000, bad3sims$isim)

bad4 <- defined %>%
  filter(IBMlab == "ES-Fstable",
         retro_type == "Catch",
         Fhist == 2,
         n_selblocks == 2,
         catch.mult == 1)

bad4sims <- mse_results %>%
  filter(iscen == bad4$iscen) %>%
  select(isim) %>%
  arrange(isim)

bad4$missing_isim <- setdiff(1:1000, bad4sims$isim)

fourbad <- data.frame(iscen = c(bad1$iscen, bad2$iscen, bad3$iscen, bad4$iscen),
                      isim = c(bad1$missing_isim, 
                               bad2$missing_isim, 
                               bad3$missing_isim, 
                               bad4$missing_isim))
fourbad

# load in the scenario specifications
mse_sim_setup <- readRDS(file = "settings/mse_sim_setup.rds")

# load in the input objects
input_setup <- readRDS(file = "settings/input_setup.rds")

# select just the four bad runs
to_do <- left_join(fourbad, mse_sim_setup, by = c("iscen", "isim")) %>% 
  select(rowid) %>% t() %>% as.numeric() %>% 
  I()

# pull out realizations to be run today from the setup list using "todo". 
mse_sim_todo <- mse_sim_setup %>% 
  filter(rowid %in% todo) %>% 
  left_join(input_setup) %>% 
  #modify the input object here
  mutate(input = pmap(list(input=input, change=specs), change_input)) %>% 
  I()

### run the MSE over each row of the mse_sims todo
#add a safe mode (returns error safely rather than crashing)
safe_wham_mse <- purrr::safely(do_wham_mse_sim, otherwise = NA_real_)
#do the MSE for all simulations and scenarios
#profvis::profvis(
#system.time(
mse_output <- mse_sim_todo %>% 
  mutate(wham = furrr::future_pmap(list(seed = seed, input = input),
                                   safe_wham_mse, .options = my_future_options)) %>% 
  # this is the regular purrr code for iterating over the simulations
  #mutate(wham = purrr::pmap(list(seed = seed, input = input), do_wham_mse_sim))
  select(-input) %>% 
  I()
#) #ends profvis
