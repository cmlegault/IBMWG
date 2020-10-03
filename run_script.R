
rscripts <- c("code/do_mse.R")
purrr::map(rscripts, source)
future::plan(future::multisession)

# run it
do_mse(nsim = 2000, user = "GF", write_to_google = FALSE)
