
rscripts <- c("code/do_mse.R")
purrr::map(rscripts, source)
future::plan(future::multisession)

# run it
do_mse(nsim = 100, user = "GF", write_to_google = FALSE)
