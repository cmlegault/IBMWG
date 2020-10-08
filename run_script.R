args = commandArgs(trailingOnly=TRUE)

rscripts <- c("code/do_mse.R")
purrr::map(rscripts, source)
future::plan(future::multisession)

# run it
do_mse(nsim = args[1], user = args[2], write_to_google = FALSE)
