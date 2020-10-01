
rscripts <- c("code/do_mse.R")
purrr::map(rscripts, source)

# run it
do_mse(nsim = 10, user = "GF", write_to_google = FALSE)

