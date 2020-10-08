
rscripts <- c("code/do_mse.R")
purrr::map(rscripts, source)
future::plan(future::multisession)

# run it inside a loop
nloop <- 2

for (iloop in 1:nloop) {
  
print(paste("Starting iteration ", iloop, ", for do_mse.R", sep=""))
do_mse(nsim = 2*224, user = "LB", write_to_google = FALSE)

}
  