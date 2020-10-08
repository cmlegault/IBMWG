args = commandArgs(trailingOnly=TRUE)

rscripts <- c("code/do_mse.R")
purrr::map(rscripts, source)
future::plan(future::multisession)

<<<<<<< HEAD
# run it inside a loop
nloop <- 2

for (iloop in 1:nloop) {
  
print(paste("Starting iteration ", iloop, ", for do_mse.R", sep=""))
do_mse(nsim = 2*224, user = "LB", write_to_google = FALSE)

}
  
=======
# run it
do_mse(nsim = args[1], user = args[2], write_to_google = FALSE)
>>>>>>> 8779f9cdc26a0a2db346cb19193e8df6df480bd8
