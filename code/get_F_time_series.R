# get_F_time_series.R
# pull the Fbar value for the feedback period from each simulation

library(tidyverse)

myres2 <- tibble(iscen = integer(),
                 isim = integer(),
                 specs = list(),
                 Fbar = list())

#mydir <- "/net/home4/clegault/IBMWG/output"
#mydir <- "/net/home0/pdy/lbrooks/Git/output_oct14_IBMWG"
mydir <- "/net/home0/pdy/lbrooks/Git/IBMWG/output"
#mydir <- "C:/Users/chris.legault/Desktop/myIBMWGruns"
myfiles <- list.files(mydir) 
mymse <- myfiles[substr(myfiles, 1, 4) == "mse-"]
nmse <- length(mymse)

myids <- seq(1, 169)
nfiles <- length(myids)

for (i in myids){
  mydat <- readRDS(file.path(mydir, mymse[i]))
  myres <- mydat %>%
    mutate(finished = map(wham, "result"),
           finished = map(finished, "finished"),
           fin2 = Reduce(c, finished),
           frate = map(wham, ~pluck(.x$result$true_sim$Fbar)),
           frate = map(frate, na_if, y = "NaN"),
    ) %>%
    slice(which(!(fin2 < "2020-10-12" & iscen > 112))) %>%  #filter out the bogus catchmult 0.75 runs
    select(iscen, isim, specs, frate) %>% 
    I()
  
    myres2 <- rbind(myres2, myres)
    print(paste(i, dim(myres2)[1]))
}  
