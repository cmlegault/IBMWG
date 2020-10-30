# reads in results files from googledrive and compiles
# probably want an intermediate save to file after extracting performance metrics
# then for viz/analysis can just work with that tibble

## install needed libraries & scripts
#library(wham)
library(tidyverse)
#library(furrr)
library(googledrive)

rscripts <- c("performance_metrics.R")
map(rscripts, source)

# will want to connect to Google Drive directly in future
# for now using a local directory that has the files from Google Drive
myfiles <- list.files()
myfiles <- str_subset(myfiles, "mse-")
myfiles

# subset for new runs
newfiles <- myfiles[c(146, 147, 149:151)]
nfiles <- length(newfiles)

# save database of which simulations run in each file
filedb <- data.frame(file = character(),
                     location = character(),
                     rowid = integer(),
                     iscen = integer(),
                     isim = integer())

for (i in 1:nfiles){
  thisrds <- readRDS(newfiles[i])
  mse_output <- thisrds %>%
    filter(map_lgl(wham, ~(.x %>% pluck("result", 1, 1) %>% is.na==FALSE)))
#mse_output <- map_dfr(newfiles, readRDS) %>% 
#  filter(map_lgl(wham, ~(.x %>% pluck("result", 1, 1) %>% is.na==FALSE)))

  # calculate performance metrics
  mse_results <- mse_output %>% 
    mutate(finished = map(wham, "result"),
           finished = map(finished, "finished"),
           fin2 = Reduce(c, finished), 
           size = map_dbl(wham, object.size),
           om_ssb = map(wham,
                        ~pluck(.x$result$true_sim$SSB)),
           catch = map(wham, 
                       ~pluck(.x$result$true_sim$pred_catch)), #true catch (without observation error)
           frate = map(wham, 
                       ~pluck(.x$result$true_sim$Fbar)), #Fbar is full F because it is averaged over just the plus group
           catch = map(catch, na_if, y = "NaN"),
           om_ssb = map(om_ssb, na_if, y = "NaN"),
           frate = map(frate, na_if, y = "NaN"),
           refpts = map(wham,
                        ~pluck(.x$result$refpts)),
           nprojyrs = map(specs, "nprojyrs"),
           ssb_metrics = pmap(list(om_ssb, refpts, nprojyrs), get_ssb_metrics),
           catch_metrics = pmap(list(catch, refpts, nprojyrs), get_catch_metrics),
           f_metrics = pmap(list(frate, refpts, nprojyrs), get_F_metrics)
    ) %>% 
    slice(which(!(fin2 < "2020-10-12" & iscen > 112))) %>%  #filter out the bogus catchmult 0.75 runs
    select(rowid, iscen, isim, ssb_metrics, catch_metrics, f_metrics) %>% 
    I()
  
  # save file info
  thisfile <- data.frame(file = newfiles[i],
                         location = getwd(),
                         rowid = mse_results$rowid,
                         iscen = mse_results$iscen,
                         isim = mse_results$isim)
  filedb <- rbind(filedb, thisfile)

  if (i == 1){
    myresults <- mse_results
  }else{
    myresults <- rbind(myresults, mse_results)
  }
  print(paste("file", newfiles[i], i, "of", nfiles, "had", dim(mse_results)[1], "successful simulations"))
}
dim(myresults)

# get results done previously
mse_results_start <- readRDS("demo-perform-metrics.rds")
filedb_start <- readRDS("filedb.rds")

# add new files to previous files
combinedresults <- rbind(mse_results_start, myresults)
combinedfiles <- rbind(filedb_start, filedb)

#save the performance metrics object
saveRDS(combinedresults, file = "demo-perform-metrics.rds")
saveRDS(combinedfiles, file = "filedb.rds")


