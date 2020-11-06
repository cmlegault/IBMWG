# reads in results files from user defined location
# an intermediate save to results directory after extracting performance metrics
# then for viz/analysis can just work with that tibble

# user note: be sure that mypath and newfiles are defined correctly

## install needed libraries & scripts
library(tidyverse)

rscripts <- c("code/performance_metrics.R")
map(rscripts, source)

# User defined path where output rds files are located
#mypath <- "/net/home4/clegault/IBMWG/output"
#mypath <- "/net/home0/pdy/lbrooks/Git/output_oct14_IBMWG"
#mypath <- "/net/home0/pdy/lbrooks/Git/IBMWG/output"
#mypath <- "C:/Users/chris.legault/Desktop/myIBMWGruns"
mypath <- "C:/Users/chris.legault/Desktop/myIBMWGruns/retrosourcenone"
myfiles <- list.files(mypath)
myfiles <- str_subset(myfiles, "mse-")
myfiles

# which files are read (will skip files already in database)
newfiles <- myfiles # can subset using brackets for testing, e.g., myfiles[1:3]
nfiles <- length(newfiles)

# defines names of files that are saved
mainfile_name <- "results/perform-metrics_noretro.rds"
dbfile_name <- "results/dbfile_noretro.rds"
dbrowid_name <- "results/dbrowid_noretro.rds"

# databases of file locations and which simulations run in each file
if(file.exists(dbfile_name)){
  dbfile_start <- readRDS(dbfile_name)
}else{
  dbfile_start <- tibble(link = character(),
                         file = character(),
                         location = character())
}

if(file.exists(dbrowid_name)){
  dbrowid_start <- readRDS(dbrowid_name)
}else{
  dbrowid_start <- tibble(link = character(),
                          rowid = integer(),
                          iscen = integer(),
                          isim = integer())
}

myresults <- NULL # so that first loop gets saved

for (i in 1:nfiles){
  if (!(newfiles[i] %in% dbfile_start$file)){ # skip if already read this file
    thisrds <- readRDS(file.path(mypath, newfiles[i]))
    if (dim(thisrds)[1] > 0){ # skip if nothing in the file
      mse_output <- thisrds %>%
        filter(map_lgl(wham, ~(.x %>% pluck("result", 1, 1) %>% is.na==FALSE)))
      
      # early runs did not have "finished" variable, so set artificial one
      # can comment out this if block for later runs to speed up
      # if (is.null(mse_output$wham[[1]]$result$finished)){
      #   for (jj in 1:dim(mse_output)[1]){
      #     mse_output$wham[[jj]]$result$finished = as.Date("2020-10-01")
      #   }
      # }
      
      if(mse_output$specs[[1]]$retro_type == "None"){ # make sure correct type
        
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
        today = format(Sys.time(),"%Y-%m-%d-%H-%M-%S") #for actual date
        mylink <- paste0(today, "-", i) # ensures unique id
        thisfile <- tibble(link = mylink,
                           file = newfiles[i],
                           location = mypath)
        thisrowid <- tibble(link = mylink,
                            rowid = mse_results$rowid,
                            iscen = mse_results$iscen,
                            isim = mse_results$isim)
        
        if (is.null(myresults)){
          myresults <- mse_results
          dbfile <- thisfile
          dbrowid <- thisrowid
        }else{
          myresults <- rbind(myresults, mse_results)
          dbfile <- rbind(dbfile, thisfile)
          dbrowid <- rbind(dbrowid, thisrowid)
        }
        # uncomment following line to watch progress in real time
        print(paste("file", newfiles[i], i, "of", nfiles, "had", dim(mse_results)[1], "successful simulations"))
      }
    }
  }
}

# get results done previously
if (file.exists(mainfile_name)){
  mse_results_start <- readRDS(mainfile_name)
  combined_results <- rbind(mse_results_start, myresults)
}else{
  combined_results <- myresults
}

# add new files to previous files
combined_dbfile <- rbind(dbfile_start, dbfile)
combined_dbrowid <- rbind(dbrowid_start, dbrowid)

#save the performance metrics object
saveRDS(combined_results, file = mainfile_name)
saveRDS(combined_dbfile, file = dbfile_name)
saveRDS(combined_dbrowid, file = dbrowid_name)


