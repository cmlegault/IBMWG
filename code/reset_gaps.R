# Script to change the values in progress table that got run, but where the simulations didn't work
# GF 2020/10/07
library(tidyverse)


# load in progress table
system("git pull")
progress <- readRDS(file = "settings/progress_table.rds")

#copy the progress table for my sanity's sake
today = format(Sys.time(),"%Y-%m-%d-%H-%M-%S") #for actual date
outfile = paste0("progress_table_",str_remove_all(today,"-"),".rds")
system(paste0("cp settings/progress_table.rds settings/",outfile))



#load the results (these are simulations that worked)
# currently reading from Chris' demo perform metrics file. 
# This won't have the full set of results that have been done yet SO don't use this to rewrite progress table.rds at this time
# (the line of code for that hasn't been written here purposefully to hedge against that)

# ideally I  think rather than reading in the performance metrics table one would read in the results fiels and check,
# but that's essentially what is happening here.
mse_perform <- readRDS(file = "demonstrations/chris/demo_plots/demo-perform-metrics.rds") %>% 
  slice(which(!duplicated(.$rowid))) %>% 
  I()
mse_perform


# look at duplicates
counts <- mse_perform  %>% group_by(iscen) %>% count()
ggplot(counts, aes(x=iscen, y=n)) + geom_point() + ylim(0,NA)


 # look at rows that there are results for
results_for <- progress %>% 
  slice(which(rowid %in% mse_perform$rowid == TRUE)) %>% 
  I()
x <- results_for %>% filter(is.na(uploaded))
table(x$user)

# so there are some runs that there are results for but 
# for which the progress table does not have an 'uploaded flag'

#reset the progress table with these
progress$uploaded[progress$rowid %in% results_for$rowid] <- TRUE


# now look at rruns that need to be reset (i.e. uploaded = TRUE but they didn't work)

# find the progress that have been done
done <- progress %>% 
  #filter(is.na(user)) %>%
  #filter(IBM != "ensemble") %>%   ## this removes the ensembles 
  #slice(1:nsim) %>% select(rowid) %>% t() %>% as.numeric() %>% 
  drop_na() %>% 
  I()

to_reset <- done %>% 
  slice(which(rowid %in% mse_perform$rowid == FALSE)) %>% 
  I()
#results_for %>% filter(is.na(uploaded))

#reset the progress table
progress$uploaded[progress$rowid %in% to_reset$rowid] <- NA
progress$user[progress$rowid %in% to_reset$rowid] <- NA
progress$date_run[progress$rowid %in% to_reset$rowid] <- NA

# add the line to save the new revised progress table here
# # write the file back to disk & commit to update
saveRDS(progress, file = "settings/progress_table.rds")
#commit back to the repo
system('git commit -am "resets incorrect values in progress table"')
system("git push")

