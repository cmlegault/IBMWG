# get_retros.R

library(tidyverse)

dbfile_scaa <- readRDS("results/dbfile_scaa.rds")
dbfile_scaa
dbrowid_scaa <- readRDS("results/dbrowid_scaa.rds")
dbrowid_scaa

# need to convert "/net" to "//net.nefsc.noaa.gov"
mytemp <- "//net.nefsc.noaa.gov/home0/pdy/lbrooks/Git/IBMWG/output"

mydat <- readRDS(file.path(mytemp, dbfile_scaa$file[1]))
mydat

mydat$wham[[1]]$result$true_sim$catch_Neff

sim1 <- mydat$wham[[1]]
saveRDS(sim1, file = "demonstrations/chris/basics/sim1.rds")
