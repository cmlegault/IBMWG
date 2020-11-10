# check_google_drive.R

library(googledrive)
x <- drive_ls("~/IBMWG/ResultFiles")
x

# get run files from IBMWG/results dbfiles
resdir <- "C:/Users/chris.legault/Desktop/qqq/IBMWG/results"
base <- readRDS(file.path(resdir, "dbfile.rds"))
scaa <- readRDS(file.path(resdir, "dbfile_scaa.rds"))
noretro <- readRDS(file.path(resdir, "dbfile_noretro.rds"))

moved_base <- base$file %in% x$name
missing_base <- base[!moved_base, ]

moved_scaa <- scaa$file %in% x$name
missing_scaa <- scaa[!moved_scaa, ]

moved_noretro <- noretro$file %in% x$name
missing_noretro <- noretro[!moved_noretro, ]

needtomove <- rbind(missing_base, missing_scaa, missing_noretro)
write.csv(needtomove, file = "needtomove.csv", row.names = FALSE)
