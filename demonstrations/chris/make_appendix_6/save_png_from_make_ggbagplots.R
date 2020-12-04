# save_png_from_make_ggbagplots.R

# source make_ggbagplots.R first
# now save all the figures as png files in mydir

mydir <- "C:/Users/chris.legault/Desktop/myIBMWG/figs_for_report"

my_png_list <- function(v1){
  v1str <- deparse(substitute(v1))
  nplots <- length(v1)
  for (j in 1:nplots){
    ggsave(file = file.path(mydir, paste0(v1str, "_", j, "_list", ".png")),
           v1[[j]], 
           width = 6.5, height = 6.5, units = "in")
  }
}

my_png_list(bagplots_td4_base)
#my_png_list(bagplots_td4_noretro)
my_png_list(bagplots_td4_scaa)

