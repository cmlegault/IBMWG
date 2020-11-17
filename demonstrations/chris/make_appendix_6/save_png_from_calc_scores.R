# save_png_from _make_tables_figures.R
# save_png_from_calc_scores.R

# source calc_scores.R first
# now save all the figures as png files in mydir

mydir <- "C:\\Users\\chris.legault\\Desktop\\myIBMWG\\figs_for_report"

my_png <- function(v1, i){
  v1str <- deparse(substitute(v1))
  if (i %in% c(1, 2, 3)){
    v2 <- v1[[i]]
  }else{
    v2 <- v1
  }
  ggsave(file = file.path(mydir, paste0(v1str, "_", i, ".png")), v2, 
         width = 6.5, height = 6.5, units = "in")
}

for (i in 1:3){
  
  my_png(xmsy_l_plot, i)
  my_png(ssbmsy_l_plot, i)
  my_png(fmsy_l_plot, i)
  my_png(catchmsy_l_plot, i)
  
  my_png(xmsy_s_plot, i)
  my_png(ssbmsy_s_plot, i)
  my_png(fmsy_s_plot, i)
  my_png(catchmsy_s_plot, i)
  
  my_png(xmsy_b_plot, i)
  my_png(ssbmsy_b_plot, i)
  my_png(fmsy_b_plot, i)
  my_png(catchmsy_b_plot, i)
  
  my_png(c_only_plot, i)
  
}
