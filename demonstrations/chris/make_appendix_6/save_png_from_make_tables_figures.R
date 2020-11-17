# save_png_from _make_tables_figures.R

# source make_tables_figures.R first
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

my_png_list <- function(v1){
  v1str <- deparse(substitute(v1))
  nplots <- length(v1)
  for (j in 1:nplots){
    ggsave(file = file.path(mydir, paste0(v1str, "_", j, "_list", ".png")),
           v1[[j]], 
           width = 6.5, height = 6.5, units = "in")
  }
}

# single plots
my_png(nsim_plot, 0)

# base, noretro, scaa plots

for (i in 1:3){

  my_png(ssb_box_probs_IBM, i) 
  my_png(ssb_box_probs_Scen, i)
  my_png(ssb_box_ns_IBM, i)
  my_png(ssb_box_ns_Scen, i)
  my_png(ssb_box_ratios_IBM, i) 
  my_png(ssb_box_ratios_Scen, i)
  
  my_png(f_box_probs_IBM, i) 
  my_png(f_box_probs_Scen, i)
  my_png(f_box_ns_IBM, i)
  my_png(f_box_ns_Scen, i)
  my_png(f_box_ratios_IBM, i) 
  my_png(f_box_ratios_Scen, i)
  
  my_png(catch_box_means_IBM, i)
  my_png(catch_box_means_Scen, i)
  my_png(catch_box_ratios_IBM, i) 
  my_png(catch_box_ratios_Scen, i)
  my_png(catch_box_other_IBM, i) 
  my_png(catch_box_other_Scen, i)
  
  my_png(td1_l_plot, i)
  my_png(td1_s_plot, i)
  my_png(td2_l_plot, i)
  my_png(td2_s_plot, i)
  my_png(td3_l_plot, i)
  my_png(td3_s_plot, i)
  
  if (i == 1){
    my_png_list(td4_l_IBM_plot)
    my_png_list(td4_s_IBM_plot)
    my_png_list(td4_l_Scen_plot)
    my_png_list(td4_s_Scen_plot)
  }
  
  if (i == 2){
    my_png_list(td4_l_IBM_noretro_plot)
    my_png_list(td4_s_IBM_noretro_plot)
    my_png_list(td4_l_Scen_noretro_plot)
    my_png_list(td4_s_Scen_noretro_plot)
  }
  
  if (i == 3){
    my_png_list(td4_l_IBM_scaa_plot)
    my_png_list(td4_s_IBM_scaa_plot)
    my_png_list(td4_l_Scen_scaa_plot)
    my_png_list(td4_s_Scen_scaa_plot)
  }
  
  my_png(ssb_ssbmsy_l, i)
  my_png(f_fmsy_l, i) 
  my_png(catch_msy_l, i)
  my_png(ssb_ssbmsy_s, i)
  my_png(f_fmsy_s, i) 
  my_png(catch_msy_s, i) 
  
  my_png(prob_status_plot, i)
  my_png(nyrs_status_plot, i)
}

# redo confetti plots to save as png
my_png_confetti <- function(v1){
  v1str <- deparse(substitute(v1))
  v2 <- list()
  v2[[1]] <- v1
  v2[[2]] <- v1 + geom_point(aes(color = retro_type))
  v2[[3]] <- v1 + geom_point(aes(color = IBMlab))
  v2[[4]] <- v1 + geom_point(aes(color = factor(Fhist)))
  v2[[5]] <- v1 + geom_point(aes(color = factor(n_selblocks)))
  v2[[6]] <- v1 + geom_point(aes(color = factor(catch.mult)))
  nplots <- length(v2)
  for (j in 1:nplots){
    ggsave(file = file.path(mydir, paste0(v1str, "_", j, "_confetti", ".png")),
           v2[[j]], 
           width = 6.5, height = 6.5, units = "in")
  }
}

my_png_confetti(ssb_probs_plot)
my_png_confetti(ssb_ns_plot)
my_png_confetti(ssb_ratios_plot)

my_png_confetti(f_probs_plot)
my_png_confetti(f_ns_plot)
my_png_confetti(f_ratios_plot)

my_png_confetti(catch_means_plot)
my_png_confetti(catch_ratios_plot)
my_png_confetti(catch_other_plot)
