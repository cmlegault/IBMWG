# make_appendix6.R
# uses officeR package to make Word doc of pngs with captions

library(tidyverse)
library(officer)

mydir <- "C:/Users/chris.legault/Desktop/myIBMWG/figs_for_report"
od <- "demonstrations/chris/make_appendix_6"

ifig <- 0 # counter for figures

# start the docx file
my_doc <- officer::read_docx()

# add figures, incrementing ifig appropriately
ifig <- ifig + 1
myfile <- file.path(mydir, "nsim_plot_0.png")
mycaption <- paste0("Figure A6.", ifig, ". Number of successfull simulations by scenario.")

my_doc <- my_doc %>%
  officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
  officer::body_add_par(mycaption, style = "Normal") %>%
  officer::body_add_par("", style = "Normal") %>% # blank line
  officer::body_add_break(pos = "after") # page break

## scores for base scenarios
score.txt <- "Two sets of scores, Rank and Resid, for the base analyses for the"
period <- c("in the long term.", "in the short term.", "in both the long and short term.")
metrics <- c("3 metrics of SSB, F, and Catch relative to their MSY reference points (denoted X/Xmsy)", "SSB relative to SSBmsy", "F relative to Fmsy", "catch relative to MSY")
mlab <- c("x", "ssb", "f", "catch")
plab <- c("l", "s", "b")
for (i in 1:3){
  for (j in 1:4){
    ifig <- ifig + 1
    myfile <- file.path(mydir, paste0(mlab[j],"msy_",plab[i],"_plot_1.png"))
    mycaption <- paste0("Figure A6.", ifig, ". ", score.txt, " ", metrics[j], " ", period[i])
    
    my_doc <- my_doc %>%
      officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
      officer::body_add_par(mycaption, style = "Normal") %>%
      officer::body_add_par("", style = "Normal") %>% # blank line
      officer::body_add_break(pos = "after") # page break
  }
}
ifig <- ifig + 1
myfile <- file.path(mydir, "c_only_plot_1.png")
mycaption <- paste0("Figure A6.", ifig, ". Two sets of scores, Rank and Resid, for the base analyses for the 2 metrics of interannual variability in catch over the entire feedback period and the short term mean Catch/MSY.")

my_doc <- my_doc %>%
  officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
  officer::body_add_par(mycaption, style = "Normal") %>%
  officer::body_add_par("", style = "Normal") %>% # blank line
  officer::body_add_break(pos = "after") # page break

## bagplots (using same caption for IBM and Scen plots)
bagIBMtext <- "Bagplots (a bivariate generalization of the boxplot) for long term (black) and short term (blue) SSB/SSBmsy and catch/MSY for each IBM in the scenario defined in the top left. The solid dot is the median, the dark shading is the 2D equivalent of the inner quartile range, the light shading encompasses an area three times the bag, and the unfilled dots are outliers."
bagScentext <- "Bagplots (a bivariate generalization of the boxplot) for long term (black) and short term (blue) SSB/SSBmsy and catch/MSY for each scenario using the IBM defined in the top left. The solid dot is the median, the dark shading is the 2D equivalent of the inner quartile range, the light shading encompasses an area three times the bag, and the unfilled dots are outliers."
for (i in 1:29){
  ifig <- ifig + 1
  myfile <- file.path(mydir, paste0("bagplots_td4_base_", i, "_list.png"))
  if (i <= 16){
    mymain <- bagIBMtext   
  }else{
    mymain <- bagScentext
  }
  mycaption <- paste0("Figure A6.", ifig, ". ", mymain)
  my_doc <- my_doc %>%
    officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
    officer::body_add_par(mycaption, style = "Normal") %>%
    officer::body_add_par("", style = "Normal") %>% # blank line
    officer::body_add_break(pos = "after") # page break
}







# make the docx file
print(my_doc, target = file.path(od, "Appendix6.docx"))
