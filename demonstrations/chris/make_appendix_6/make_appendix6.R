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





# make the docx file
print(my_doc, target = file.path(od, "Appendix6.docx"))
