# make_appendix6.R
# uses officeR package to make Word doc of pngs with captions

library(tidyverse)
library(officer)

mydir <- "C:/Users/chris.legault/Desktop/myIBMWG/figs_for_report"
od <- mydir # in case want to save docx file somewhere else

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

### boxplots
mlab <- c("SSB", "F", "catch")
mlablower <- c("ssb", "f", "catch")
tlab <- c("IBM", "scenario")
tlabshort <- c("IBM", "Scen")
for (i in 1:3){ # SSB, F, catch
  for (j in 1:3){ # set, e.g., probs, ns, ratios 
    if (i != 3){
      slab <- c("probs", "ns", "ratios")
    }else{
      slab <- c("means", "ratios", "other")
    }
    for (k in 1:2){ # IBM, Scen
      mymain <- paste0("Boxplot of the mean values for the ", mlab[i], " metrics in the base analyses by ", tlab[k], ".")
      ifig <- ifig + 1
      myfile <- file.path(mydir, paste0(mlablower[i],"_box_",slab[j],"_",tlabshort[k],"_1.png"))
      mycaption <- paste0("Figure A6.", ifig, ". ", mymain) 
      my_doc <- my_doc %>%
        officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
        officer::body_add_par(mycaption, style = "Normal") %>%
        officer::body_add_par("", style = "Normal") %>% # blank line
        officer::body_add_break(pos = "after") # page break
    }
  }
}

### trade off plots
basetext1 <- "Trade off plot by IBM between "
tdtext <- c("the probability of a rebuilt stock and the mean catch relative to MSY", "the probability the stock is overfished and the probability that overfishing is occurring", "the average SSB and catch relative to their MSY reference points")
plab <- c(" in the long term.", " in the short term.")
basetext2 <- " Each point represents one scenario with the color indicating the source of the retrospective pattern for that scenario."
plabshort <- c("l", "s")
for (i in 1:3){ # number of td plots
  for (j in 1:2){ # long and short term
    ifig <- ifig + 1
    myfile <- file.path(mydir, paste0("td", i, "_", plabshort[j], "_plot_1.png"))
    mycaption <- paste0("Figure A6.", ifig, ". ", basetext1, tdtext[i], plab[j], basetext2)
    my_doc <- my_doc %>%
      officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
      officer::body_add_par(mycaption, style = "Normal") %>%
      officer::body_add_par("", style = "Normal") %>% # blank line
      officer::body_add_break(pos = "after") # page break
  }  
}


### 1,000 point plots for trade off 4
flab <- c("scenario", "IBM") # reversed because describing top left of fig
flabshort <- c("IBM", "Scen")
nplots <- c(16, 13) # number of plots by IBM and Scen
plab <- c("long", "short")
plabshort <- c("l", "s")
basetext1 <- "Spawning stock biomass (SSB) relative to the SSB at maximum sustainable yield (SSBmsy) and catch relative to MSY for the "
basetext2 <- " defined in the top left in the "
basetext3 <- " term with each dot representing one of the 1,000 simulations." 
for (i in 1:2){ # IBM and Scen
  for (j in 1:2){ # long and short term
    for (k in 1:nplots[i]){
      ifig <- ifig + 1
      myfile <- file.path(mydir, paste0("td4_", plabshort[j], "_", flabshort[i],"_plot_", k, "_list.png"))
      mycaption <- paste0("Figure A6.", ifig, ". ", basetext1, flab[i], basetext2, plab[j], basetext3)
      my_doc <- my_doc %>%
        officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
        officer::body_add_par(mycaption, style = "Normal") %>%
        officer::body_add_par("", style = "Normal") %>% # blank line
        officer::body_add_break(pos = "after") # page break
      
    }
  }
}




# make the docx file
print(my_doc, target = file.path(od, "Appendix6.docx"))
