# make_ggbagplots.R
# uses ggplot to make bagplots

# first source code/make_tables_figures.R
library(aplpack)

# gets bagplots for SSB/SSBmsy vs catch/MSY in short and long term
get_td4_bagdata <- function(mytib){
  myscenlabs <- sort(unique(mytib$Scenlab))
  nscenlabs <- length(myscenlabs)
  myibmlabs <- sort(unique(mytib$IBMlab))
  nibmlabs <- length(myibmlabs)
  mybag <- tibble(IBM = character(),
                  Scen = character(),
                  bagpart = character(),
                  period = character(),
                  x = double(),
                  y = double())
  
  for (i in 1:nscenlabs){
    for (j in 1:nibmlabs){
      tmpdf <- mytib %>%
        filter(Scenlab == myscenlabs[i], 
               IBMlab == myibmlabs[j])
      
      for (k in 1:2){
        if (k == 1){
          bag <- compute.bagplot(x=tmpdf$l_avg_ssb_ssbmsy, y=tmpdf$l_avg_catch_msy)
          myperiod <-  "Long"
          
        }else{
          bag <- compute.bagplot(x=tmpdf$s_avg_ssb_ssbmsy, y=tmpdf$s_avg_catch_msy)
          myperiod <- "Short"
        }
        
        this.bag <- tibble(IBM=myibmlabs[j],Scen=myscenlabs[i],bagpart="bag",period=myperiod,x=bag$hull.bag[,1], y=bag$hull.bag[,2])
        this.loop <- tibble(IBM=myibmlabs[j],Scen=myscenlabs[i],bagpart="loop",period=myperiod,x=bag$hull.loop[,1], y=bag$hull.loop[,2])
        this.outlier <- tibble(IBM=myibmlabs[j],Scen=myscenlabs[i],bagpart="outlier",period=myperiod,x=bag$pxy.outlier[,1], y=bag$pxy.outlier[,2])
        this.median <- tibble(IBM=myibmlabs[j],Scen=myscenlabs[i],bagpart="median",period=myperiod,x=median(bag$xy[,1]), y=median(bag$xy[,2]))
        
        mybag <- rbind(mybag, this.bag, this.loop, this.outlier, this.median)
        
      }
    }
  }
  return(mybag)
}

# compute bagplot data
bag_td4_base <- get_td4_bagdata(sims)  
# no retro bombed - don't know why
#bag_td4_noretro <- get_td4_bagdata(sims_noretro)
bag_td4_scaa <- get_td4_bagdata(sims_scaa)

# plotting function
get_bagplots <- function(myt){
  loop.col <- c("#66666633", '#0055AA33')
  bag.col <- c("#66666677", '#0055AA77')
  outlier.col <- c("#44444444", '#0055dd77')
  median.col <- c("black", "blue")
  myplot <- ggplot(myt, aes(x=x, y=y)) +
    geom_hline(aes(yintercept = 1), color = "red", linetype =  2) +
    geom_vline(aes(xintercept = 1), color = "red", linetype = 2) +
    geom_vline(aes(xintercept = 0.5), color= "red", linetype = 4) +
    geom_polygon(data = filter(myt, bagpart == "loop", period == "Long"), 
                 aes(x=x, y=y), fill = loop.col[1]) +
    geom_polygon(data = filter(myt, bagpart == "bag", period == "Long"), 
                 aes(x=x, y=y), fill = bag.col[1]) +
    geom_point(data = filter(myt, bagpart == "outlier", period == "Long"), 
               aes(x=x, y=y), color = outlier.col[1], shape = 1) +
    geom_point(data = filter(myt, bagpart == "median", period == "Long"),
               aes(x=x, y=y), color = median.col[1]) +
    geom_polygon(data = filter(myt, bagpart == "loop", period == "Short"), 
                 aes(x=x, y=y), fill = loop.col[2]) +
    geom_polygon(data = filter(myt, bagpart == "bag", period == "Short"), 
                 aes(x=x, y=y), fill = bag.col[2]) +
    geom_point(data = filter(myt, bagpart == "outlier", period == "Short"), 
               aes(x=x, y=y), color = outlier.col[2], shape = 1) +
    geom_point(data = filter(myt, bagpart == "median", period == "Short"),
               aes(x=x, y=y), color = median.col[2]) +
    labs(x="SSB/SSBmsy", y="Catch/MSY") +
    theme_minimal()
  return(myplot)  
}

# organizing function
make_bagplots <- function(mytib, mysmax, mycmax, mytitleext){
  myscen <- sort(unique(mytib$Scen))
  nscen <- length(myscen)
  myibm <- sort(unique(mytib$IBM))
  nibm <- length(myibm)

  res <- list()
  
  for (i in 1:nscen){
    myt <- filter(mytib, Scen == myscen[i])
    res[[i]] <- get_bagplots(myt) +
      facet_wrap(~IBM) + 
      expand_limits(x=mysmax, y=mycmax) +
      ggtitle(paste(myscen[i], mytitleext))
  }
  
  for (j in 1:nibm){
    myt <- filter(mytib, IBM == myibm[j])
    res[[nscen + j]] <- get_bagplots(myt) +
      facet_wrap(~Scen) +
      expand_limits(x=mysmax, y=mycmax) +
      ggtitle(paste(myibm[j], mytitleext))
  }
  
  return(res)
}

# get limits so scales are the same as flip through scenarios and IBMs
mysmax <- max(mysmax_l, mysmax_s)
mycmax <- max(mycmax_l, mycmax_s)
mysmax_noretro <- max(mysmax_l_noretro, mysmax_s_noretro)
mycmax_noretro <- max(mycmax_l_noretro, mycmax_s_noretro)
mysmax_scaa <- max(mysmax_l_scaa, mysmax_s_scaa)
mycmax_scaa <- max(mycmax_l_scaa, mycmax_s_scaa)

bagplots_td4_base <- make_bagplots(bag_td4_base, mysmax, mycmax, 
                                   "(Base scenarios)")
#bagplots_td4_noretro <- make_bagplots(bag_td4_noretro, mysmax_noretro, 
#                                      mycmax_noretro, "(No retro scenarios)") 
bagplots_td4_scaa <- make_bagplots(bag_td4_scaa, mysmax_scaa, mycmax_scaa, 
                                   "(SCAA scenarios)")

pdf(file = "demonstrations/chris/demo_plots/ggbagplots.pdf")

walk(bagplots_td4_base, print)
#walk(bagplots_td4, print)
walk(bagplots_td4_scaa, print)

dev.off()
