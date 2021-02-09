# make_anova_heatmap.R
# code from Liz to conduct ANOVA and create heatmaps
# copied from demonstrations/chris/demo_plots/demo_make_tables_figures.R to make easier to find in future
# NOTE: something wrong with plot.aov.dist function

library(tidyverse)
library(aplpack)
library(pracma)
library(car)
library(gplots)

# get resuls and simulation set up information
mse_results <- readRDS("results/perform-metrics_clean.rds")
mse_sim_setup <- readRDS("settings/mse_sim_setup.rds")

# remove duplicate runs
dupes <- duplicated(mse_sim_setup[,-(1:2)])
not_dupes <- mse_sim_setup$rowid[!dupes]

mse_results <- mse_results %>%
  filter(rowid %in% not_dupes)

# check number of simlations per scenario
count_table <- mse_results %>%
  group_by(iscen) %>%
  summarise(n = length(unique(isim)))
count_table$n

### join with setup to figure out what's in each scenario
Fhistlab <- c("F","O") # O = always overfishing, F = Fmsy in last year of base
Sellab <- c("1", "2") # just whether 1 or 2 blocks for selectivity
CMlab <- c("A", "R") # A = catch advice applied, R = reduced (mult=0.75)
EMlab <- c("FSPR", "Fstable", "FM", "Frecent")
CClab <- c("FSPR", NA, "FM")
defined <- mse_sim_setup %>%
  filter(rowid %in% not_dupes) %>% 
  filter(isim == 1) %>%
  select(iscen, specs) %>%
  unnest(cols = specs) %>%
  inner_join(count_table, by="iscen") %>%
  mutate(IBMlab = case_when(
    IBM == "true_Skate_CR" ~ "Skate",
    IBM == "M_CC" ~ paste("CC", CClab[M_CC_method], sep="-"),
    IBM == "planBsmoothfxn" ~ "PBS",
    IBM == "ExpandSurvey_modified" ~ paste("ES", EMlab[expand_method], sep="-"),
    IBM == "run.aim" ~ "AIM",
    IBM == "JoeDLM" ~ "DLM",
    IBM == "ensemble" ~ "Ensemble",
    TRUE ~ IBM),
    Scenlab = paste(substr(retro_type, 1, 1), Fhistlab[Fhist], Sellab[n_selblocks], ifelse(catch.mult == 1, CMlab[1], CMlab[2]), sep = ""))
defined

###pull out the ssb metrics
ssb_results <- mse_results %>% 
  select(iscen, isim, ssb_metrics) %>% 
  mutate(ssb_metrics = map(ssb_metrics, enframe)) %>% 
  unnest(cols = c(ssb_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
ssb_results
unique(ssb_results$metric)

###pull out the f metrics
f_results <- mse_results %>% 
  select(iscen, isim, f_metrics) %>% 
  mutate(f_metrics = map(f_metrics, enframe)) %>% 
  unnest(cols = c(f_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
f_results
unique(f_results$metric)

###pull out the catch metrics
catch_results <- mse_results %>% 
  select(iscen, isim, catch_metrics) %>% 
  mutate(catch_metrics = map(catch_metrics, enframe)) %>% 
  unnest(cols = c(catch_metrics)) %>% 
  mutate(value = map_dbl(value, I)) %>% 
  rename(metric = name) %>% 
  I()
catch_results
unique(catch_results$metric)

# get additional information about each scenario
ssb_sims <- ssb_results %>%
  filter(grepl("avg_ssb_ssbmsy", metric)) %>%
  inner_join(defined, by = c("iscen"))

f_sims <- f_results %>%
  filter(grepl("avg_f_fmsy", metric)) %>%
  inner_join(defined, by = c("iscen"))

catch_sims <- catch_results %>%
  filter(grepl("avg_catch_msy", metric)) %>%
  inner_join(defined, by = c("iscen"))

# make time variable for use in ANOVA and heatmaps
ssb_sims.time <- ssb_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  mutate(time.avg = ifelse(substr(ssb_sims$metric,1,1)=="l", "L", "S"))

f_sims.time <- f_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  mutate(time.avg = ifelse(substr(f_sims$metric,1,1)=="l", "L", "S"))

catch_sims.time <- catch_sims %>% select(value, IBM, IBMlab, retro_type, Fhist, n_selblocks, catch.mult) %>%
  mutate(time.avg = ifelse(substr(catch_sims$metric,1,1)=="l", "L", "S"))

#-----------------------------
# function to make anova table
make.aov.table <- function(dataset, time.as.factor, out.table.name)  {
  
  method <- unique(dataset$IBMlab)  
  
  if (time.as.factor==T) test.factors <- "retro_type + time.avg + Fhist   + Fhist*catch.mult + Fhist*time.avg + retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+ catch.mult*time.avg + n_selblocks + catch.mult"
  if (time.as.factor==F) test.factors <- "retro_type + Fhist + n_selblocks + catch.mult +  retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult"
  
  n.factors <- length(strsplit(test.factors, split="+", fixed=T) [[1]])
  
  ss.table <- matrix(nrow=n.factors, ncol=length(method))
  sig.table <- matrix(nrow=n.factors, ncol=length(method))
  
  
  for (i in 1:length(method)) {
    
    
    tmp <- dataset[dataset$IBMlab==method[i],] 
    
    if (time.as.factor==T)    tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+  catch.mult*time.avg +   Fhist*catch.mult) , data = tmp)
    
    if (time.as.factor==F)    tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult +  retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
    
    tmp.sum <- summary(tmp.aov)[[1]]
    
    ss.tot <- sum(tmp.sum[,2])
    frac.ss <- tmp.sum[1:n.factors,2]/ss.tot
    ss.table[,i] <- frac.ss
    
    tmp.sig <- tmp.sum[1:n.factors,5]
    tmp.code <- rep(NA, n.factors)
    tmp.code[(which(tmp.sig<0.1))] <- 0.05
    tmp.code[(which(tmp.sig<0.05))] <- 0.01
    tmp.code[(which(tmp.sig<0.01))] <- 0.001
    tmp.code[(which(tmp.sig<0.001))] <- 0
    
    
    sig.table[,i] <- tmp.code
    
    
  }#end i loop over method
  
  rownames(ss.table) <- rownames(tmp.sum)[1:n.factors]
  colnames(ss.table) <- method
  s.table <- rbind(ss.table, "SS.frac.explained"=apply(ss.table, 2, sum) )
  s.table <- round(s.table,3)
  write.csv(s.table, file=paste0("tables_figs/anova_tables_figs/", out.table.name,".ANOVA.csv"))
  rownames(sig.table  ) <- rownames(tmp.sum)[1:n.factors]
  colnames(sig.table ) <- method
  write.csv(sig.table, file=paste0("tables_figs/anova_tables_figs/", "Signif.",out.table.name,".ANOVA.csv"))
  
}  # end function make.aov.table

#--------------------------------
# function to examine distribution of (transformed) data and check normality
plot.aov.dist <- function(dataset, time.as.factor, fig.label, out.label)  {
  
  method <- unique(dataset$IBMlab)  
  p.list <- list()
  pln.list <- list()
  psqrt.list <- list()
  pasinsqrt.list <- list()
  
  q.list <- list()
  qln.list <- list()
  qsqrt.list <- list()
  qasinsqrt.list <- list()
  
  pdf(file=paste0("tables_figs/anova_tables_figs/", out.label, ".pdf"), onefile=T)
  par(mfrow=c(2,2))
  
  for (i in 1:length(method)) {
    
    
    tmp <- dataset[dataset$IBMlab==method[i],] 
    p.list[[i]] <- hist(tmp$value, main=method[i], xlab=fig.label)
    pln.list[[i]] <- hist(log(tmp$value), main=method[i], xlab=paste0("LN_",fig.label))
    psqrt.list[[i]] <- hist(sqrt(tmp$value), main=method[i], xlab=paste0("SQRT_",fig.label))
    plot(x=1, y=1, type='n', bty='n', axes=F, xlab="", ylab="")
    #pasinsqrt.list[[i]] <- hist(asin(sqrt(tmp$value)), main=method[i], xlab=paste0("ARCSIN_SQRT",fig.label))
    
    # if time as factor ==F then run this code chunk
    if (time.as.factor==F)  {
      tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
      q.list[i] <- qqPlot(tmp.aov$residuals, id = FALSE, main="NO Transform" )
      
      tmp.aov <- aov(log(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
      qln.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE , main= "LN Transform")
      
      tmp.aov <- aov(sqrt(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
      qsqrt.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE, main="SQRT Transform" )
      
      plot(x=1, y=1, type='n', bty='n', axes=F, xlab="", ylab="")
      # tmp.aov <- aov(asin(sqrt(value)) ~ (retro_type + Fhist + n_selblocks + catch.mult + retro_type*Fhist + retro_type*catch.mult + Fhist*catch.mult) , data = tmp)
      # qasinsqrt.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE, main="ARCSIN-SQRT Transform" )
      
    } # end test for time.as.factor==F
    
    
    # if time as factor ==T then run this code chunk
    if (time.as.factor==T)  {
      tmp.aov <- aov(value ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult), data = tmp)
      q.list[i] <- qqPlot(tmp.aov$residuals, id = FALSE )
      
      tmp.aov <- aov(log(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult) , data = tmp)
      qln.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE )
      
      tmp.aov <- aov(sqrt(value) ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult) , data = tmp)
      qsqrt.list[i] <- qqPlot((tmp.aov$residuals), id = FALSE )
      
      plot(x=1, y=1, type='n', bty='n', axes=F, xlab="", ylab="")
      
      # tmp.aov <- aov((value) ~ (retro_type + Fhist + n_selblocks + catch.mult + time.avg + Fhist*time.avg+ retro_type*Fhist + retro_type*catch.mult +retro_type*time.avg+   Fhist*catch.mult) , data = tmp)
      # qasinsqrt.list[i] <- qqPlot(asin(sqrt(tmp.aov$residuals)), id = FALSE )
      
    } # end test for time.as.factor==F 
    
    
  }#end i loop over method
  
  #save PDF of distributions
  
  # pdf(file=paste0(out.label, ".pdf"), onefile=T)
  # par(mfrow=c(2,2))
  # plot(q.list)
  dev.off()
  
} # end plot.aov.dist
#--------------------

# create the plots and tables
# NOTE: something wrong with plot.aov.dist function
plot.aov.dist(dataset=ssb_sims, time.as.factor=F, fig.label="AVG_SSB_SSBMSY", out.label="Dist_AVG_SSB_SSBMSY")
plot.aov.dist(dataset=f_sims, time.as.factor=F, fig.label="AVG_F_FMSY", out.label="Dist_AVG_F_FMSY")
plot.aov.dist(dataset=catch_sims, time.as.factor=F, fig.label="AVG_CATCH_MSY", out.label="Dist_AVG_CATCH_MSY")

make.aov.table(dataset=ssb_sims.time, time.as.factor=T, out.table.name="SSB.frac.explained")
make.aov.table(dataset=f_sims.time, time.as.factor=T, out.table.name="F.frac.explained")
make.aov.table(dataset=catch_sims.time, time.as.factor=T, out.table.name="Catch.frac.explained")

#==== HEATMAP STUFF ====

heat.cols <- c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')  # 9 colors

# 1. by IBM
ssb_median_by_ibm <- ssb_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm <- f_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm <- catch_sims.time %>%
  group_by(IBMlab) %>%
  summarise(medianval=median(value))
all_median_by_ibm <- cbind(SSB=ssb_median_by_ibm$medianval, F=f_median_by_ibm$medianval, Catch=catch_median_by_ibm$medianval)
rownames(all_median_by_ibm) <- ssb_median_by_ibm$IBMlab


# 2. by IBM*retro type
ssb_median_by_ibm_retro <- ssb_sims.time %>%
  group_by(IBMlab, retro_type) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_retro <- f_sims.time %>%
  group_by(IBMlab, retro_type) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_retro <- catch_sims.time %>%
  group_by(IBMlab, retro_type) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_retro <- cbind(SSB=ssb_median_by_ibm_retro$medianval, F=f_median_by_ibm_retro$medianval, Catch=catch_median_by_ibm_retro$medianval)
rownames(all_median_by_ibm_retro) <- paste(ssb_median_by_ibm_retro$IBMlab, ssb_median_by_ibm_retro$retro_type, sep= "-")


# 3. by IBM* time
ssb_median_by_ibm_time <- ssb_sims.time %>%
  group_by(IBMlab, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_time <- f_sims.time %>%
  group_by(IBMlab, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_time <- catch_sims.time %>%
  group_by(IBMlab, time.avg) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_time <- cbind(SSB=ssb_median_by_ibm_time$medianval, F=f_median_by_ibm_time$medianval, Catch=catch_median_by_ibm_time$medianval)
rownames(all_median_by_ibm_time) <- paste(ssb_median_by_ibm_time$IBMlab, ssb_median_by_ibm_time$time.avg, sep= "-")


# 4. by IBM* catch multiplier
ssb_median_by_ibm_catch.mult <- ssb_sims.time %>%
  group_by(IBMlab, catch.mult) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_catch.mult <- f_sims.time %>%
  group_by(IBMlab, catch.mult) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_catch.mult <- catch_sims.time %>%
  group_by(IBMlab, catch.mult) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_cmult <- cbind(SSB=ssb_median_by_ibm_catch.mult$medianval, F=f_median_by_ibm_catch.mult$medianval, Catch=catch_median_by_ibm_catch.mult$medianval)
rownames(all_median_by_ibm_cmult) <- paste(ssb_median_by_ibm_catch.mult$IBMlab, ssb_median_by_ibm_catch.mult$catch.mult, sep= "-")

# 5. by IBM* Fhistory
ssb_median_by_ibm_Fhist <- ssb_sims.time %>%
  group_by(IBMlab, Fhist) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_Fhist <- f_sims.time %>%
  group_by(IBMlab, Fhist) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_Fhist <- catch_sims.time %>%
  group_by(IBMlab, Fhist) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_Fhist <- cbind(SSB=ssb_median_by_ibm_Fhist$medianval, F=f_median_by_ibm_Fhist$medianval, Catch=catch_median_by_ibm_Fhist$medianval)
rownames(all_median_by_ibm_Fhist) <- paste(ssb_median_by_ibm_Fhist$IBMlab, ssb_median_by_ibm_Fhist$Fhist, sep= "-")

# 6. by IBM* Cmult * time
ssb_median_by_ibm_cmult_time <- ssb_sims.time %>%
  group_by(IBMlab, catch.mult, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_cmult_time <- f_sims.time %>%
  group_by(IBMlab, catch.mult, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_cmult_time <- catch_sims.time %>%
  group_by(IBMlab, catch.mult, time.avg) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_Cmult_time <- cbind(SSB=ssb_median_by_ibm_cmult_time$medianval, F=f_median_by_ibm_cmult_time$medianval, Catch=catch_median_by_ibm_cmult_time$medianval)
rownames(all_median_by_ibm_Cmult_time) <- paste(ssb_median_by_ibm_cmult_time$IBMlab, ssb_median_by_ibm_cmult_time$time.avg, ssb_median_by_ibm_cmult_time$catch.mult, sep= "-")

# 7. by IBM* Fhist * time
ssb_median_by_ibm_Fhist_time <- ssb_sims.time %>%
  group_by(IBMlab, Fhist, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_Fhist_time <- f_sims.time %>%
  group_by(IBMlab, Fhist, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_Fhist_time <- catch_sims.time %>%
  group_by(IBMlab, Fhist, time.avg) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_Fhist_time <- cbind(SSB=ssb_median_by_ibm_Fhist_time$medianval, F=f_median_by_ibm_Fhist_time$medianval, Catch=catch_median_by_ibm_Fhist_time$medianval)
rownames(all_median_by_ibm_Fhist_time) <- paste(ssb_median_by_ibm_Fhist_time$IBMlab, ssb_median_by_ibm_Fhist_time$Fhist, ssb_median_by_ibm_Fhist_time$time.avg, sep= "-")

# 8. by IBM* retro * Fhist
ssb_median_by_ibm_retro_Fhist <- ssb_sims.time %>%
  group_by(IBMlab, retro_type, Fhist) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_retro_Fhist <- f_sims.time %>%
  group_by(IBMlab, retro_type, Fhist) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_retro_Fhist <- catch_sims.time %>%
  group_by(IBMlab, retro_type, Fhist) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_retro_Fhist <- cbind(SSB=ssb_median_by_ibm_retro_Fhist$medianval, F=f_median_by_ibm_retro_Fhist$medianval, Catch=catch_median_by_ibm_retro_Fhist$medianval)
rownames(all_median_by_ibm_retro_Fhist) <- paste(ssb_median_by_ibm_retro_Fhist$IBMlab, ssb_median_by_ibm_retro_Fhist$retro_type, ssb_median_by_ibm_retro_Fhist$Fhist, sep= "-")

# 9. by IBM* retro * time
ssb_median_by_ibm_retro_time <- ssb_sims.time %>%
  group_by(IBMlab, retro_type, time.avg) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_retro_time <- f_sims.time %>%
  group_by(IBMlab, retro_type, time.avg) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_retro_time <- catch_sims.time %>%
  group_by(IBMlab, retro_type, time.avg) %>%
  summarise(medianval=median(value))
all_median_by_ibm_retro_time <- cbind(SSB=ssb_median_by_ibm_retro_time$medianval, F=f_median_by_ibm_retro_time$medianval, Catch=catch_median_by_ibm_retro_time$medianval)
rownames(all_median_by_ibm_retro_time) <- paste(ssb_median_by_ibm_retro_time$IBMlab, ssb_median_by_ibm_retro_time$retro_type, ssb_median_by_ibm_retro_time$time.avg, sep= "-")

# 10. Fhist: catch.mult 
ssb_median_by_ibm_Fhist_Cmult <- ssb_sims.time %>%
  group_by(IBMlab, Fhist, catch.mult) %>%
  summarise(medianval=median(value)) 
f_median_by_ibm_Fhist_Cmult <- f_sims.time %>%
  group_by(IBMlab, Fhist, catch.mult) %>%
  summarise(medianval=median(value)) 
catch_median_by_ibm_Fhist_Cmult <- catch_sims.time %>%
  group_by(IBMlab, Fhist, catch.mult) %>%
  summarise(medianval=median(value)) 
all_median_by_ibm_Fhist_Cmult <- cbind(SSB=ssb_median_by_ibm_Fhist_Cmult$medianval, F=f_median_by_ibm_Fhist_Cmult$medianval, Catch=catch_median_by_ibm_Fhist_Cmult$medianval)
rownames(all_median_by_ibm_Fhist_Cmult) <- paste(ssb_median_by_ibm_Fhist_Cmult$IBMlab, ssb_median_by_ibm_Fhist_Cmult$Fhist, ssb_median_by_ibm_Fhist_Cmult$catch.mult, sep= "-")


# --- make pdf files ====
# uncomment this section to check to see heatmap function is working
# pdf(file = "tables_figs/heatmap_tables_figs/heatmaps_median_ohhh_fancier.pdf")
# #  by IBM
# heat2.all_med <-heatmap.2(all_median_by_ibm, scale="column", margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM")
# 
# # 1 factor by IBM
# heat2.all.retro_med <- heatmap.2(all_median_by_ibm_retro, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Retro Source")
# heat2.all.time_med <- heatmap.2(all_median_by_ibm_time, scale="column",  margins=c(7,10), keysize = 1, col=heat.cols, main = "IBM by Time Horizon")
# heat2.all.cmult_med <- heatmap.2(all_median_by_ibm_cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by Catch Multiplier")
# heat2.all.Fhist_med <- heatmap.2(all_median_by_ibm_Fhist, scale="column", margins=c(7,10), keysize = 1, col=heat.cols , main = "IBM by F history")
# 
# # 2 factors by IBM
# heat2.all.time.cmult.time_med <- heatmap.2(all_median_by_ibm_Cmult_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Catch Mult and Time Horizon")
# heat2.all.fhist.time_med <- heatmap.2(all_median_by_ibm_Fhist_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by F History and Time Horizon")
# heat2.all.retro.fhist_med <- heatmap.2(all_median_by_ibm_retro_Fhist, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Type and F History")
# heat2.all.retro.time_med <- heatmap.2(all_median_by_ibm_retro_time, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by Retro Type and Time Horizon")
# heat2.all.Fhist.cmult_med <- heatmap.2(all_median_by_ibm_Fhist_Cmult, scale="column", margins=c(7,10), keysize = 1, col=heat.cols  , main = "IBM by F History and Catch Multiplier")
# 
# dev.off()
# 

# --- make png files ====

# function to make the png
make_heatmap_png <- function(myfile,
                             mydata,
                             mymain,
                             mydir="tables_figs/heatmap_tables_figs/",
                             png.h = 10,
                             png.w = 8,
                             myunits = "in",
                             png.res = 500,
                             mytype = "cairo",
                             mycols = heat.cols){
  
  png(file=paste0(mydir,myfile), height=png.h, width=png.w, units=myunits,
      res=png.res, type=mytype)
  
  heatmap.2(mydata, scale="column", margins=c(7,10), keysize = 1, col=mycols, main = mymain)

  dev.off()
}

# 1
make_heatmap_png(myfile = "heatmap.ibm_median.png",
                 mydata = all_median_by_ibm,
                 mymain = "IBM")
# 2
make_heatmap_png(myfile = "heatmap.ibm.retro_median.png",
                 mydata = all_median_by_ibm_retro,
                 mymain = "IBM by Retro Source")
# 3
make_heatmap_png(myfile = "heatmap.ibm.time_median.png",
                 mydata = all_median_by_ibm_time,
                 mymain = "IBM by Time Horizon")
# 4
make_heatmap_png(myfile = "heatmap.ibm.cmult_median.png",
                 mydata = all_median_by_ibm_cmult,
                 mymain = "IBM by Catch Multiplier")
# 5
make_heatmap_png(myfile = "heatmap.ibm.Fhist_median.png",
                 mydata = all_median_by_ibm_Fhist,
                 mymain = "IBM by F history")
# 6
make_heatmap_png(myfile = "heatmap.ibm.cmult.time_median.png",
                 mydata = all_median_by_ibm_Cmult_time,
                 mymain = "IBM by Catch Mult and Time Horizon")
# 7
make_heatmap_png(myfile = "heatmap.ibm.fhist.time_median.png",
                 mydata = all_median_by_ibm_Fhist_time,
                 mymain = "IBM by F history and Time Horizon")
# 8
make_heatmap_png(myfile = "heatmap.ibm.retro.fhist_median.png",
                 mydata = all_median_by_ibm_retro_Fhist,
                 mymain = "IBM by Retro Type and F history")
# 9
make_heatmap_png(myfile = "heatmap.ibm.retro.time_median.png",
                 mydata = all_median_by_ibm_retro_time,
                 mymain = "IBM by Retro Type and Time Horizon")
# 10
make_heatmap_png(myfile = "heatmap.ibm.fhist.cmult_median.png",
                 mydata = all_median_by_ibm_Fhist_Cmult,
                 mymain = "IBM by F history and Catch Mult")



# --- write csv files for table of MEAN heatmap results ====

#1 
nrows <- dim(t(all_median_by_ibm))[2]
#write.csv(t(heat2.all_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm_median_normalized.csv")  #this gives table of standardized values
write.csv(all_median_by_ibm[rev(heat2.all_med$rowInd),heat2.all_med$colInd], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm_median.csv") #this gives table of values


# 2
nrows <- dim(t(all_median_by_ibm_retro))[2]
#write.csv(t(heat2.all.retro_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.retro_median_normalized.csv")
write.csv(all_median_by_ibm_retro[rev(heat2.all.retro_med$rowInd),heat2.all.retro_med$colInd], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.retro_median.csv")

# 3
nrows <- dim(t(all_median_by_ibm_time))[2]
#write.csv(t(heat2.all.time_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.time_median_normalized.csv")
write.csv(all_median_by_ibm_time[rev(heat2.all.time_med$rowInd),heat2.all.time_med$colInd], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.time_median.csv")

# 4
nrows <- dim(t(all_median_by_ibm_cmult))[2]
#write.csv(t(heat2.all.cmult_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.cmult_median_normalized.csv")
write.csv( all_median_by_ibm_cmult[rev(heat2.all.cmult_med$rowInd),heat2.all.cmult_med$colInd], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.cmult_median.csv")

# 5
nrows <- dim(t(all_median_by_ibm_Fhist))[2]
#write.csv(t(heat2.all.Fhist_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.Fhist_median_normalized.csv")
write.csv(all_median_by_ibm_Fhist[rev(heat2.all.Fhist_med$rowInd),heat2.all.Fhist_med$colInd] , file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.Fhist_median.csv")

# 6
nrows <- dim(t(all_median_by_ibm_Cmult_time))[2]
#write.csv(t(heat2.all.time.cmult.time_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.cmul.time_median_normalized.csv")
write.csv(all_median_by_ibm_Cmult_time[rev(heat2.all.time.cmult.time_med$rowInd),heat2.all.time.cmult.time_med$colInd] , file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.cmul.time_median.csv")

# 7
nrows <- dim(t(all_median_by_ibm_Fhist_time))[2]
#write.csv(t(heat2.all.fhist.time_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.fhist.time_median_normalized.csv")
write.csv( all_median_by_ibm_Fhist_time[rev(heat2.all.fhist.time_med$rowInd),heat2.all.fhist.time_med$colInd], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.fhist.time_median.csv")

# 8
nrows <- dim(t(all_median_by_ibm_retro_Fhist))[2]
#write.csv(t(heat2.all.retro.fhist_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.retro.fhist_median_normalized.csv")
write.csv(all_median_by_ibm_retro_Fhist[rev(heat2.all.retro.fhist_med$rowInd),heat2.all.retro.fhist_med$colInd] , file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.retro.fhist_median.csv")

# 9
nrows <- dim(t(all_median_by_ibm_retro_time))[2]
#write.csv(t(heat2.all.retro.time_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.retro.time_median_normalized.csv")
write.csv(all_median_by_ibm_retro_time[rev(heat2.all.retro.time_med$rowInd),heat2.all.retro.time_med$colInd] , file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.retro.time_median.csv")

# 10
nrows <- dim(t(all_median_by_ibm_Fhist_Cmult))[2]
#write.csv(t(heat2.all.Fhist.cmult_med$carpet)[rev(seq(1,nrows)),], file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.fhist.cmult_median_normalized.csv")
write.csv(all_median_by_ibm_Fhist_Cmult[rev(heat2.all.Fhist.cmult_med$rowInd),heat2.all.Fhist.cmult_med$colInd]  , file="tables_figs/heatmap_tables_figs/table.heatmap.ibm.fhist.cmult_median.csv")



##--- End of Heatmap Stuff (Medians) ====
