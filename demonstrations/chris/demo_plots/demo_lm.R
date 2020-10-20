#do linear models on results.
# demo_make_tables_figures.R
# uses demo_perform-metrics.rds results to create summary tables and figures

library(tidyverse)

# ### read in the performance metrics results
# mse_results <- readRDS("C:/Users/jonathan.deroba/Documents/GitHub/IBMWG-master/demonstrations/chris/demo_plots/demo-perform-metrics.rds")
# startdim <- dim(mse_results)
# 
# # remove any duplicate rows
# mse_results <- mse_results %>%
#   distinct()
# enddim <- dim(mse_results)
# startdim - enddim # if greater than zero, then there were duplicates
# names(mse_results)
# head(mse_results)
# 
# # find and remove duplicate scenarios
# mse_sim_setup <- readRDS("C:/Users/jonathan.deroba/Documents/GitHub/IBMWG-master/settings/mse_sim_setup.rds")
# dupes <- duplicated(mse_sim_setup[,-(1:2)])
# not_dupes <- mse_sim_setup$rowid[!dupes]
# mse_results <- mse_results %>%
#   filter(rowid %in% not_dupes)
# 
# # counts
# count_table <- mse_results %>%
#   group_by(iscen) %>%
#   summarise(n = n())
# count_table$n 
# 
# ### join with setup to figure out what's in each scenario
# Fhistlab <- c("O","F") # O = always overfishing, F = Fmsy in last year of base
# Sellab <- c("1", "2") # just whether 1 or 2 blocks for selectivity
# CMlab <- c("A", "R") # A = catch advice applied, R = reduced (mult=0.75)
# EMlab <- c("FSPR", "Fstable", "FM", "Frecent")
# CClab <- c("FSPR", NA, "FM")
# defined <- mse_sim_setup %>%
#   filter(rowid %in% not_dupes) %>% 
#   filter(isim == 1) %>%
#   select(iscen, specs) %>%
#   unnest(cols = specs) %>%
#   inner_join(count_table, by="iscen") %>%
#   mutate(IBMlab = case_when(
#     IBM == "Itarget" ~ "Itarg",
#     IBM == "true_Skate_CR" ~ "Skate",
#     IBM == "M_CC" ~ paste("CC", CClab[M_CC_method], sep="-"),
#     IBM == "planBsmoothfxn" ~ "PlanB",
#     IBM == "ExpandSurvey_modified" ~ paste("ES", EMlab[expand_method], sep="-"),
#     IBM == "run.aim" ~ "AIM",
#     IBM == "JoeDLM" ~ "DLM",
#     TRUE ~ IBM),
#     Scenlab = paste(substr(retro_type, 1, 1), Fhistlab[Fhist], Sellab[n_selblocks], ifelse(catch.mult == 1, CMlab[1], CMlab[2]), sep = ""))
# defined
# names(defined)
# unique(defined$IBMlab)
# unique(defined$Scenlab)
# 
# # counting scenarios and simulations
# countIBM <- defined %>%
#   group_by(IBMlab, Scenlab) %>%
#   summarise(nscenarios = n(), nsim = sum(n)) 
# 
# nscentab <- countIBM %>%
#   select(IBMlab, Scenlab, nscenarios) %>%
#   pivot_wider(names_from = Scenlab, values_from = nscenarios)
# nscentab
# 
# nsimtab <- countIBM %>%
#   select(IBMlab, Scenlab, nsim) %>%
#   pivot_wider(names_from = Scenlab, values_from = nsim)
# nsimtab
# 
# nsim_plot <- ggplot(countIBM, aes(x=Scenlab, y=nsim)) +
#   geom_bar(stat = "identity") +
#   facet_wrap(~IBMlab) +
#   labs(x="Scenario", y="Number of Simulations") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90))
# 
# 
# ###pull out the ssb metrics
# ssb_results <- mse_results %>% 
#   select(iscen, isim, ssb_metrics) %>% 
#   mutate(ssb_metrics = map(ssb_metrics, enframe)) %>% 
#   unnest(cols = c(ssb_metrics)) %>% 
#   mutate(value = map_dbl(value, I)) %>% 
#   rename(metric = name) %>% 
#   I()
# ssb_results
# unique(ssb_results$metric)
# 
# ###pull out the f metrics
# f_results <- mse_results %>% 
#   select(iscen, isim, f_metrics) %>% 
#   mutate(f_metrics = map(f_metrics, enframe)) %>% 
#   unnest(cols = c(f_metrics)) %>% 
#   mutate(value = map_dbl(value, I)) %>% 
#   rename(metric = name) %>% 
#   I()
# f_results
# unique(f_results$metric)
# 
# ###pull out the catch metrics
# catch_results <- mse_results %>% 
#   select(iscen, isim, catch_metrics) %>% 
#   mutate(catch_metrics = map(catch_metrics, enframe)) %>% 
#   unnest(cols = c(catch_metrics)) %>% 
#   mutate(value = map_dbl(value, I)) %>% 
#   rename(metric = name) %>% 
#   I()
# catch_results
# unique(catch_results$metric)
# 
# ### compute mean for all metrics for each scenario
# ssb_mean_by_scenario <- ssb_results %>%
#   group_by(iscen, metric) %>%
#   summarise_all(mean) %>%
#   inner_join(defined, by = "iscen")
# 
# f_mean_by_scenario <- f_results %>%
#   group_by(iscen, metric) %>%
#   summarise_all(mean) %>%
#   inner_join(defined, by = "iscen")
# 
# catch_mean_by_scenario <- catch_results %>%
#   group_by(iscen, metric) %>%
#   summarise_all(mean) %>%
#   inner_join(defined, by = "iscen")

# read in mean by scenario results
ssb_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/ssb_mean_by_scenario.rds")

f_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/f_mean_by_scenario.rds")

catch_mean_by_scenario <- readRDS(file = "demonstrations/chris/demo_plots/catch_mean_by_scenario.rds")

## log transform avg or number metrics. arcsine square root transform probs (Punt et al. 2008 Fisheries Research; Fay et al. 2011 Fisheries Research)
asinTrans<- function(p) { asin(sqrt(p)) }

lmfxn<-function(big_dat=NULL) {
metrics<-unique(big_dat$metric)
lm_res <- lapply(1:length(metrics), function(x) {
  thisdat<-big_dat[big_dat$metric==metrics[x],]
  if(grepl("_is_",metrics[x])){
    thisdat$value_trans<-asinTrans(thisdat$value)
  } else {
    thisdat$value_trans<-log(thisdat$value+0.00001)
  }
  lm(value_trans~retro_type+Fhist+n_selblocks+IBMlab+catch.mult,data=thisdat) })
names(lm_res)=metrics

do_anova<-lapply(lm_res,anova)
#ssb_AIC<-lapply(ssb_lm_res,AIC)

#row.names(ssb_anova[[1]])
table_res<-lapply(1:length(metrics), function(x)
  ifelse(do_anova[[x]]$`Pr(>F)`<0.05,"Sig","NS"))
table_res<-do.call(rbind,table_res)
colnames(table_res)<-row.names(do_anova[[1]])
table_res<-table_res[,!colnames(table_res) %in% c("Residuals")]
row.names(table_res)=metrics
perc_sig=lapply(1:ncol(table_res),function(x) 
  round(table(table_res[,x])["Sig"]/nrow(table_res),2)  )

table_res=rbind(table_res,do.call(cbind,perc_sig))
row.names(table_res)[nrow(table_res)]<-"Fraction_Sig"
return(table_res)
} #end lmfxn

SSB_lm=lmfxn(big_dat=ssb_mean_by_scenario)
F_lm=lmfxn((big_dat=f_mean_by_scenario))
catch_lm=lmfxn((big_dat=catch_mean_by_scenario))

lm_signigicance_table=rbind(SSB_lm,colnames(SSB_lm),F_lm,colnames(SSB_lm),catch_lm)
write.csv(lm_signigicance_table,file="demonstrations/chris/demo_plots/lm_signigicance_table.csv")

