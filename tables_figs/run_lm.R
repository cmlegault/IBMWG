#do linear models on results.
# demo_make_tables_figures.R
# uses demo_perform-metrics.rds results to create summary tables and figures

library(tidyverse)

# read in mean by scenario results
ssb_mean_by_scenario <- readRDS(file = "tables_figs/ssb_mean_by_scenario.rds")

f_mean_by_scenario <- readRDS(file = "tables_figs/f_mean_by_scenario.rds")

catch_mean_by_scenario <- readRDS(file = "tables_figs/catch_mean_by_scenario.rds")

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
  lm(value_trans~retro_type+Fhist+n_selblocks+IBMlab+catch.mult
     +retro_type:Fhist+retro_type:n_selblocks+retro_type:IBMlab+retro_type:catch.mult+
       Fhist:n_selblocks+Fhist:IBMlab+Fhist:catch.mult+
       n_selblocks:IBMlab+n_selblocks:catch.mult+IBMlab:catch.mult,data=thisdat) })
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

