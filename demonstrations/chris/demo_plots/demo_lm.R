##transformations necessary?  log for some, maybe arcsine square root (Punt et al. 2008 Fisheries Research; Fay et al. 2011 Fisheries Research)

metrics<-unique(ssb_mean_by_scenario$metric)
ssb_lm_res <- lapply(1:length(metrics), function(x) {
  thisdat<-ssb_mean_by_scenario[ssb_mean_by_scenario$metric==metrics[x],]
  lm(value~retro_type+Fhist+n_selblocks,data=thisdat) })

ssb_anova<-lapply(ssb_lm_res,anova)
#ssb_AIC<-lapply(ssb_lm_res,AIC)
