# functions to calculate performance metrics

# as detailed in 
# https://docs.google.com/document/d/1ytEuChrB13n0wyKsp3MPhRdYEI20biQEcWWcBj0fWTs/edit

# SSB metrics

get_ssb_metrics <- function(ssb = NULL, refpts = NULL, nprojyrs = 40) {
  SSBlim <- refpts$SSBlim
  fyear <- length(ssb) - nprojyrs
  shortyrs <- fyear:(fyear+5)
  longyrs <- (fyear+20):length(ssb)
  projyrs <- fyear:length(ssb)
  metrics <- list(
    s_is_less_01_bmsy = ifelse(any(ssb[shortyrs]<0.1*SSBlim),1,0),
    s_is_less_05_bmsy = ifelse(any(ssb[shortyrs]<0.5*SSBlim),1,0),
    s_is_ge_bmsy = ifelse(any(ssb[shortyrs]>=SSBlim),1,0),
    s_n_less_01_bmsy = length(which(ssb[shortyrs]<0.1*SSBlim)),
    s_n_less_05_bmsy = length(which(ssb[shortyrs]<0.5*SSBlim)),
    s_n_ge_bmsy = length(which(ssb[shortyrs]>=SSBlim)),
    l_is_less_01_bmsy = ifelse(any(ssb[longyrs]<0.1*SSBlim),1,0),
    l_is_less_05_bmsy = ifelse(any(ssb[longyrs]<0.5*SSBlim),1,0),
    l_is_ge_bmsy = ifelse(any(ssb[longyrs]>=SSBlim),1,0),
    l_n_less_01_bmsy = length(which(ssb[longyrs]<0.1*SSBlim)),
    l_n_less_05_bmsy = length(which(ssb[longyrs]<0.5*SSBlim)),
    l_n_ge_bmsy = length(which(ssb[longyrs]>=SSBlim))
  )
  return(metrics)
}

# catch metrics
library(RcppRoll)
get_catch_metrics <- function(catch = NULL, refpts = NULL, nprojyrs = 40) {
  msy <- refpts$msy
  fyear <- max(c(1,length(catch) - nprojyrs))
  shortyrs <- fyear:(min(c(length(catch),fyear+5)))
  longyrs <- (min(c(fyear+20,length(catch)))):length(catch)
  projyrs <- fyear:length(catch)

  metrics <- list(
    s_avg_catch = mean(catch[shortyrs],na.rm=TRUE),
    l_avg_catch = mean(catch[longyrs],na.rm=TRUE),
    s_avg_catch_msy = s_avg_catch/msy,
    l_avg_catch_msy = l_avg_catch/msy,    
    s_sd_catch = sd(catch[shortyrs],na.rm=TRUE),
    l_sd_catch = sd(catch[longyrs],na.rm=TRUE),
    l_iav_catch = sqrt(sum(diff(catch[longyrs])^2)/(length(longyrs-1)))/(sum(catch[longyrs])/length(longyrs)),
    l_is_g_msy = ifelse(l_avg_catch_msy>1, 1, 0),
    rollsum_g_msy <- RcppRoll::roll_sum(l_is_g_msy, 3),
    l_prop_g_msy_2_of_3 <- sum(rollsum_g_msy>2)/length(rollsum_g_msy),
  )
  return(metrics)
} 


