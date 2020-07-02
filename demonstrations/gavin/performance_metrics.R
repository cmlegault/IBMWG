# functions to calculate performance metrics


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
    s_n_ge_bmsy = length(which(ssb[shortyrs]>=SSBlim))

    l_is_less_01_bmsy = ifelse(any(ssb[longyrs]<0.1*SSBlim),1,0),
    l_is_less_05_bmsy = ifelse(any(ssb[longyrs]<0.5*SSBlim),1,0),
    l_is_ge_bmsy = ifelse(any(ssb[longyrs]>=SSBlim),1,0),
    l_n_less_01_bmsy = length(which(ssb[longyrs]<0.1*SSBlim)),
    l_n_less_05_bmsy = length(which(ssb[longyrs]<0.5*SSBlim)),
    l_n_ge_bmsy = length(which(ssb[longyrs]>=SSBlim))
  )
  return(metrics)
}
