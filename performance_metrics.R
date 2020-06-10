# functions to calculate performance metrics


# SSB metrics

get_ssb_metrics <- function(ssb = NULL, refpts = NULL, nprojyrs = 40) {
  SSBlim <- refpts$SSBlim
  fyear <- length(ssb) - nprojyrs
  shortyrs <- fyear:(fyear+5)
  longyrs <- (fyear+20):length(ssb)
  projyrs <- fyear:length(ssb)
  metrics <- list(
    is_less_01_bmsy = ifelse(any(ssb[projyrs]<0.1*SSBlim),1,0),
    is_less_05_bmsy = ifelse(any(ssb[projyrs]<0.5*SSBlim),1,0),
    is_ge_bmsy = ifelse(any(ssb[projyrs]>=SSBlim),1,0),
    n_less_01_bmsy = length(which(ssb[projyrs]<0.1*SSBlim)),
    n_less_05_bmsy = length(which(ssb[projyrs]<0.5*SSBlim)),
    n_ge_bmsy = length(which(ssb[projyrs]>=SSBlim))
  )
  return(metrics)
}
