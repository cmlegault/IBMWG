#functions to make retro patterns

change_q_sim = function(sim = NULL, new_q = c(0.4,0.8), year_change = 2000, years = 1970:2019)
{
  #proportions at age shouldn't be affected
  if(is.null(sim)) stop("need sim argument with simulated data")
  if(length(new_q) != length(sim$q)) stop("new qs supplied do not match the number of indices")
  sim$agg_indices[which(input$years<year_change),] = t(ratios * t(sim$agg_indices[which(input$years<year_change),]))
  keep_I = sim$keep_I
  keep_I[which(years<year_change),] = NA
  sim$obsvec[keep_I+1] = sim$agg_indices[which(years<year_change),]
  return(sim)
}

change_catch_sim = function(sim = NULL, catch_ratio = 1.5, year_change = 2000, years = 1970:2019)
{
  #proportions at age shouldn't be affected
  if(is.null(sim)) stop("need sim argument with simulated data")
  sim$agg_catch[which(input$years<year_change),] = catch_ratio * sim$agg_catch[which(input$years<year_change),]
  keep_C = sim$keep_C
  keep_C[which(years<year_change),] = NA
  sim$obsvec[keep_C+1] = sim$agg_catch[which(years<year_change),]
  return(sim)
}

change_M_om = function(wham_input = NULL, M_new_ratio = 3, n_ramp_years = 10, year_change = 2009)
{
  #proportions at age shouldn't be affected
  if(is.null(wham_input)) stop("need wham_input")
  #if(length(new_M) != wham_input$data$n_ages) stop("new Ms supplied do not match the number of ages")
  logM = logMout = t(wham_input$par$M0 + wham_input$par$M_a + t(wham_input$par$M_re))
  ramp_ratio = seq(1, M_new_ratio, length.out = n_ramp_years)
  rampyears = wham_input$years==year_change - (n_ramp_years-1):0
  logMout[rampyears,] = logMout[rampyears,] + log(ramp_ratio)
  logMout[wham_input$years>year_change,] = logMout[wham_input$years>year_change,] + log(M_new_ratio) 
  print(exp(logMout))
  Mre = logMout - logM
  #print(Mre)  
  wham_input$par$M_re = Mre
  return(wham_input)
}
