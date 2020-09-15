#functions to make retro patterns

change_q_sim = function(sim = NULL, q_ratio = 1/3, n_ramp_years = 1, year_change = 2009, years = 1970:2019, change_proj = TRUE)
{
  #proportions at age shouldn't be affected
  if(is.null(sim)) stop("need sim argument with simulated data")
  ramp_ratio = seq(1, q_ratio, length.out = n_ramp_years)
  rampyears = year_change - (n_ramp_years-1):0 - min(years) + 1
  sim$agg_indices[rampyears,] = ramp_ratio * sim$agg_indices[rampyears,]
  sim$agg_indices[years>year_change,] = q_ratio * sim$agg_indices[years>year_change,]
  sim$agg_indices_proj = q_ratio * sim$agg_indices_proj
  keep_I = sim$keep_I[which(years>(year_change - n_ramp_years)),]
  sim$obsvec[keep_I+1] = log(sim$agg_indices[which(years>(year_change - n_ramp_years)),])
  return(sim)
}

change_catch_sim = function(sim = NULL, catch_ratio = 1/3, n_ramp_years = 10, year_change = 2009, years = 1970:2019)
{
  #proportions at age shouldn't be affected
  if(is.null(sim)) stop("need sim argument with simulated data")
  ramp_ratio = seq(1, catch_ratio, length.out = n_ramp_years)
  rampyears = year_change - (n_ramp_years-1):0 - min(years) + 1
  sim$agg_catch[rampyears,] = ramp_ratio * sim$agg_catch[rampyears,]
  sim$agg_catch[years>year_change,] = catch_ratio * sim$agg_catch[years>year_change,]
  sim$agg_catch_proj = catch_ratio * sim$agg_catch_proj
  keep_C = sim$keep_C[which(years>(year_change - n_ramp_years)),]
  sim$obsvec[keep_C+1] = log(sim$agg_catch[which(years>(year_change - n_ramp_years)),])
  return(sim)
}

change_M_om = function(wham_input = NULL, M_new_ratio = 3, n_ramp_years = 10, year_change = 2009)
{
  #proportions at age shouldn't be affected
  if(is.null(wham_input)) stop("need wham_input")
  #if(length(new_M) != wham_input$data$n_ages) stop("new Ms supplied do not match the number of ages")
  logM = logMout = t(wham_input$par$M0 + wham_input$par$M_a + t(wham_input$par$M_re))
  ramp_ratio = seq(1, M_new_ratio, length.out = n_ramp_years)
  rampyears = year_change - (n_ramp_years-1):0 - min(wham_input$years) + 1
  logMout[rampyears,] = logMout[rampyears,] + log(ramp_ratio)
  logMout[wham_input$years>year_change,] = logMout[wham_input$years>year_change,] + log(M_new_ratio) 
  Mre = logMout - logM
  #print(Mre)  
  wham_input$par$M_re = Mre
  return(wham_input)
}
