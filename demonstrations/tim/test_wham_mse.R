library(wham)
source("demonstrations/gavin/base_input.R")
source("~/work/wham/wham/R/update_wham_input.R")
source("~/work/wham/wham/R/prepare_wham_input.R")

Fhist = 1
Fhist = 2
Fmsy_scale = 2.5
n_selblocks = 1
n_selblocks = 2
om = get_base_input(n_selblocks, Fhist, Fmsy_scale)
x = om$par$logit_selpars[3,11:12]
om$par$logit_selpars[3,11] = log(3) - log(10-3)
10/(1 + exp(-x))
om_wham = fit_wham(om, do.fit = FALSE)
simdat = om_wham$simulate(complete = TRUE)
sim_input = om
sim_input$data = simdat
sim_fit = fit_wham(sim_input, do.fit = TRUE, do.retro = FALSE, do.osa = FALSE)
update_input = update_wham_input(simdat, om_wham, n_years_add=20)
update_om = fit_wham(update_input, do.fit = FALSE)
update_sim_fit = fit_wham(update_input, do.fit = TRUE, do.retro = FALSE, do.osa = FALSE)

cbind(mse.results1$true_om$env$par[names(mse.results1$true_om$env$par) == "log_NAA"], log(mse.results1$sim_data_series$NAA[-1,1]))
