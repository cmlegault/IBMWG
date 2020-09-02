source("demonstrations/tim/estimate_retro_Catch_rho.R")
source("demonstrations/tim/estimate_retro_M_rho.R")
scen11 = estimate_retro_Catch_rho(C_ratio = 1) #SSB 0.01, F 0.01, R 0.06
scen12 = estimate_retro_Catch_rho(C_ratio = 1, Fhist = 2) #SSB 0.01, F 0.01, R 0.06
scen21 = estimate_retro_Catch_rho(C_ratio = 1, n_selblocks = 2) #SSB 0.00, F 0.01, R 0.08
scen22 = estimate_retro_Catch_rho(C_ratio = 1, n_selblocks = 2, Fhist = 2) #SSB 0.00, F 0.02, R 0.05


Catch_scen11 = estimate_retro_Catch_rho(C_ratio = 5) #SSB 0.4, F -0.28, R 0.32
Catch_scen12 = estimate_retro_Catch_rho(C_ratio = 2, Fhist = 2) #SSB 0.54, F -0.38, R 0.52
Catch_scen21 = estimate_retro_Catch_rho(C_ratio = 5, n_selblocks = 2) #SSB 0.45, F -0.31, R 0.30
Catch_scen22 = estimate_retro_Catch_rho(C_ratio = 2.25, n_selblocks = 2, Fhist = 2) #SSB 0.52, F -0.41, R 0.45


M_scen11 = estimate_retro_M_rho(M_ratio = 1.6) #SSB 0.54, F -0.36, R 0.70
M_scen12 = estimate_retro_M_rho(M_ratio = 1.8, Fhist = 2) #SSB 0.48, F -0.37, R 0.75
M_scen21 = estimate_retro_M_rho(M_ratio = 1.6, n_selblocks = 2) #SSB 0.51, F -0.36, R 0.65
M_scen22 = estimate_retro_M_rho(M_ratio = 1.8, n_selblocks = 2, Fhist = 2) #SSB 0.49, F -0.38, R 0.79

save(scen11,scen12,scen21,scen22,Catch_scen11,Catch_scen12,Catch_scen21,Catch_scen22,M_scen11,M_scen12,M_scen21,M_scen22, file = "demonstrations/tim/retro_res.RData")

dat = cbind.data.frame(retro_type = rep("None", 200), Fhist = rep(c(1,2,1,2), each = 50), selhist = rep(1:2, each = 100), 
  rbind(scen11, scen12, scen21, scen22))

dat = cbind.data.frame(retro_type = rep("Catch", 200), Fhist = rep(c(1,2,1,2), each = 50), selhist = rep(1:2, each = 100), 
  rbind(Catch_scen11, Catch_scen12, Catch_scen21, Catch_scen22))

dat = cbind.data.frame(retro_type = rep("M", 200), Fhist = rep(c(1,2,1,2), each = 50), selhist = rep(1:2, each = 100), 
  rbind(M_scen11, M_scen12, M_scen21, M_scen22))

