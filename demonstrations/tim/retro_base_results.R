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


M_scen11 = estimate_retro_M_rho(M_ratio = 1.6) #SSB 0.54, F -0.36, R 0.65
M_scen12 = estimate_retro_M_rho(M_ratio = 1.8, Fhist = 2) #SSB 0.47, F -0.36, R 0.67
M_scen21 = estimate_retro_M_rho(M_ratio = 1.6, n_selblocks = 2) #SSB 0.50, F -0.35, R 0.60
M_scen22 = estimate_retro_M_rho(M_ratio = 1.8, n_selblocks = 2, Fhist = 2) #SSB 0.47, F -0.36, R 0.68

t(sapply(paste0('scen',rep(1:2, each = 2), rep(1:2,2)), function(x)apply(get(x)[[1]][,1:3],2,median)))

t(sapply(paste0('Catch_scen',rep(1:2, each = 2), rep(1:2,2)), function(x)apply(get(x)[[1]][,1:3],2,median)))

t(sapply(paste0('M_scen',rep(1:2, each = 2), rep(1:2,2)), function(x)apply(get(x)[[1]][,1:3],2,median)))

save(scen11,scen12,scen21,scen22,Catch_scen11,Catch_scen12,Catch_scen21,Catch_scen22,M_scen11,M_scen12,M_scen21,M_scen22, file = "demonstrations/tim/retro_res.RData")
#load("demonstrations/tim/retro_res.RData")
block = cbind.data.frame(retro_type = rep("None", 200), Fhist = rep(c(1,2,1,2), each = 50), selhist = rep(1:2, each = 100))
for(i in c("SSB","Fbar","R")) 
{
  if(i == "SSB") dat = cbind(block, type = i, retro = c(scen11[[1]][,i], scen12[[1]][,i], scen21[[1]][,i], scen22[[1]][,i]))
  else dat = rbind(dat, cbind(block, type = i, retro = c(scen11[[1]][,i], scen12[[1]][,i], scen21[[1]][,i], scen22[[1]][,i])))
}
block = cbind.data.frame(retro_type = rep("Catch", 200), Fhist = rep(c(1,2,1,2), each = 50), selhist = rep(1:2, each = 100))
for(i in c("SSB","Fbar","R")) 
{
    dat = rbind(dat, cbind(block, type = i, retro = c(Catch_scen11[[1]][,i], Catch_scen12[[1]][,i], Catch_scen21[[1]][,i], Catch_scen22[[1]][,i])))
}
block = cbind.data.frame(retro_type = rep("M", 200), Fhist = rep(c(1,2,1,2), each = 50), selhist = rep(1:2, each = 100))
for(i in c("SSB","Fbar","R")) 
{
    dat = rbind(dat, cbind(block, type = i, retro = c(M_scen11[[1]][,i], M_scen12[[1]][,i], M_scen21[[1]][,i], M_scen22[[1]][,i])))
}

library(ggplot2)

p = ggplot(dat, aes(x=type, y=retro, fill=retro_type)) + 
    geom_boxplot() +
    facet_wrap(~retro_type*Fhist*selhist)

cairo_pdf("demonstrations/tim/retro_results_plots.pdf", family = "Times", height = 10, width = 10)
p
dev.off()

png("demonstrations/tim/retro_results_plots.png", family = "Times", units = "in", type = "cairo-png", height = 10, width = 10, res = 500)
p
dev.off()
