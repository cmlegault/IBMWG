# 3_make_CJFAS_SupplMat_figs.R
# create the figures for the Supplemental Materials document associated with the CJFAS publication

library(tidyverse)

# remember to set working directory to source file location to begin

SSB_label <-expression(paste("SSB / ",SSB[MSY],sep = ""))
F_label <-expression(paste("F / ",F[MSY],sep = ""))
C_label <- "C / MSY"
sims <- readRDS("sims.RDS")
simsbig <- sims %>%
  filter(metric %in% c("avg_ssb_ssbmsy","avg_f_fmsy","avg_catch_msy"),
         time.avg == "L") %>%
  select(isim, IBMlab, Scenlab, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

scens <- unique(sims$Scenlab)

for (i in 1:length(scens)){
  thisdat <- simsbig %>%
    filter(Scenlab == scens[i])
  thisplot <- ggplot(thisdat, aes(x=avg_ssb_ssbmsy, y=avg_catch_msy)) +
    geom_point(alpha=0.15) +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~factor(IBMlab, level=rev(IBM_order_new))) +
    theme_classic() + 
    theme(text = element_text(family = "Times")) +
    labs(x=SSB_label, y=C_label, title=scens[i])
  png(filename = paste0("supplmat/", scens[i], "_C_vs_SSB.png"), width = 6, height = 6, units = "in", res=300)
    print(thisplot)
  dev.off()

  thisplot <- ggplot(thisdat, aes(x=avg_ssb_ssbmsy, y=avg_f_fmsy)) +
    geom_point(alpha=0.15, color="blue") +
    geom_vline(xintercept = 1, color="red", linetype="dashed") +
    geom_hline(yintercept = 1, color="red", linetype="dashed") +
    facet_wrap(~factor(IBMlab, level=rev(IBM_order_new))) +
    theme_classic() + 
    theme(text = element_text(family = "Times")) +
    labs(x=SSB_label, y=F_label, title=scens[i])
  png(filename = paste0("supplmat/", scens[i], "_F_vs_SSB.png"), width = 6, height = 6, units = "in", res=300)
  print(thisplot)
  dev.off()
  
}

