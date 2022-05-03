# 2_make_CJFAS_figs_rev1.R
# exploring alternative figures for CJFAS based on first round of reviewer comments

library(ggplot2); library(gridExtra); library(grid); library(ggpubr)
library(dplyr); library(tidyr)

# used for Windows 
windowsFonts(Times=windowsFont("Times New Roman"))
# end used for Windows section

# set working directory to source file location to begin

# specify order for IBMs so Ensemble and SCAA are at the end
IBM_order <- rev(c("AIM","CC-FM","CC-FSPR","DLM","ES-FM","ES-Frecent",
               "ES-FSPR","ES-Fstable","Islope","Ismooth","Itarget",
               "Skate","Ensemble","SCAA"))
IBM_order_new <- rev(c("AIM","CC-FM","CC-FSPR","DynLin","ES-FM","ES-Frecent",
                       "ES-FSPR","ES-Fstable","Islope","Ismooth","Itarget",
                       "Skate","Ensemble","SCAA"))

# get median values across scenarios
all_scen <- read.csv("../MS_tables_figs/PMs_all_scenarios_new.csv",
                     header=TRUE,stringsAsFactors=FALSE) 
retro_Fhist <- read.csv("../MS_tables_figs/PMs_by_retro_Fhistory_new.csv",
                        header=TRUE,stringsAsFactors=FALSE)
time_Fhist <- read.csv("../MS_tables_figs/PMs_by_time_Fhist_new.csv",
                    header=TRUE,stringsAsFactors=FALSE)
Cmult_Fhist <- read.csv("../MS_tables_figs/PMs_by_Cmult_Fhist_new.csv",
                        header=TRUE,stringsAsFactors=FALSE)
Cmult_Fhist$Catch_mult <- factor(Cmult_Fhist$Catch_mult)

# get more data
ssb_median_by_scenario <- readRDS("../tables_figs/ssb_median_by_scenario.rds") %>%
  mutate(metric = paste0("ssb_", metric))

f_median_by_scenario <- readRDS("../tables_figs/f_median_by_scenario.rds") %>%
  mutate(metric = paste0("f_", metric))

catch_median_by_scenario <- readRDS("../tables_figs/catch_median_by_scenario.rds") %>%
  mutate(metric = paste0("catch_", metric))

td_med <- rbind(ssb_median_by_scenario, f_median_by_scenario, catch_median_by_scenario)

td3_l_med <- td_med %>%
  filter(metric %in% c("ssb_l_avg_ssb_ssbmsy", "catch_l_avg_catch_msy")) %>%
  distinct() %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  rename(x_value = ssb_l_avg_ssb_ssbmsy, y_value = catch_l_avg_catch_msy)

sims <- readRDS("sims.RDS")
sims5 <- readRDS("../MS_tables_figs/Fig5_sims.rds")


# NOTE: have to rename DLM for revised figures


# Figure labels for each PM
SSB_label <-expression(paste("SSB / ",SSB[MSY],sep = ""))
F_label <-expression(paste("F / ",F[MSY],sep = ""))
C_label <- "C / MSY"
IAV_label <- "Relative variability in catch (IAV)"
PING_label <- "Probability of overfishing"
PED_label <- "Probability of being overfished"
NING_label <- "Number of years overfishing"
NED_label <- "Number of years overfished"

# Figure 1 ---------------------------------------------------------------
# PMs aggregated across all scenarios
{
  #quartz() # can only run this on Mac
  pSSB<-ggplot(data=all_scen, aes(x=factor(IBM,level=IBM_order), y=SSB)) +
  geom_bar(stat="identity",fill="gray")+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(SSB_label) + ggtitle("A")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()

pF<-ggplot(data=all_scen, aes(x=factor(IBM,level=IBM_order), y=F)) +
  geom_bar(stat="identity",fill="gray")+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(F_label) + ggtitle("B")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()

pC<-ggplot(data=all_scen, aes(x=factor(IBM,level=IBM_order), y=Catch)) +
  geom_bar(stat="identity",fill="gray")+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(C_label) + ggtitle("C")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()

pIAV<-ggplot(data=all_scen, aes(x=factor(IBM,level=IBM_order), y=IAV)) +
  geom_bar(stat="identity",fill="gray")+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(IAV_label) + ggtitle("D") +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()

pOFG<-ggplot(data=all_scen, aes(x=factor(IBM,level=IBM_order), y=PING)) +
  geom_bar(stat="identity",fill="gray")+
  theme_classic() + 
  theme(text=element_text(family="Times"))+ 
  xlab("") + ylab(PING_label) + ggtitle("E") +
  scale_y_continuous(expand = c(0,0))+
  geom_hline(yintercept = 0.5)+ 
  coord_flip()

pOFD<-ggplot(data=all_scen, aes(x=factor(IBM,level=IBM_order), y=PED)) +
  geom_bar(stat="identity",fill="gray")+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(PED_label) + ggtitle("F") +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()

 p1 <- grid.arrange(pSSB,pF,pC,pIAV,pOFG,pOFD,nrow=3)
 ggsave("Figure_1_orig.pdf",plot=p1,device="pdf",width=7,height=8)
}

# Alternative 1 for Figure 1
# add points showing medians for each of the 16 scenarios
pSSBalt1 <- pSSB +
  geom_point(data=filter(td_med, metric %in% c("ssb_l_avg_ssb_ssbmsy", "ssb_s_avg_ssb_ssbmsy")), aes(x=IBMlab, y=value))
pFalt1 <- pF +
  geom_point(data=filter(td_med, metric %in% c("f_l_avg_f_fmsy", "f_s_avg_f_fmsy")), aes(x=IBMlab, y=value))
pCalt1 <- pC +
  geom_point(data=filter(td_med, metric %in% c("catch_l_avg_catch_msy", "catch_s_avg_catch_msy")), aes(x=IBMlab, y=value))
pIAValt1 <- pIAV +
  geom_point(data=filter(td_med, metric %in% c("catch_l_iav_catch", "f_s_iav_catch")), aes(x=IBMlab, y=value))
p1alt1 <- grid.arrange(pSSBalt1,pFalt1,pCalt1,pIAValt1,pOFG,pOFD,nrow=3)
ggsave("Figure_1_alt1.pdf",plot=p1alt1,device="pdf",width=7,height=8)

# Alternative 2 using violin plots
# something wrong with number of years plots (should not be able to have more than 26 years)
# could try to limit upper values of each plot to see the broader part of the violin better
{
pSSBalt2 <- ggplot(data=filter(sims, metric=="avg_ssb_ssbmsy"), aes(x=factor(IBMlab,level=IBM_order_new), y=value)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    theme_classic() +
    theme(text=element_text(family="Times"))+
    xlab("") + ylab(SSB_label) + ggtitle("A")+
    geom_hline(yintercept = 1) +
    scale_y_continuous(expand = c(0,0))+ 
    coord_flip()
pFalt2 <- ggplot(data=filter(sims, metric=="avg_f_fmsy"), aes(x=factor(IBMlab,level=IBM_order_new), y=value)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_classic() +
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(F_label) + ggtitle("B")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()
pCalt2 <- ggplot(data=filter(sims, metric=="avg_catch_msy"), aes(x=factor(IBMlab,level=IBM_order_new), y=value)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_classic() +
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(C_label) + ggtitle("C")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()
pIAValt2 <- ggplot(data=filter(sims, metric=="iav_catch"), aes(x=factor(IBMlab,level=IBM_order_new), y=value)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_classic() +
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(IAV_label) + ggtitle("D")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()
pINGalt2 <- ggplot(data=filter(sims, metric=="n_gr_fmsy"), aes(x=factor(IBMlab,level=IBM_order_new), y=value)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_classic() +
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(NING_label) + ggtitle("E")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()
pEDalt2 <- ggplot(data=filter(sims, metric=="n_less_05_bmsy"), aes(x=factor(IBMlab,level=IBM_order_new), y=value)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_classic() +
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(NED_label) + ggtitle("F")+
  geom_hline(yintercept = 1) +
  scale_y_continuous(expand = c(0,0))+ 
  coord_flip()
p1alt2 <- grid.arrange(pSSBalt2,pFalt2,pCalt2,pIAValt2,pINGalt2,pEDalt2,nrow=3)
ggsave("Figure_1_alt2.pdf",plot=p1alt2,device="pdf",width=7,height=8)
}

# Figure 2 --------------------------------------------------------------- 
# Median long term C/MSY vs SSB/SSBmsy
{
  point_cols <- c("black","dark gray")
  
  fig2plot <- ggplot(td3_l_med, aes(x=x_value, y=y_value, color=retro_type)) +
    geom_point() +
    facet_wrap(~factor(IBMlab, level=rev(IBM_order))) +
    theme_classic() + 
    theme(text = element_text(family = "Times")) +
    labs(x=SSB_label, y=C_label) +
    scale_color_manual(values=point_cols)
  ggsave(filename = "Figure_2_orig.pdf", plot=fig2plot, device="pdf",width=6,height=6)
}

# Figure 3 ---------------------------------------------------------------
# PMs by F history and Retro source
{
#quartz() # can only run this on Mac
point_cols <- c("black","dark gray")
pSSB_retro<-ggplot(data=retro_Fhist, aes(x=factor(IBM,level=IBM_order), y=SSB)) +
  geom_point((aes(colour = Retro_source, shape=F_history)))+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(SSB_label) + ggtitle("A")+
  geom_hline(yintercept = 1) +
  scale_color_manual(values=point_cols)+
  coord_flip()

pF_retro<-ggplot(data=retro_Fhist, aes(x=factor(IBM,level=IBM_order), y=F)) +
  geom_point((aes(colour = Retro_source, shape=F_history)))+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(F_label) + ggtitle("B")+
  geom_hline(yintercept = 1) +
  scale_color_manual(values=point_cols)+
  coord_flip()

pC_retro<-ggplot(data=retro_Fhist, aes(x=factor(IBM,level=IBM_order), y=Catch)) +
  geom_point((aes(colour = Retro_source, shape=F_history)))+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(C_label) + ggtitle("C")+
  geom_hline(yintercept = 1) +
  scale_color_manual(values=point_cols)+
  coord_flip()

pIAV_retro<-ggplot(data=retro_Fhist, aes(x=factor(IBM,level=IBM_order), y=IAV)) +
  geom_point((aes(colour = Retro_source, shape=F_history)))+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(IAV_label) + ggtitle("D")+
  scale_color_manual(values=point_cols)+
  coord_flip()

pOFG_retro<-ggplot(data=retro_Fhist, aes(x=factor(IBM,level=IBM_order), y=PING)) +
  geom_point((aes(colour = Retro_source, shape=F_history)))+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(PING_label) + ggtitle("E")+
  geom_hline(yintercept = 0.5) +
  scale_color_manual(values=point_cols)+
  coord_flip()

pOFD_retro<-ggplot(data=retro_Fhist, aes(x=factor(IBM,level=IBM_order), y=PED)) +
  geom_point((aes(colour = Retro_source, shape=F_history)))+
  theme_classic() + 
  theme(text=element_text(family="Times"))+
  xlab("") + ylab(PED_label) + ggtitle("F")+
  scale_color_manual(values=point_cols)+
  coord_flip()

p2 <-ggarrange(pSSB_retro,pF_retro,pC_retro,pIAV_retro,pOFG_retro,pOFD_retro, 
               ncol=2, nrow=3, common.legend = TRUE, legend="bottom")
ggsave("Figure_3_orig.pdf",plot=p2,device="pdf",width=7,height=8)
}

# Figure 4 ---------------------------------------------------------------
# F / Fmsy by Cmult, and  Short vs Long
{
  pF_time_Fhist<-ggplot(data=time_Fhist, aes(x=factor(IBM,level=IBM_order), y=F)) +
    geom_point((aes(colour = Period, shape=F_history)))+
    theme_classic() + 
    theme(text=element_text(family="Times"))+
    xlab("") + ylab(F_label) + ggtitle("A")+
    geom_hline(yintercept = 1) +
    scale_color_manual(values=point_cols)+
    coord_flip()
  
  pF_Cmult_Fhist<-ggplot(data=Cmult_Fhist, aes(x=factor(IBM,level=IBM_order), y=F)) +
    geom_point((aes(colour = Catch_mult, shape=F_history)))+
    theme_classic() + 
    theme(text=element_text(family="Times"))+
    xlab("") + ylab(F_label) + ggtitle("B")+
    geom_hline(yintercept = 1) +
    scale_color_manual(values=point_cols)+
    coord_flip()

  p3 <- grid.arrange(pF_time_Fhist,pF_Cmult_Fhist,nrow=2)
  ggsave("Figure_4_orig.pdf",plot=p3,device="pdf",width=6,height=6)  
}

# Figure 5 --------------------------------------------------------------- 
# Long term C/MSY vs SSB/SSBmsy by realization and IBM
# uses sceanario CF1A (see make_MS_tables_figures.R at bottom)
{
fig5plot <- ggplot(sims5, aes(x=l_avg_ssb_ssbmsy, y=l_avg_catch_msy)) +
  geom_point() +
  geom_vline(xintercept = 1, color="red", linetype="dashed") +
  geom_hline(yintercept = 1, color="red", linetype="dashed") +
  facet_wrap(~factor(IBMlab, level=rev(IBM_order))) +
  theme_classic() + 
  theme(text = element_text(family = "Times")) +
  labs(x=SSB_label, y=C_label)
ggsave(filename = "Figure_5_orig.pdf", plot=fig5plot, device="pdf", width=6,height=6)
}




