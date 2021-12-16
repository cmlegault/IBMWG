# IBMWG plotting based on the summary tables
library(ggplot2); library(gridExtra); library(grid); library(ggpubr)


# specify order for IBMs so Ensemble and SCAA are at the end
IBM_order <- rev(c("AIM","CC-FM","CC-FSPR","DLM","ES-FM","ES-Frecent",
               "ES-FSPR","ES-Fstable","Islope","Ismooth","Itarget",
               "Skate","Ensemble","SCAA"))

# diffuse methods 
dif <- c("CC-FM","CC-FSPR","DLM","ES-Frecent","Islope","Ismooth")


# median values across ass scenarios
all_scen <- read.csv("PMs_all_scenarios_new.csv",
                     header=TRUE,stringsAsFactors=FALSE) 
retro <- read.csv("PMs_by_retro_new.csv",
                  header=TRUE,stringsAsFactors=FALSE)
Cmult <- read.csv("PMs_by_Cmult.csv",
                  header=TRUE,stringsAsFactors=FALSE)
Cmult$Category <- "Linear"
Cmult$IBM[Cmult$IBM=="PBS"] <- "Ismooth"
Cmult$Category[Cmult$IBM==dif[1] | Cmult$IBM==dif[2] | Cmult$IBM==dif[3] | Cmult$IBM==dif[4] | Cmult$IBM==dif[5] | Cmult$IBM==dif[6]] = "Diffuse"
Cmult$IBM=="DLM"
Cmult$cat_pch <- 1
Cmult$cat_pch[Cmult$Category=="Diffuse"] <- 16




retro_Fhist <- read.csv("PMs_by_retro_Fhistory_new.csv",
                        header=TRUE,stringsAsFactors=FALSE)
time_Fhist <- read.csv("PMs_by_time_Fhist_new.csv",
                    header=TRUE,stringsAsFactors=FALSE)
Cmult_Fhist <- read.csv("PMs_by_Cmult_Fhist_new.csv",
                        header=TRUE,stringsAsFactors=FALSE)
Cmult_Fhist$Catch_mult <- factor(Cmult_Fhist$Catch_mult)


# 

# Figure labels for each PM
SSB_label <-expression(paste("SSB / ",SSB[MSY],sep = ""))
F_label <-expression(paste("F / ",F[MSY],sep = ""))
C_label <- "C / MSY"
IAV_label <- "Relative variability in catch (IAV)"
PING_label <- "Probability of overfishing"
PED_label <- "Probability of being overfished"

# PMs aggregated across all scenarios
{
  quartz()
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
 #ggsave("PMs_all_scenarios",plot=p1,device="pdf",width=7,height=8)
 
}

# PMs by F history and Retro source
{
quartz()
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
print(p2)
#p2 <- grid.arrange(p)
ggsave("PMs_by_retro_Fhistory_new",plot=p2,device="pdf",width=7,height=8)
}


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
  ggsave("F_Fmsy_by_scens",plot=p3,device="pdf",width=6,height=6)  
}










