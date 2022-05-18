


#	PCA 20220509

#	pulled input data from # 2_make_CJFAS_figs_rev1.R


#	change format for PCA
df_wide <- reshape(data.frame(medIBM), idvar="IBMlab", timevar="metric", v.names="medval", direction="wide", sep="_")
colnames(df_wide)<-c('IBMlab','MSY','Fmsy','SSBmsy','IAV','N_Fmsy','N_Bmsy')
df_wide

#	run PCA
cs.pca <- prcomp(df_wide[,2:7],center = TRUE,scale. = TRUE)



plot(cs.pca[[1]]^2/sum(cs.pca[[1]]^2), type = "l")	#proportion

# plot method
dev.new()
plot(cs.pca, type = "l")

summary(cs.pca)

Importance of components:
                         PC1   PC2     PC3     PC4     PC5     PC6
Standard deviation     2.174 1.004 0.44262 0.21615 0.11732 0.08681
Proportion of Variance 0.788 0.168 0.03265 0.00779 0.00229 0.00126
Cumulative Proportion  0.788 0.956 0.98866 0.99645 0.99874 1.00000

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(cs.pca, obs.scale = 1, var.scale = 1,labels=df_wide[,1]) 

#	creating simple groups
create.groups<-c(4,2,2,1,5,4,1,4,4,1,1,3,1,4)
#	creating same groups, but with better names for legend
create.groups<-c('High F','Conser','Conser','Med','Var','High F','Med','High F','High F','Med','Med','Low Var','Med','High F')

Par(mfrow=c(2,1))
#	simple plot
ggbiplot(cs.pca, obs.scale = 1, var.scale = 1,labels=df_wide[,1],var.axes=FALSE) 
#	Plot with features
ggbiplot(cs.pca, obs.scale = 1, var.scale = 1,labels=df_wide[,1],ellipse=TRUE, groups=create.groups) 




##############################
summary plot for orginal Fig 3

medretro_Fhist$retro.label<-'M'
medretro_Fhist$retro.label[medretro_Fhist$Retro_source=='Catch']<-'C'

medretro_Fhist$f.label<-'F'
medretro_Fhist$f.label[medretro_Fhist$F_history=='Fmsy']<-'Fmsy'

medretro_Fhist$label<-paste(medretro_Fhist$IBMlab,'-',medretro_Fhist$retro.label,'-',medretro_Fhist$f.label,sep='')
head(medretro_Fhist)
medretro_Fhist$label[1:8]



df_wide <- reshape(data.frame(medretro_Fhist), idvar="label", timevar="metric", v.names="medval", direction="wide", sep="_")
colnames(df_wide)[9:14]<-c('MSY','Fmsy','SSBmsy','IAV','N_Fmsy','N_Bmsy')
df_wide

cs.pca <- prcomp(df_wide[,9:14],center = TRUE,scale. = TRUE)
pca.labels<-df_wide[,8]

plot(cs.pca[[1]]^2/sum(cs.pca[[1]]^2), type = "l")	#proportion
# plot method
dev.new()
plot(cs.pca, type = "l")

summary(cs.pca)

Importance of components:
                          PC1    PC2     PC3     PC4     PC5     PC6
Standard deviation     2.0814 0.9671 0.71905 0.39257 0.22707 0.09796
Proportion of Variance 0.7221 0.1559 0.08617 0.02569 0.00859 0.00160
Cumulative Proportion  0.7221 0.8780 0.96412 0.98981 0.99840 1.00000

str(cs.pca)
	
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(cs.pca, obs.scale = 0, var.scale = 1,labels=pca.labels) #	msy is on the y-axis


