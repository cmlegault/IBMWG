#	output of WHAM model is a large list named 'y'
#	this section should take the components of y and create a single index, a single catch, single catch at age, etc... 
#	so that the same inputs go into the different index based methods and changes can be made once, here.

load("ywham_oneflt.RData")

#	current index based methods
#	The first component for each method will be the catch, all other components from that method will be subsequent in a list


#	Joe Langan is a place holder for his function
index_methods<-c('Islope','Itarget','true_Skate_CR','M_CC','planBsmooth','ExpandSurvey_modified','run.aim','joe.langan','ensemble')

#	other functions
#DLM_z - catch curve analysis used in M_CC, outputs slope
#spr- Spawner per recruit function to derive an Fmsy proxy
#aim.stable.period - modification of the aim method that determines when a histrocially stable period existed.  Output the years it existed
#	Is there a function to produce a relative F, something like total catch over survey biomass ?


#	should create single index that is consistent for all index based methods
	#	combining base period and projection period into single object
y$seasonal_index<-rbind(y$agg_indices, y$agg_indices_proj)	#	seasonal indices as two columns.  rbind to combine base period with feedback period
#took mean of fall 2019 and spring 2020 as agreed on July 2 meeting
	temp.index<-data.frame(spr=c(y$seasonal_index[,1],0),fall=c(0,y$seasonal_index[,2]))
y$index<-rowMeans(temp.index[1:(nrow(temp.index)-1),])	#	check:Should this start at row 2?,  I had it start at row one so that the number of years stays consistent with catch even though year one only has one survey, not two with this particular method

y$years<-seq(1,length(y$index),1)

#	should create single catch that is consistent for all index based methods
y$catch<-rbind(y$agg_catch, y$agg_catch_proj)	#assuming one fleet

#	numbers at age in each survey
obs_survey_NAA_func<-function(y){
base_yr<-nrow(y$agg_indices)	#	yrs in base period
proj_yr<-nrow(y$agg_indices_proj)	#	yrs in feedback period, updates every run

#	base period and projections are two different objects within the list, index_paa is 3D array (first dimension is which survey)
#	survey N = survey B/(prop at age in N * weight at age)
survey_one_N_base<-y$agg_indices[,1]/rowSums(y$index_paa[1,,]*y$waa[y$waa_pointer_indices[1],1:base_yr,])	
survey_two_N_base<-y$agg_indices[,2]/rowSums(y$index_paa[2,,]*y$waa[y$waa_pointer_indices[2],1:base_yr,])

survey_one_N_proj<-y$agg_indices_proj[,1]/rowSums(y$index_paa_proj[1,,]*y$waa[y$waa_pointer_indices[1],(base_yr+1):sum(base_yr,proj_yr),])	#	base period and projections are two different objects within the list, index_paa is 3D array 
survey_two_N_proj<-y$agg_indices_proj[,2]/rowSums(y$index_paa_proj[2,,]*y$waa[y$waa_pointer_indices[2],(base_yr+1):sum(base_yr,proj_yr),])

#	survey NAA = survey N * proportion at age in N
caa_survey_one_base<-survey_one_N_base*y$index_paa[1,,]
caa_survey_two_base<-survey_two_N_base*y$index_paa[2,,]

caa_survey_one_proj<-survey_one_N_proj*y$index_paa_proj[1,,]
caa_survey_two_proj<-survey_two_N_proj*y$index_paa_proj[2,,]

#	combining base period and projection
caa_survey_one<-rbind(caa_survey_one_base,caa_survey_one_proj)
caa_survey_two<-rbind(caa_survey_two_base,caa_survey_two_proj)

caa_survey<-array(c(caa_survey_one,caa_survey_two),dim=c(nrow(caa_survey_one),ncol(caa_survey_one),2)  )
caa_survey
}	#	end function

y$index_naa<-obs_survey_NAA_func(y)

#	create single natural mortality that is consistent for all index based methods
y$M<-mean(y$MAA[1,])	#	Plan is for natural mortality to be constant with age, but possibly exhibit a ramp at some point in time series.
				#	WHAM output enables age specific and year specific natural mortality
				#	plan is to simply take M in year one of the base period, M prior to the ramp and average across years, but all the years should be the same. 

#	check:catchability - currently q is time invariant, but at some point WHAM will likely output annual q for the two surveys and have a ramp
#	If there is an annual q  it takes the first year of the base period
if(is.null(nrow(y$q))==TRUE){
y$init_q<-y$q
}
if(is.null(nrow(y$q))==FALSE){
y$init_q<-y$q[1,]
}
		

#-------Spawners per recruit -----------------------------
	###JJD SPR from ASAPPlots

s.per.recr<-function(y,F.mult=NULL,spawn.time=NULL) {
  #	nages,mat.age,M.age, F.mult, sel.age, spawn.time 
nages<-ncol(y$mature)
mat.age<-y$mature[nrow(y$mature),]	#	check: assumes using the most recent year
M.age<-y$MAA[1,]		#	check: assumes we know the true M and it is not mis-specified

#	check: which selectivity (final year?) and should we know the true value or with obs error.  This is the true value in the final year
sel.age<-y$selAA[[1]][nrow(y$selAA[[1]]),]	#	number one should be the fishery selectivity

  spr=0.0
  cum.survive=1.0
  z=0.0
  for (i in 1:(nages-1)  ) {
    z=M.age[i] + F.mult*sel.age[i]
    z.ts=(M.age[i]+F.mult*sel.age[i])*spawn.time
    spr=spr+cum.survive*mat.age[i]*exp(-z.ts)
    cum.survive=cum.survive*exp(-z )
    
  }
  
  z= M.age[nages] + F.mult*sel.age[nages]
  z.ts=(M.age[nages]+F.mult*sel.age[nages])*spawn.time
  spr=spr + mat.age[nages]*cum.survive*exp(-z.ts)/( 1- exp(-z ) )
  
  return(spr)
  
}
###end SPR as done by JJD; adapted from ASAPPlots

	#	This function runs the s.per.recr function to generate the proxy Fmsy (e.g. F40%)
	#	and adds it to the larger y list.  The index based methods can then pull the value from the Y list as opposed to rerunning it each iteration
run.spr<-function(y){
spr0<- s.per.recr(y=y,F.mult=0,spawn.time=y$fracyr_SSB[length(y$fracyr_SSB)])
    F.start <-0.11  # starting guess for optimization routine to find F_SPR%
    t.spr <- y$percentSPR/100
    spr.f <- function(F.start) {
      abs(s.per.recr(y=y,F.mult=F.start, spawn.time=y$fracyr_SSB[length(y$fracyr_SSB)])/spr0 - t.spr )
    }
    yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
    f.spr.vals <- yyy$par #Fx%
return(f.spr.vals)
}
y$F_SPR<-run.spr(y)



#----------------------------------------------




#	additional components needed in the list for each index based method, components can be changed here

#	Islope
y$Islope_yrsmth<-5
y$Islope_version<-1
y$Islope_cap<-FALSE

#	Itarget	
y$Itarget_ref_yrs<-25	#	NEEDS TO BE DETERMINED
y$Itarget_yrsmth<-3
y$Itarget_cmult<-1
y$Itarget_w<-0.5

#	true_Skate_CR	#	actual skate control rule used by NEFSC
y$true_Skate_CR_yrsmth<-3
y$true_Skate_CR_assess_span<-2	#	This is the frequency of assessments and should line up with the plan for the simulations
y$true_Skate_CR_percent<-0.75		#	proportion of output assuming it is an ABC that should be the ACT, needs modification

#	DLM_Z <-function(CAA,yrs=3) #	catch curve
y$DLM_Z_yrs<-3


#	M_CC	# Simple method that used catch curve to estimate Z, and assumes Fmsy = assumed M
y$M_CC_yrs<-3
y$M_CC_Fmin<-0.05
y$M_CC_method<-3
#1.	SPR
#2.	Defunct - used to be Stable historic period but that doesn't make sense for M_CC
#3.	Natural mortality



#	check

#	Created a modified expanded survey function that accounts for two surveys.  
#		It changes two things
#	1. adds a scaler to modify the true q, we may want to add some variability to the scaler
#	2. takes in the two surveys scales them by a q*scaler for each season
#	2b. combines the expanded survey biomass for the two seasons into one 
#		Currently happens in the expand function as a place holder and needs to be corrected
#  y$q is an object from WHAM and has two values.  In Jon's original function simply selected the spring q
y$expand_method=1 
#1.	SPR
#2.	Stable historic period
#3.	Natural mortality
#4.	Average of recent catch
#expand_yrs also used by PlanBSmooth to ID how many years of catch to average for use with multiplier
y$expand_yrs=3 #if method=4 then number of years to average for exploitation rates.
y$expand_q_scaler<-1.0	#	proportion to scale true q can be greater than or less than one, but plan is to use true, non-ramp q  


#for AIM and get.stable.period
y$AIM_fscalar = 1
y$AIM_I_smooth=5
y$AIM_F_smooth=3
y$AIM_center=T
y$AIM_Fscalar<-1
y$AIM_plot=F
y$AIM_avg<-T
y$AIM_npts<-10		#	number of years to include, default is one

#-----------------------------------------

get.stable.period <- function(y) {

#	(catch, index, I.smooth=5, F.smooth=3, center=T, F.scalar=1, npts=1, avg=F, plot=T )
#######################################################
## exploration of AIM toolbox model
## use AIM to identify stable year/index/catch values corresponding to replacement F
##
## Liz Brooks
## Created on:  31 August 2009
## Last updated: 10 August 2020
#######################################################


# Note 10 August 2020: this function builds on my AIM code;
# AIM finds a relative F ("replacement F") where the population would be stable (via regression of replacement ratio on relative F)
# i then calculate the euclidean distance from observed points [log(relative F), log(replacement ratio)] and select the point with the minimum distance to the replacement F
# stable.index is the index value at this closest point
# stable.catch is the catch value at this closest point
# stable.year is the year position at this closest point (vector of year is not passed, so this is the just the place in that unknown year vector)

#catch is a vector of total catch in biomass
#index is a vector of total index in biomass
#I.smooth is number of years of index to smooth over (5 years default)
#F.smooth is number of years of relative F to smooth over (3 years default)
#center T or F to center the lags on the midpoint of smoothing interval for relative F, or lag backwards
#F.scalar is a constant that scales relative F to calculate catch as index[nyears]*relativeF*F.scalar
#npts is the number of points nearest replacement F to calculate the "stable period" (note: they are not always adjacent index values--suggest 1 yr as default); if npts>1, and avg=T then i return the average index value and average catch across those npts, if avg=F i return the npts for index and catch
   
# NOTE: assumes same number of years in catch and index vector!!   
   
catch<-y$catch
index<-y$index
I.smooth=y$AIM_I_smooth
F.smooth=y$AIM_F_smooth
center=y$AIM_center
F.scalar<-y$AIM_Fscalar
plot=y$AIM_plot
avg<-y$AIM_avg
npts<-y$AIM_npts

nyears <- length(index)
years <- seq(1,nyears)
dc <- dim(catch)
if(is.null(dc)==F)  catch <- apply(catch,1,sum)

# Calculate Replacement Ratio (rr)
rr= rep(NA, nyears)
denom=1.0
for (rrloop in (1+I.smooth):nyears) {
   
   denom= sum(index[(rrloop-I.smooth):(rrloop-1)])/I.smooth
   rr[rrloop]=index[rrloop]/denom
} #end rrloop  



# Calculate Relative F (ff)

if (center==T) {
   ff= rep(NA, nyears)
   denom=1.0
   ctr = trunc(F.smooth/2)
   
   for (floop in 1:nyears) {
      if (floop==1  )  {
         denom= sum(index[1:2])/2
         ff[floop]=catch[floop]/denom
      }
      
      if ((floop)>1  &  (floop+F.smooth-ctr-1)<=(nyears)  ) {
         denom= sum(index[(floop-ctr):(floop+F.smooth-ctr-1)])/F.smooth
         
         ff[floop]=catch[floop]/denom
      }
      
      if ((floop+F.smooth-ctr -1)>(nyears)   ) {
         divis = length(index[(floop-ctr):nyears])
         denom= sum(index[(floop-ctr):nyears])/divis
         ff[floop]=catch[floop]/denom
      }
   } #end floop
   
   } #end ctr if statement

if (center==F) {
   ff= rep(NA, nyears)
   denom=1.0
 
   for (floop in F.smooth:nyears) {
 
      denom= sum(index[(floop-F.smooth+1):floop])/F.smooth
      ff[floop]=catch[floop]/denom
 
   } #end floop
   
} #end ctr if statement



## autocorrelation in index
ind.ac <- acf(index, lag.max=1, plot=F)

#cross-correlation between catch and index
catch.ind.ccf <- ccf(catch, index, lag.max=5, plot=F) 


#Replacement ratio regression
df <- as.data.frame(cbind(R=log(rr[(I.smooth+1):nyears]), F=log(ff[(I.smooth+1):nyears])))
ln.rr <- lm(R~F, data=df)
reg.pars <- summary(ln.rr)

#Solve for ln(relative F) where ln(Replacement ratio) =0    
#fstart = log(ff[1])
fstart = log(ff[which(is.na(ff)==F)] [10])

get.r.int <- function(fstart) {
   tmp.r <- reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*fstart 
   return(abs(tmp.r)  )
}   # end get.yield.f.min function

repl.f <- nlminb( start=fstart , objective=get.r.int, lower=min(log(ff), na.rm=T), upper=max(log(ff), na.rm=T),
                  control=list(eval.max=500, iter.max=200  )   )


# retransform repl.f (median)
rel.f.soln <- exp(repl.f$par)
proj.catch <- F.scalar*rel.f.soln*index[nyears]



#  New stuff to find stable period (2020 aug) ====

rr.ff.vec <- cbind(log(ff), log(rr))
edist <- function(x, rF=repl.f$par) { sqrt( (x[1]-rF)^2 + x[2]^2) }
edist.relF <- apply(rr.ff.vec, 1, edist)

shortest <- order(edist.relF)[1:npts]
stable.index <- index[shortest]
stable.year <- shortest  # this is just the position in the vector, not a year value
stable.catch <- catch[shortest]

# Note: if npts >1, then you might want to average or take median?
if (npts>1){
   if (avg==T) {
   stable.index <- mean(stable.index)
   stable.catch <- mean(stable.catch)
   #doesn't make sense to average stable.year
   }
}
   

if (plot==T){
   plot(log(ff), log(rr), xlab="Ln(Relative F)", ylab="Ln(Replacement Ratio)")   
   lines(x=log(ff), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*log(ff)), col='red')
   abline(h=0)
   abline(v=0)
   points(x=(repl.f$par), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*repl.f$par), pch=19, col='red')
   # paint closest point green:
   points(x=rr.ff.vec[shortest,1], y=rr.ff.vec[shortest,2], pch=20, col='green3')
   
   
   
   plot(seq(1,nyears), index, type='l', xlab="Year index", ylab="Index")
   points(years[shortest], index[shortest], col='green3', pch=20, cex=1.4)
   abline(h=stable.index, col='green3', lty=4)
   
} #end plot-test





stable.list <- list(stable.year=stable.year, stable.index=stable.index, stable.catch=stable.catch, proj.catch=proj.catch, F.replacement=rel.f.soln, repl.ratio=rr, rel.F=ff, catch.index.ccf=catch.ind.ccf, reg.pars=reg.pars, repl.f.calc=repl.f )

return(stable.list )

} #end function to identify stable period

y$stable.year<-get.stable.period(y)$stable.year

#-------------------------------------------

Islope <- function (y) 
{
  # y = output list from WHAM containing all inputs
  # index = single survey index over time, or an avg of multiple surveys
  # catch = total catch in wt by year
  # yrsmth = # of years you want to use or "smooth" over (default of 5 years)
  # version = which version of the method do you want to use (version 1 is the default)
  # cap = should you cap the target catch to only allow changes of 20% (TRUE = yes, FALSE = no (the default))
  
index<-y$index
catch<-y$catch
yrsmth<-y$Islope_yrsmth
version<-y$Islope_version
cap<-y$Islope_cap

  if(version > 4 | version < 1)
  {
    stop("You must enter a version between 1 and 4, 1=least conservative, 4 = most conservative")
  }
  
  # versions 1-4 of the Islope method (based on Geromont and Butterworth 2014)
  lambdas = c(0.4,0.4,0.4,0.2) # this param weights the slope stimates
  cmults = c(0.8,0.7,0.6,0.6)  # multiplier of recent avg catch
  
  lambda <- lambdas[version]
  cmult <- cmults[version]
  
  # which years of the index and catch vectors should be used - take the last yrsmth values
  iyrs <- (length(index) - (yrsmth - 1)):length(index)
  cyrs <- (length(catch) - (yrsmth - 1)):length(catch)
  
  i.use <- index[iyrs]
  c.use <- catch[cyrs]
  mu.C <- mean(c.use,na.rm=TRUE) # avg. catch
  C.last <- tail(catch,n=1) # most recent catch 
  
  yind <- 1:yrsmth
  slppar <- summary(lm(log(i.use) ~ yind))$coefficients[2, 1:2]
  #Islope <- rnorm(reps, slppar[1], slppar[2])  # this is if we want to use random draws based on pt estimate and std error
  Islope <- slppar[1]
  C.targ <- cmult * mu.C * (1 + lambda * Islope)
  
  if(cap==TRUE) # this caps the change at +/- 20% per year
  {
    C.targ[C.targ >1.2*C.last] <- 1.2 * C.last
    C.targ[C.targ < 0.8 * C.last] <- 0.8 * C.last
  }
  
  names(C.targ) <-NULL
  return(C.targ)
}

# default run
Islope(y)

# use a slightly more conservative option
y$Islope_version<-2	#	ranges 1:4
Islope(y)

# use a slightly more conservative option and more years in averageing
y$Islope_yrsmth<-7
Islope(y)

# use the most conservative option AND cap changes at 20%
y$Islope_cap<-TRUE
Islope(y)

Itarget <- function(y)
{
  # this is a hybrid of the skate CR ref points and the I target control rule 
  # ref_yrs is the number of years for the reference period - this should stay fixed throughout sim so we don't have shifting ref pts. 
  # Cmult determines the target catch (relative to the avg.)
  # w is a parameter (0-1) that determines how quickly catch declines between I.target and I.threshold 
  
  
index<-y$index
Ctot<-y$catch
ref_yrs<-y$Itarget_ref_yrs
yrsmth<-y$Itarget_yrsmth
cmult<-y$Itarget_cmult
w<-y$Itarget_w


  # moving avg - sides = 1 uses preceeding values; sides = 2 uses values on either side
  I.smooth <- stats::filter(index,rep(1/yrsmth,yrsmth), sides=1)
  
  # Target = 75th percentile of ref. period, threshold = 0.5 * I.targ
  I.targ <- quantile(I.smooth[1:ref_yrs],0.75,na.rm=TRUE)
  I.thresh <- 0.5 * I.targ
  
  C.star <- cmult*mean(Ctot[1:ref_yrs],na.rm=TRUE) # some target level of catch
  
  I.rec <- tail(I.smooth,1)  # recent smooth index
  
  if(I.rec >= I.thresh) # if above thresh catch increases linearly
  {
    C.targ <- w*C.star*(1+(I.rec-I.thresh)/(I.targ-I.thresh))
  }
  else if(I.rec < I.thresh) # if below threshold, decline quadratically
  {
    C.targ <- w*C.star * (I.rec/I.thresh)^2
  }
  
  names(C.targ)<-NULL
  
  return(C.targ)
}

Itarget(y)

#---------------------------
#	interpretation of the skate control rule from the skate spreadsheet

true_Skate_CR <- function(y)
{
  #	assess_span is the number of years since the last assessment.  From the spreadsheet, these years are not considered in the median C/B ratio

index<-y$index
catch<-y$catch
yrsmth<-y$true_Skate_CR_yrsmth
assess_span<-y$true_Skate_CR_assess_span
percent<-y$true_Skate_CR_percent		#	proportion of output assuming it is an ABC that should be the ACT, needs modification


   # moving avg - sides = 1 uses preceeding values; sides = 2 uses values on either side
  I.smooth <- stats::filter(index,rep(1/yrsmth,yrsmth), sides=1)
  C.smooth <- stats::filter(catch,rep(1/yrsmth,yrsmth), sides=1)


  c.b<-C.smooth/I.smooth/1000	
  median.c.b<-median(c.b[yrsmth:(length(c.b)-assess_span)],na.rm=T)	#	median C/B ratio over the entire time series minus the latest set of years, first few years of moving average are NA
  	
  abc<-I.smooth[length(I.smooth)]*median.c.b*1000		#	this appears to be the ABC, but the spread sheet also had a target C/B ratio but unknown how it was calculated
  act<-abc*percent	#	Not sure if we want to output the ABC or the ACT or...

  return(abc)
}

true_Skate_CR(y)



#--------------------
# this is a modfication of the way DLMtool calculates Z from catch at age data
# it returns Z estimate only and not a target catch.  
# The M_CC function returns a target catch by calling this function

DLM_Z <-function(y) 
{
#	method takes abundance at age from each survey calculates catch curve and then takes the mean of the coef.
  # caa = numerical catch at age in matrix form (rows=years,cols = ages)
  # yrs = how many years of data to use
 

CAA_one<-	y$index_naa[,,1]		#	check, make this change
CAA_two<-	y$index_naa[,,2]		#	check, make this change
yrs<-y$DLM_Z_yrs

    ny_one <- nrow(CAA_one) # total number of years in CAA matrix
    ny_two <- nrow(CAA_two) # total number of years in CAA matrix
    use.rows_one<-(ny_one-yrs+1):ny_one
    use.rows_two<-(ny_two-yrs+1):ny_two
    Csum_one <- apply(CAA_one[use.rows_one, ], 2, sum,na.rm=TRUE) # sum up by column
    Csum_two <- apply(CAA_two[use.rows_two, ], 2, sum,na.rm=TRUE) # sum up by column
    
    maxage_one <- length(Csum_one) # max. age
    maxage_two <- length(Csum_two) # max. age
    AFS_one <- which.max(Csum_one)  # age at full selection
    AFS_two <- which.max(Csum_two)  # age at full selection
    AFS_one[AFS_one > (maxage_one - 3)] <- maxage_one - 3 # need at least 3 ages to do the regression
    AFS_two[AFS_two > (maxage_two - 3)] <- maxage_two - 3 # need at least 3 ages to do the regression
    
    y_one <- log(Csum_one[AFS_one:maxage_one]/sum(Csum_one[AFS_one:maxage_one], na.rm = T))
    y_two <- log(Csum_two[AFS_two:maxage_two]/sum(Csum_two[AFS_two:maxage_two], na.rm = T))
    xc_one <- 1:length(y_one)
    xc_two <- 1:length(y_two)
    y_one[y_one == "-Inf"] <- NA
    y_two[y_two == "-Inf"] <- NA
    mod_one <<- lm(y_one ~ xc_one)
    mod_two <<- lm(y_two ~ xc_two)
    #print(summary(mod))
    chk_one <- sum(is.na(coef(mod_one)))
    chk_two <- sum(is.na(coef(mod_two)))
    if(chk_one) 
    {
       return(NA)
     }
    else 
    {
       coefs_one <- summary(mod_one, weights = Csum_one[AFS_one:maxage_one])$coefficients[2,1:2]
       #print(coefs)
       coefs_one[is.nan(coefs_one)] <- NA
       names(coefs_one) <- NULL
    }
    if(chk_two) 
    {
       return(NA)
     }
    else 
    {
       coefs_two <- summary(mod_two, weights = Csum_two[AFS_two:maxage_two])$coefficients[2,1:2]
       #print(coefs)
       coefs_two[is.nan(coefs_two)] <- NA
       names(coefs_two) <- NULL
    }
mean_coef<-mean(c(-coefs_one[1],-coefs_two[1]))
return(mean_coef)
} # end DLM_Z function

# default
DLM_Z(y)

# use more years of data
y$DLM_Z_yrs<-5
DLM_Z(y)


# Simple method that used catch curve to estimate Z, and assumes Fmsy = assumed M
M_CC = function (y) 
{
    # catch = catch time series in weight
    # caa = numerical catch at age in matrix form (rows=years,cols = ages)
    # yrs = how many years of data to use
    # M = assumed natural mortality
    # Fmin is the minimum estimate of recent F to prevent very low values which causes really high target catches
  
catch<-y$catch
#	CAA<-	y$index_naa	#	 not needed because the y list gets passed to DLM_Z function to compute catch curve
M<-y$M
yrs<-y$M_CC_yrs
Fmin<-y$M_CC_Fmin
M_CC_method<-y$M_CC_method
    cyrs <- (length(catch) - (yrs - 1)):length(catch)
    c.use <- catch[cyrs]
    mu.C <- mean(c.use,na.rm=TRUE) # avg. catch
    Z.est <- DLM_Z(y)
    F.est <- Z.est - M
    F.est[is.na(F.est)] <- Fmin  # if you get an NA, replace with Fmin
    F.est[F.est < Fmin] <- Fmin  # if you get a value below Fmin, use Fmin
    
    Ac <- mu.C/((F.est/Z.est)*(1 - exp(-Z.est))) # approx biomass based on catch and estimate of F
   
if(M_CC_method==1){FMSY <- y$F_SPR/Z.est*(1-exp(-Z.est))}    	#	SPR
#if(M_CC_method==2){FMSY<-mean(catch[y$stable.year]/Ac[y$stable.year])  }		#	stable historic period; #check:Divides catch/biomass in each year and then takes mean, This one doesn't make sense for CC.  Ac is a single value, not time series.  Stable period only applies to expanded biomass
if(M_CC_method==3){FMSY <- M/Z.est*(1-exp(-Z.est))} 		#	natural mortality= Fmsy

    C.targ<- FMSY * Ac
    return(C.targ)
}

M_CC(y)


####plan B smooth
planBsmoothfxn<-function(y){
  devtools::install_github("cmlegault/PlanBsmooth")
  library(PlanBsmooth)
  library(ggplot2)
  library(dplyr)
  library(tidyr) 
  planBsmooth<-ApplyPlanBsmooth(data.frame("Year"=y$years,"avg"=y$index))
  meancatch=mean(y$catch[(length(y$catch)-(y$expand_yrs-1)):length(y$catch)])
  catch.advice=planBsmooth$multiplier*meancatch
  return(catch.advice)
}

planBsmoothfxn(y=y)


#------------------------
####Expanded survey biomass		#	Bell modification
ExpandSurvey_modified<-function(y){
	#	modified from above
	#	added a scaler to modify true q
	#	expanded each survey index individually and then combined them
	#	check:PLACE HOLDER ONLY, NEED TO DETERMINE HOW TO COMBINE SURVEYS	
	#	currently takes the mean of spring survey at time T and fall survey at time T-1
expanded_one<-y$seasonal_index[,1]/(y$init_q[1]*y$expand_q_scaler)
expanded_two<-y$seasonal_index[,1]/(y$init_q[2]*y$expand_q_scaler)
temp.calc<-data.frame(spr=c(expanded_one,0),fall=c(0,expanded_two))
expanded<-rowMeans(temp.calc)[1:nrow(y$seasonal_index)]	#	assumes spring survey is available in current year
f.spr.vals<-y$F_SPR	#	proxy Fmsy level from spawner per recruit (e.g. F40%)
  if(y$expand_method==4){
    exploit=y$catch/expanded
    #ifelse(exploit>1,print("Warning: Exploitation > 1"),print("Exploit OK, <1"))
    meanexploit<-mean(exploit[(length(exploit)-(y$expand_yrs-1)):length(exploit)])
    catch.advice=expanded[length(expanded)]*meanexploit
  } 
  if(y$expand_method==1) { #Fspr
    Z<-y$M+f.spr.vals 
    mu<-(f.spr.vals/Z)*(1-exp(-Z))
    catch.advice=expanded[length(expanded)]*mu
    }
  if(y$expand_method==2) {		#	stable historical period	
    mu<-mean(y$catch[y$stable.year]/expanded[y$stable.year])  		#	stable historic period		check:Divides catch/biomass in each year and then takes mean
    catch.advice=expanded[length(expanded)]*mu
    }
  if(y$expand_method==3) {		#	proxy Fmsy = natural mortality
  	proxy_F<-y$M
    Z<-y$M+proxy_F 
    mu<-(proxy_F/Z)*(1-exp(-Z))
    catch.advice=expanded[length(expanded)]*mu
    }
  
  return(catch.advice)
}
ExpandSurvey_modified(y=y)

#------------------------
####AIM; courtesy of Liz Brooks
run.aim <- function(y) {
  #catch is a vector of total catch in biomass
  #index is a vector of index of abundance
  #I.smooth is number of years of index to smooth over (5 years default)
  #F.smooth is number of years of relative F to smooth over (3 years default)
  #center T or F to center the lags on the midpoint of smoothing interval for relative F, or lag backwards
  #F.scalar is a constant that scales relative F to calculate catch as index[nyears]*relativeF*F.scalar
  
  # NOTE: assumes same number of years in catch and index vector!!   
  
catch<-y$catch
index<-y$index
I.smooth=y$AIM_I_smooth
F.smooth=y$AIM_F_smooth
center=y$AIM_center
F.scalar<-y$AIM_Fscalar
plot=y$AIM_plot

  nyears <- length(index)
  dc <- dim(catch)
  if(is.null(dc)==F)  catch <- apply(catch,1,sum)
  
  # Calculate Replacement Ratio (rr)
  rr= rep(NA, nyears)
  denom=1.0
  for (rrloop in (1+I.smooth):nyears) {
    
    denom= sum(index[(rrloop-I.smooth):(rrloop-1)])/I.smooth
    rr[rrloop]=index[rrloop]/denom
  } #end rrloop  
  
  
  
  # Calculate Relative F (ff)
  
  if (center==T) {
    ff= rep(NA, nyears)
    denom=1.0
    ctr = trunc(F.smooth/2)
    
    for (floop in 1:nyears) {
      if (floop==1  )  {
        denom= sum(index[1:2])/2
        ff[floop]=catch[floop]/denom
      }
      
      if ((floop)>1  &  (floop+F.smooth-ctr-1)<=(nyears)  ) {
        denom= sum(index[(floop-ctr):(floop+F.smooth-ctr-1)])/F.smooth
        
        ff[floop]=catch[floop]/denom
      }
      
      if ((floop+F.smooth-ctr -1)>(nyears)   ) {
        divis = length(index[(floop-ctr):nyears])
        denom= sum(index[(floop-ctr):nyears])/divis
        ff[floop]=catch[floop]/denom
      }
    } #end floop
    
  } #end ctr if statement
  
  if (center==F) {
    ff= rep(NA, nyears)
    denom=1.0
    
    for (floop in F.smooth:nyears) {
      
      denom= sum(index[(floop-F.smooth+1):floop])/F.smooth
      ff[floop]=catch[floop]/denom
      
    } #end floop
    
  } #end ctr if statement
  
  
  
  ## autocorrelation in index
  ind.ac <- acf(index, lag.max=1, plot=F)
  
  #cross-correlation between catch and index
  catch.ind.ccf <- ccf(catch, index, lag.max=5, plot=F) 
  
  
  #Replacement ratio regression
  df <- as.data.frame(cbind(R=log(rr[(I.smooth+1):nyears]), F=log(ff[(I.smooth+1):nyears])))
  ln.rr <- lm(R~F, data=df)
  reg.pars <- summary(ln.rr)
  
  #Solve for ln(relative F) where ln(Replacement ratio) =0    
  #fstart = log(ff[1])
  fstart = log(ff[which(is.na(ff)==F)] [10])
  
  get.r.int <- function(fstart) {
    tmp.r <- reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*fstart 
    return(abs(tmp.r)  )
  }   # end get.yield.f.min function
  
  repl.f <- nlminb( start=fstart , objective=get.r.int, lower=min(log(ff), na.rm=T), upper=max(log(ff), na.rm=T),
                    control=list(eval.max=500, iter.max=200  )   )
  
  if (plot==T){
    plot(log(ff), log(rr))   
    lines(x=log(ff), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*log(ff)), col='red')
    abline(h=0)
    abline(v=0)
    points(x=(repl.f$par), y=(reg.pars$coefficients[1,1] +  reg.pars$coefficients[2,1]*repl.f$par), pch=19, col='red')
  } #end plot-test
  
  # retransform repl.f (median)
  rel.f.soln <- exp(repl.f$par)
  proj.catch <- F.scalar*rel.f.soln*index[nyears]
  aim.settings <- list(F.smooth=F.smooth, I.smooth=I.smooth, center=center, nyears=nyears)
  aim.list <- list(proj.catch=proj.catch, F.replacement=rel.f.soln, repl.ratio=rr, rel.F=ff, ccf=catch.ind.ccf, reg.pars=reg.pars, repl.f.calc=repl.f, aim.settings=aim.settings )
  
  return(aim.list )
  
} #end run.aim function
####End AIM function

run.aim(y)$proj.catch

#----------------------------------
ensemble<-function(y=NULL){
  advice<-Islope(y)[1]
  advice<-c(advice,Itarget(y)[1])
  advice<-c(advice,true_Skate_CR(y))
  advice<-c(advice,planBsmoothfxn(y))
  advice<-c(advice,run.aim(y)$proj.catch)
  advice<-c(advice,M_CC(y))
  advice<-c(advice,ExpandSurvey_modified(y))
  return(mean(advice))
}
ensemble(y)

#--------------------------------------
#	Joe Langan's function once we get it - Place holder

#--------------------------------------
#	Alternative SPR function that uses a look up table instead of optimizing
#	I pulled this straight from Chris L's original simulation code. 
#	all the inputs are already part of the WHAM output. 
#	Does not have the time of year when spawning occurs, but has everything else. 
#	It uses one function from dplyr and that could be replaced if we want to keep it in base R

library(dplyr)

SPR_func<-function(y){
# calculate F.table for use in spawner per recruit
nsteps <- 2001
F.table <- data.frame(Fval = double(),
                      ypr = double(),
                      spr = double()  )
nages<-ncol(y$mature)
maturityatage<-y$mature[nrow(y$mature),]	#	check: assumes using the most recent year
M<-y$MAA[nrow(y$MAA),]		#	check: assumes we know the true M and it is not mis-specified
#	check: which selectivity (final year?) and should we know the true value or with obs error.  This is the true value in the final year
fisheryselectivityatage<-y$selAA[[1]][nrow(y$selAA[[1]]),]	#	number one should be the fleet selectivity
weightsatage<-y$waa[5,nrow(y$waa[5,,]),]	#	check: I believe 5 is SSB
ref_percentage<-y$percentSPR/100


for (istep in 1:nsteps){
  Fval <- (istep - 1)/1000
  yprval <- 0.0
  sprval <- 0.0
  Nval <- 1.0
  for (i in 1:nages){
    selx <- fisheryselectivityatage[i]
    waa <- weightsatage[i]
    maturity <- maturityatage[i]
    Zval <- Fval * selx  + M[i]
    yprval <- yprval + Nval * waa * Fval * selx * (1 - exp(-Zval)) / Zval
    sprval <- sprval + Nval * waa * maturity
    Nval <- Nval * exp(-Zval)
  }
  selx <- fisheryselectivityatage[nages]
  waa <- weightsatage[nages]
  maturity <- maturityatage[nages]
  Nval <- Nval / (1 - exp(-(Fval * selx + M[nages])))
  yprval <- yprval + Nval * waa * Fval * selx * (1 - exp(-Zval)) / Zval
  sprval <- sprval + Nval * waa * maturity
  F.row <- data.frame(Fval = Fval,
                      ypr = yprval,
                      spr = sprval)
  F.table <- rbind(F.table, F.row)
}
#	F.table
spr0 <- filter(F.table, Fval == 0)$spr 
select.row<-which.min(abs(F.table$spr-spr0*ref_percentage))
F.table[select.row,]	#	check: likely just output the fishing mortality, Fval
}	#	end function

SPR_func(y)


