# catch in wt (mt) for summer flounder 1982-2014
Ctot = c(21408,	30425,	30470,	26264,	24837,	23577,	24598,	11231,	9028,	12914,	15985,	13347,	15016,	
         12482,	14734,	13867,	17079,	15138,	20146,	15695,	16473,	19224,	20958,	19293,	17868,	14773,	
         12909,	14307,	15408,	17487,	16163,	17483,	15275)


#index avg. spring / fall kg / tow for summer flounder 1982-2014
Itot <- c(1.005, 0.5, 0.515,  1.035, 0.635,  0.33, 0.395,  0.16, 0.23, 0.26,0.475, 0.26,0.405, 0.645, 0.56, 0.765, 1.17, 
  1.335, 1.76, 1.855, 1.845, 2.175, 2.745, 1.71, 1.565, 2.81, 1.5, 1.9, 1.471, 2.147, 1.703, 1.5415, 1.693)

years <- length(Ctot)
yearseq<-seq(1982,2014,1)
# numerical catch at age for summer flounder 1982-2014
CAA <- matrix(data = c(
5344,	19423,	10149,	935,	328,	117,	66,	26,
4925,	28441,	10911,	2181,	693,	323,	16,	36,
4802,	26582,	15454,	3180,	829,	94,	5, 5,
2078,	14623,	17979,	1767,	496,	252,	30,	5,
1943,	17141,	11056,	3783,	316,	140,	58,	8,
1138,	17214,	10840,	1649,	544,	25,	29,	27,
789,	20440,	14528,	2138,	642,	121,	19,	15,
1080,	4213,	7754,	1713,	357,	55,	9,	3,
1458,	8497,	2217,	1011,	221,	31,	7,	2,
449,	9382,	7162,	742,	217,	32,	3,	1,
3043,	15085,	6507,	1143,	151,	69,	2,	1,
952,	11924,	6118,	585,	74,	46,	19,	2,
1922,	12503,	7697,	968,	209,	28,	13,	0,
2119,	5914,	7563,	1245,	401,	78,	5,	1,
281,	7286,	9889,	1914,	481,	94,	18,	3,
66,	2669,	8519,	3305,	592,	172,	11,	4,
101,	2346,	6667,	5333,	1035,	158,	31,	3,
189,	2255,	6440,	4206,	1228,	358,	55,	11,
13,	1674,	8741,	4895,	1598,	382,	83,	19,
38,	3109,	4826,	3690,	1255,	356,	118,	28,
176,	1934,	5773,	3924,	1317,	316,	144,	18,
56,	2142,	5415,	4206,	1631,	588,	250,	74,
130,	1238,	6356,	5023,	2046,	840,	346,	130,
273,	2070,	4234,	4454,	2409,	1186,	591,	304,
164,	1127,	5705,	3465,	1948,	950,	435,	149,
125,	1040,	2392,	4833,	1902,	810,	386,	154,
159,	1170,	1497,	1992,	2734,	1143,	515,	219,
236,	1272,	2071,	2611,	2237,	1455,	468,	183,
161,	1401,	2224,	2989,	2682,	1232,	611,	213,
112,	720,	2045,	3464,	3328,	1674,	638,	359,
111,	522,	1916,	3539,	2733,	1264,	573,	304,
76,	322,	1219,	3030,	3288,	1196,	499,	211,
136,	705,	1692,	2226,	2768,	1575,	443,	178),nrow = years,ncol=8,byrow=TRUE)

#-----------------------------------------

#	output of WHAM model is a large list named 'y'
#	this section should take the components of y and create a single index, a single catch, single catch at age, etc... 
#	so that the same inputs go into the different index based methods and changes can be made once, here.

load("ywham.RData")

#	should create single index that is consistent for all index based methods
	#	combining base period and projection period into single object
y$seasonal_index<-rbind(y$agg_indices, y$agg_indices_proj)	#	seasonal indices as two columns.  rbind to combine base period with feedback period
#	check: HOW TO COMBINE SEASONAL INDICES ???
#		as a place holder, took mean of fall 2019 and spring 2020
	temp.index<-data.frame(spr=c(y$seasonal_index[,1],0),fall=c(0,y$seasonal_index[,2]))
y$index<-rowMeans(temp.index[1:(nrow(temp.index)-1),])	#	check:Should this start at row 2?,  I had it start at row one so that the number of years stays consistent with catch even though year one only has one survey, not two with this particular method


#	should create single catch that is consistent for all index based methods
y$catch<-rbind(y$agg_catch, y$agg_catch_proj)	#assuming one fleet
#	y$catch<-y$catch[,1]	#	looks like two fleets mistakenly got coded.  Simply selecting the first one


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

#	HOW TO COMBINE INDEX AT AGE FOR TWO DIFFERENT SEASONS
#	check: as a place holder to test functions simply used the spring
y$index_naa<-y$index_naa[,,1]


#	check: should create single natural mortality that is consistent for all index based methods

y$M<-?	#y$MAA[nrow(y$MAA),]*	#	natural mortality, possiblty mean M of mature individuals, M weighted by biomass at age of fully selected ages


#	additional components needed in the list for each index based method, components can be changed here

#	to make it functional created y list with fluke data # replaced by WHAM output 20200715
#	y<-list(index=Itot, catch=Ctot, CAA=CAA,M=0.3, Year=yearseq)

#	Islope
y$Islope_yrsmth<-5
y$Islope_version<-1
y$Islope_cap<-FALSE

#	Skate_CR	#	modification created by John W.
y$Skate_CR_ref_yrs<-25	#	NEEDS TO BE DETERMINED
y$Skate_CR_yrsmth<-3
y$Skate_CR_cmult<-1
y$Skate_CR_w<-0.5

#	true_Skate_CR	#	actual skate control rule used by NEFSC
y$true_Skate_CR_yrsmth<-3
y$true_Skate_CR_assess_span<-2	#	This is the frequency of assessments and should line up with the plan for the simulations
y$true_Skate_CR_percent<-0.75		#	proportion of output assuming it is an ABC that should be the ACT, needs modification

#	DLM_Z <-function(CAA,yrs=3) #	catch curve
y$DLM_Z_yrs<-3


#	M_CC	# Simple method that used catch curve to estimate Z, and assumes Fmsy = assumed M
y$M_CC_yrs<-3
y$M_CC_Fmin<-0.05


#	check

#	Created a modified expanded survey function that accounts for two surveys.  
#		It changes two things
#	1. adds a scaler to modify the true q, we may want to add some variability to the scaler
#	2. takes in the two surveys scales them by a q*scaler for each season
#	2b. combines the expanded survey biomass for the two seasons into one 
#		Currently happens in the expand function as a place holder and needs to be corrected
#  y$q is an object from WHAM and has two values.  In Jon's original function simply selected the spring q
y$expand_method=1 #1 to use average of recent exploitation rates for catch advice; nothing else implemented yet
y$expand_yrs=3 #if method=1 then number of years to average for exploitation rates.
y$expand_q_scaler<-0.5	#	proportion to scale true q can be greater than or less than one.  May want to be a bit more stocastic than simply a proportion of true (in the real world, q varies with time and space, but in the model it is constant, to be realistic may want a little variability)


#for AIM
y$AIM_fscalar = 1
y$AIM_I_smooth=5
y$AIM_F_smooth=3
y$AIM_center=T
y$AIM_Fscalar<-1
y$AIM_plot=F

##For JJD spr	
y$nages=8
y$mat.age=c(0,0.1,1,1,1,1,1,1)
y$M.age=rep(0.2,y$nages)
y$sel.age=c(0,0.1,0.2,0.5,1,1,1,1)
y$spawn.time=1

#-----------------------------------------


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

Skate_CR <- function(y)
{
  # this is a hybrid of the skate CR ref points and the I target control rule 
  # ref_yrs is the number of years for the reference period - this should stay fixed throughout sim so we don't have shifting ref pts. 
  # Cmult determines the target catch (relative to the avg.)
  # w is a parameter (0-1) that determines how quickly catch declines between I.target and I.threshold 
  
index<-y$index
catch<-y$catch
ref_yrs<-y$Skate_CR_ref_yrs
yrsmth<-y$Skate_CR_yrsmth
cmult<-y$Skate_CR_cmult
w<-y$Skate_CR_w


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

Skate_CR(y)

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
  # caa = numerical catch at age in matrix form (rows=years,cols = ages)
  # yrs = how many years of data to use
 

CAA<-	y$index_naa		#	check, make this change
yrs<-y$DLM_Z_yrs

    ny <- nrow(CAA) # total number of years in CAA matrix
    use.rows<-(ny-yrs+1):ny
    Csum <- apply(CAA[use.rows, ], 2, sum,na.rm=TRUE) # sum up by column
    
    maxage <- length(Csum) # max. age
    AFS <- which.max(Csum)  # age at full selection
    AFS[AFS > (maxage - 3)] <- maxage - 3 # need at least 3 ages to do the regression
    
    y <- log(Csum[AFS:maxage]/sum(Csum[AFS:maxage], na.rm = T))
    xc <- 1:length(y)
    y[y == "-Inf"] <- NA
    mod <<- lm(y ~ xc)
    #print(summary(mod))
    chk <- sum(is.na(coef(mod)))
    if(chk) 
    {
       return(NA)
     }
    else 
    {
       coefs <- summary(mod, weights = Csum[AFS:maxage])$coefficients[2,1:2]
       #print(coefs)
       coefs[is.nan(coefs)] <- NA
       names(coefs) <- NULL
       return(-coefs[1])
    }
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
CAA<-	y$index_naa
M<-y$M
yrs<-y$M_CC_yrs
Fmin<-y$M_CC_Fmin

    cyrs <- (length(catch) - (yrs - 1)):length(catch)
    c.use <- catch[cyrs]
    mu.C <- mean(c.use,na.rm=TRUE) # avg. catch
    Z.est <- DLM_Z(y)
    F.est <- Z.est - M
    F.est[is.na(F.est)] <- Fmin  # if you get an NA, replace with Fmin
    F.est[F.est < Fmin] <- Fmin  # if you get a value below Fmin, use Fmin
    
    Ac <- mu.C/(1 - exp(-F.est)) # approx biomass based on catch and estimate of F
    FMSY <- M
    C.targ<- FMSY * Ac
    return(C.targ)
}

# assuming M = 0.25
y$M<-0.25
M_CC(y)

# changing M = 0.15
y$M<-0.15
M_CC(y)


####plan B smooth
planBsmoothfxn<-function(y){
  devtools::install_github("cmlegault/PlanBsmooth")
  library(PlanBsmooth)
  library(ggplot2)
  library(dplyr)
  library(tidyr) 
  planBsmooth<-ApplyPlanBsmooth(data.frame("Year"=y$Year,"avg"=y$index))
  return(list("multiplier"=planBsmooth$multiplier,"planBsmoothall"=planBsmooth))
}

planB<-planBsmoothfxn(y=y)

####Expanded survey biomass	#	Jon D. original
ExpandSurvey<-function(y){
  expanded=y$index/y$q[1]		#	check:Place holder just to make function run
  if(y$expand_method==1){
    exploit=y$catch/expanded
    #ifelse(exploit>1,print("Warning: Exploitation > 1"),print("Exploit OK, <1"))
    meanexploit<-mean(exploit[(length(exploit)-(y$expand_yrs-1)):length(exploit)])
    catch.advice=expanded[length(expanded)]*meanexploit
  } else { print("expanded survey F method undefined") }
  
  return(catch.advice)
}
ExpandSurveyAdvice<-ExpandSurvey(y=y)

#------------------------
####Expanded survey biomass		#	Bell modification
ExpandSurvey_modified<-function(y){
	#	modified from above
	#	added a scaler to modify true q
	#	expanded each survey index individually and then combined them
	#	check:PLACE HOLDER ONLY, NEED TO DETERMINE HOW TO COMBINE SURVEYS	
  #	expanded=y$index/y$q
expanded_one<-y$seasonal_index[,1]/(y$q[1]*y$expand_q_scaler)
expanded_two<-y$seasonal_index[,1]/(y$q[2]*y$expand_q_scaler)
temp.calc<-data.frame(spr=c(expanded_one,0),fall=c(0,expanded_two))
expanded<-rowMeans(temp.calc)[1:nrow(y$seasonal_index)]	#	assumes spring survey is available in current year
  if(y$expand_method==1){
    exploit=y$catch/expanded
    #ifelse(exploit>1,print("Warning: Exploitation > 1"),print("Exploit OK, <1"))
    meanexploit<-mean(exploit[(length(exploit)-(y$expand_yrs-1)):length(exploit)])
    catch.advice=expanded[length(expanded)]*meanexploit
  } else { print("expanded survey F method undefined") }
  
  return(catch.advice)
}
ExpandSurveyAdvice<-ExpandSurvey_modified(y=y)


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

aim<-run.aim(y)

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


#----------------------------------
###JJD SPR from ASAPPlots
#-------Spawners per recruit -----------------------------
s.per.recr<-function(y) {
  #	nages,mat.age,M.age, F.mult, sel.age, spawn.time 
nages<-ncol(y$mature)
mat.age<-y$mature[nrow(y$mature),]	#	check: assumes using the most recent year
M.age<-y$MAA[nrow(y$MAA),]		#	check: assumes we know the true M and it is not mis-specified
F.mult<-y$spr_F_mult
#	check: which selectivity (final year?) and should we know the true value or with obs error.  This is the true value in the final year
sel.age<-y$selAA[[1]][nrow(y$selAA[[1]]),]	#	number one should be the fishery selectivity
spawn.time<-y$spr_spawn_time

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

spr0<- s.per.recr(nages=y$nages, mat.age=y$mat.age, M.age= y$M.age, F.mult=0, sel.age=y$sel.age, spawn.time=y$spawn.time)
F.start <-0.11  # starting guess for optimization routine to find F_SPR%

  t.spr <- 0.4
  
  spr.f <- function(F.start) {
    abs(s.per.recr(nages=y$nages, mat.age=y$mat.age, M.age= y$M.age, F.mult=F.start, sel.age=y$sel.age, spawn.time=y$spawn.time)/spr0 - t.spr )
  }
  yyy <- nlminb(start=F.start, objective=spr.f, lower=0, upper=3)
  f.spr.vals <- yyy$par
  
 #test<-s.per.recr(nages=y$nages, mat.age=y$mat.age, M.age= y$M.age, F.mult=f.spr.vals, sel.age=y$sel.age, spawn.time=y$spawn.time)
 #test/spr0
###end SPR as done by JJD; adapted from ASAPPlots


