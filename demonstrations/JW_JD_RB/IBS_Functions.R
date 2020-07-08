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

#	should create single index that is consistent for all index based methods
seasonal_index<-rbind(y$agg_indices, y$agg_indices_proj)	#	seasonal indices as two columns.  rbind to combine base period with feedback period
#	HOW TO COMBINE SEASONAL INDICES
y$index<-?

#	should create single catch that is consistent for all index based methods
y$catch<-rbind(y$agg_catch, y$agg_catch_proj)	#assuming one fleet

#	creating abundance at age from two survey indices in simple base R
survey_one_base<-y$agg_indices[,1]*y$index_paa[,,1]	#	base period and projections are two different objects within the list, index_paa is #D array (assuming row=yrs, col=ages, third dimension = two surveys)
survey_two_base<-y$agg_indices[,2]*y$index_paa[,,2]

survey_one_proj<-y$agg_indices_proj[,1]*y$index_paa_proj[,,1]
survey_two_proj<-y$agg_indices_proj[,2]*y$index_paa_proj[,,2]

caa_survey_one<-rbind(survey_one_base,survey_one_proj)
caa_survey_two<-rbind(survey_two_base,survey_two_proj)
#	HOW TO COMBINE INDEX AT AGE FOR TWO DIFFERENT SEASONS
y$CAA<?

#	should create single natural mortality that is consistent for all index based methods

y$M<-?	#y$MAA[nrow(y$MAA),]*	#	natural mortality, possiblty M weighted by biomass at age of fully selected ages


#	additional components needed in the list for each index based method, components can be changed here

#	to make it functional created y list with fluke data
y<-list(index=Itot, catch=Ctot, CAA=CAA,M=0.3, Year=yearseq)

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
 

CAA<-	y$CAA
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
caa<-y$CAA
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

