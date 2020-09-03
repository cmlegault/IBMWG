IBM.options<-function(y=NULL){
  
  #	additional components needed in the list for each index based method, components can be changed here
  y$F_SPR<-run.spr(y)  #F %spr just in case using spr method; The desired %spr is specified via WHAM y$percentSPR
  
  #	Islope
  y$Islope_yrsmth<-5
  y$Islope_version<-1
  y$Islope_cap<-FALSE
  
  #	Itarget	
  y$Itarget_ref_yrs<-50	#	NEEDS TO BE DETERMINED
  y$Itarget_version<-1
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
  
  #	Created a modified expanded survey function that accounts for two surveys.  
  #		It changes two things
  #	1. adds a scaler to modify the true q, we may want to add some variability to the scaler
  #	2. takes in the two surveys scales them by a q*scaler for each season
  #	2b. combines the expanded survey biomass for the two seasons into one 
  #		Currently happens in the expand function as a place holder and needs to be corrected
  #  y$q is an object from WHAM and has two values.  In Jon's original function simply selected the spring q
  y$expand_method=4
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
  y$AIM_npts<-1		#	number of years to include, default is one



#	Joe Langan's dynamic linear model function
y$JoeDLM_prop_inc<-0.05	#	proportion to increase biomass each year
y$JoeDLM_n_ahead<-2 # forecasting 2 years and returning 2 years of catch advice

  return(y)
}
