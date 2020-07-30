# design_matrix.R
# creates all combinations of scenarios for IBMWG
# also describes one-offs

library(dplyr)

# set working directory to source file location to begin

# set up the factors
stocktype <- c("groundfish-ish")

retrosource <- c("Missing Catch", 
                 "Increased M", 
                 "Changed Survey q", 
                 "Selectivity Changes") 

fhistory <- c("always overfishing", 
              "overfishing then fmsy")

fisheryselectivity <- c("Constant", 
                        "Time-varying")

autocorrelatedrecruitment <- TRUE

combinedsurveys <- c("Spring and Previous Fall")

ibms <- c("Islope", 
          "Itarget", 
          "true_Skate_CR", 
          "M_CC", 
          "PlanBsmooth", 
          "ExpandSurvey_modified_low", 
          "ExpandSurvey_modified_high", 
          "AIM", 
          "CatchCurve_Fspr",
          "CatchCurve_stableperiod",
          "JoeDLM",
          "Ensemble")

catchmultiplier <- c(1.00, 
                     0.75)

capchange <- c("none")

scenarios <- expand.grid(stocktype, retrosource, fhistory, fisheryselectivity, autocorrelatedrecruitment, combinedsurveys, ibms, catchmultiplier, capchange) %>%
  rename(StockType = Var1,
         RetroSource = Var2,
         Fhistory = Var3,
         FisherySelectivity = Var4,
         AutoCorrR = Var5,
         CombinedSurveys = Var6,
         IBM = Var7,
         CatchMultiplier = Var8,
         CapChange = Var9) %>%
  mutate(factorial = TRUE)
dim(scenarios)
head(scenarios)

oneoffs <- c("StockType = Pelagic-ish",
             "RetroSource = none",
             "RetroSource = Multiple Factors (TBD)",
             "RetroSource = big cohort",
             "Fhistory = always underfished",
             "Fhistory = under then overfishing",
             "FisherySelectivity = domed",
             "All selectivities domed",
             "AutoCorrR = FALSE",
             "CombinedSurveys = Spring and Same Fall",
             "IBM = FSD",
             "IBM = age-based method",
             "capchange = plus/minus 20%",
             "Change in Weights at Age",
             "Use wrong M when M changes",
             "Use wrong survey q when survey q changes",
             "Reference point calculations")
length(oneoffs)
