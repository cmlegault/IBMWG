# design_matrix.R
# creates all combinations of scenarios for IBMWG
# also describes one-offs

library(dplyr)

# set up the factors
stocktype <- c("groundfish-ish")

retrosource <- c("Missing Catch", 
                 "Increased M") 

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
          "ExpandSurvey_recentavgrelF", 
          "ExpandSurvey_F%SPR",
          "ExpandSurvey_FequalM",
          "ExpandSurvey_stableperiodF",
          "AIM", 
          "CatchCurve_F%SPR",
          "CatchCurve_FequalM",
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
write.csv(scenarios, file = "settings/scenarios.csv", row.names = FALSE)

oneoffs <- c("StockType = Pelagic-ish",
             "RetroSource = none",
             # for the following 2 one offs, 
             # only if able to achieve Mohn's rho of 0.5 for SSB
             "RetroSource = Changed Survey q",  # important due to Bigelow
             "RetroSource = Selectivity Changes",
             "RetroSource = Multiple Factors (TBD)",
             "RetroSource = big cohort",
             "Fhistory = always underfished",
             "Fhistory = under then overfishing",
             "FisherySelectivity = domed",
             "All selectivities domed",
             "AutoCorrR = FALSE",
             "CombinedSurveys = Spring and Same Fall",
             "IBM = FSD",
             "IBM = ExpandSurvey_lowq",
             "IBM = ExpandSurvey_highq",
             "IBM = age-based method",
             "IBM = Islope low option",
             "IBM = Islope high option",
             "IBM = Itarget low option",
             "IBM = Itarget high option",
             "capchange = plus/minus 20%",
             "Change in Weights at Age",
             "Use wrong M when M changes",
             "Use wrong survey q when survey q changes",
             "Reference point calculations",
             "Missing year(s) of survey data")
length(oneoffs)
