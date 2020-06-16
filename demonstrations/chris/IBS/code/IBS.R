# IBS.R
# Index Based Simulator
# demonstration for IBMWG

# if needed
# library(devtools)
# devtools::install_github("cmlegault/ASAPplots")
# devtools::install_github("cmlegault/rose")

library(ggplot2)
library(dplyr)
library(ASAPplots)
library(rose)

# get initial data and settings
# base period 1971-2020, feedback period 2021-2060
startyear <- 1971
nbaseyears <- 50
nfeedbackyears <- 40
M <- 0.2
plusgroupage <- 10
weightsatage <- c(0.15, 0.5, 0.9, 1.4, 2.0, 2.6, 3.2, 4.1, 5.9, 9.0)
maturityatage <- c(0.04, 0.25, 0.60, 0.77, 0.85, 0.92, 1, 1, 1, 1)
fisheryselectivityatage <- c(0.05, 0.20, 0.38, 0.60, 0.79, 0.88, 0.92, 1, 1, 1)
yieldobserror <- 0.1
survey1fracyear <- 0.25
survey2fracyear <- 0.75
survey1selectivityatage <- c(0.1, 0.8, 1, 1, 1, 1, 1, 1, 1, 1)
survey2selectivityatage <- c(0.25, 1, 1, 1, 1, 1, 1, 1, 1, 1)
survey1catchability <- 0.0002
survey2catchability <- 0.0001
survey1obserror <- 0.3
survey2obserror <- 0.4
SR_R0 <- 10e6
SR_steepness <- 0.75
sigmaR <- 0.5
Fmultbase <- c(seq(0.1, 0.7, length.out = 25), seq(0.67, 0.25, length.out = 15), rep(0.25, 10))
#cbind(seq(1971,2020), Fmultbase)
rseedbaseperiod <- 3141593
rseedfull <- 2718282
nbaserealizations <- 10
catch_report_start_year <- 2005
catch_report_frac <- 0.25 # values less than 1 mean under-reporting of catch

# create full matrices
nyears <- nbaseyears + nfeedbackyears
nages <- plusgroupage
MAA <- matrix(M, nrow = nyears, ncol = nages)
NAA <- matrix(NA, nrow = nyears, ncol = nages)
SSB <- rep(NA, nyears)
WAA <- outer(rep(1, nyears), weightsatage)
matAA <- outer(rep(1, nyears), maturityatage)
selAA <- outer(rep(1, nyears), fisheryselectivityatage)
FAA <- matrix(NA, nrow = nyears, ncol = nages)
FAA[1:nbaseyears, ] <- outer(Fmultbase, fisheryselectivityatage)
ZAA <- MAA + FAA
SAA <- exp(-ZAA)
CAA <- matrix(NA, nrow = nyears, ncol = nages)
Yield <- rep(NA, nyears)
survey1B <- rep(NA, nyears)
survey2B <- rep(NA, nyears)
survey1CAA <- matrix(NA, nrow = nyears, ncol = nages)
survey2CAA <- matrix(NA, nrow = nyears, ncol = nages)
survey1sel <- outer(rep(1, nyears), survey1selectivityatage)
survey2sel <- outer(rep(1, nyears), survey2selectivityatage)
c_report_frac <- rep(1, nyears)
c_report_frac[(catch_report_start_year - startyear + 1):nyears] <- catch_report_frac

# calculate F.table for use in MSY reference points
nsteps <- 2001
F.table <- data.frame(Fval = double(),
                      ypr = double(),
                      spr = double()  )
for (istep in 1:nsteps){
  Fval <- (istep - 1)/1000
  yprval <- 0.0
  sprval <- 0.0
  Nval <- 1.0
  for (i in 1:nages){
    selx <- fisheryselectivityatage[i]
    waa <- weightsatage[i]
    maturity <- maturityatage[i]
    Zval <- Fval * selx  + M
    yprval <- yprval + Nval * waa * Fval * selx * (1 - exp(-Zval)) / Zval
    sprval <- sprval + Nval * waa * maturity
    Nval <- Nval * exp(-Zval)
  }
  selx <- fisheryselectivityatage[nages]
  waa <- weightsatage[nages]
  maturity <- maturityatage[nages]
  Nval <- Nval / (1 - exp(-(Fval * selx + M)))
  yprval <- yprval + Nval * waa * Fval * selx * (1 - exp(-Zval)) / Zval
  sprval <- sprval + Nval * waa * maturity
  F.row <- data.frame(Fval = Fval,
                      ypr = yprval,
                      spr = sprval)
  F.table <- rbind(F.table, F.row)
}
F.table
spr0 <- filter(F.table, Fval == 0)$spr 

# compute SR curve parameters
sr_alpha <- 4 * SR_steepness * SR_R0 / (5 * SR_steepness - 1)
sr_beta  <- SR_R0 * spr0 * (1 - SR_steepness) / (5 * SR_steepness - 1)

# calculate true reference points
F.table.1 <- mutate(F.table, pcSPR = 100 * spr / spr0,
                    SSB = sr_alpha * spr - sr_beta)
F.table.2 <- mutate(F.table.1, Rval = sr_alpha * SSB / (sr_beta + SSB))
F.table.full <- mutate(F.table.2, Yield = ypr * Rval)
ref.pts <- filter(F.table.full, Yield == max(Yield))

# ensure all equilibrium values are positive for starting conditions
F.table.full$SSB <- ifelse(F.table.full$SSB < 0, 1e-8, F.table.full$SSB)
F.table.full$Rval <- ifelse(F.table.full$Rval < 0, 1e-8, F.table.full$Rval)
F.table.full$Yield <- ifelse(F.table.full$Yield < 0, 1e-8, F.table.full$Yield)

# generate random values to be used for base period testing
set.seed(rseedbaseperiod)
N1_devs <- matrix(rnorm(nages * nbaserealizations), nrow = nbaserealizations, ncol = nages)
R_devs <- matrix(rnorm(nbaseyears * nbaserealizations), nrow = nbaserealizations, ncol = nbaseyears)
Y_devs <- matrix(rnorm(nbaseyears * nbaserealizations), nrow = nbaserealizations, ncol = nbaseyears)
S1_devs <- matrix(rnorm(nbaseyears * nbaserealizations), nrow = nbaserealizations, ncol = nbaseyears)
S2_devs <- matrix(rnorm(nbaseyears * nbaserealizations), nrow = nbaserealizations, ncol = nbaseyears)

# generate many base periods to examine distributions
saveres <- list()
for (ibrlz in 1:nbaserealizations){

  # fill NAA matrix for base period
  N1 <- rep(1, nages)
  for (i in 2:nages){
    N1[i] <- N1[i-1] * SAA[1,i-1]
  }
  N1[nages] <- N1[nages] / (1 - SAA[1,nages])
  
  R1 <- filter(F.table.full, Fval == Fmultbase[1])
  NAA[1,] <- R1$Rval * N1 * exp(N1_devs[ibrlz, ] * sigmaR - 0.5 * sigmaR * sigmaR)
  NAA[1,nages] <- R1$Rval * N1[nages] # no rec dev applied to plus group 
  SSB[1] <- sum(NAA[1, ] * matAA[1, ] * WAA[1, ])
  
  for (j in 2:nbaseyears){
    pred_R <- sr_alpha * SSB[j-1] / (sr_beta + SSB[j-1])
    NAA[j, 1] <- pred_R * exp(R_devs[ibrlz, j] * sigmaR - 0.5 * sigmaR * sigmaR)
    for (i in 2:nages){
      NAA[j,i] <- NAA[j-1,i-1] * SAA[j-1,i-1]
    }
    NAA[j,nages] <- NAA[j,nages] + NAA[j-1,nages] * SAA[j-1,nages]
    SSB[j] <- sum(NAA[j,] * maturity * WAA)
  }
  
  # calculate catch in numbers and yield in weight
  CAA <- NAA * FAA * (1 - SAA) / ZAA
  YAA <- CAA * WAA
  Yield[1:nbaseyears] <- apply(YAA[1:nbaseyears, ], 1, sum)* exp(Y_devs[ibrlz, ] * yieldobserror - 0.5 * yieldobserror * yieldobserror) * c_report_frac[1:nbaseyears] * 0.001 # last term to convert to metric tons
  
  # make the surveys
  survey1CAA <- survey1sel[1:nbaseyears, ] * NAA[1:nbaseyears, ] * exp(-ZAA[1:nbaseyears, ] * survey1fracyear) * survey1catchability
  survey2CAA <- survey2sel[1:nbaseyears, ] * NAA[1:nbaseyears, ] * exp(-ZAA[1:nbaseyears, ] * survey2fracyear) * survey2catchability 
  survey1B[1:nbaseyears] <-  apply(survey1CAA * WAA[1:nbaseyears, ], 1, sum) * exp(S1_devs[ibrlz, ] * survey1obserror - 0.5 * survey1obserror * survey1obserror)
  survey2B[1:nbaseyears] <-  apply(survey1CAA * WAA[1:nbaseyears, ], 1, sum) * exp(S2_devs[ibrlz, ] * survey2obserror - 0.5 * survey2obserror * survey2obserror)
  
  # save the base period information for this realization
  saveres[[ibrlz]] <- list(CAA=CAA, Yield=Yield, survey1CAA=survey1CAA, survey1B=survey1B, survey2CAA=survey2CAA, survey2B=survey2B)
}

# optionally create ASAP input files from base period to check retros
run_ASAPs <- TRUE
if (run_ASAPs == TRUE){
  orig.dir <- getwd()
  setwd("..\\ASAPruns")
  n.peels <- 5
  terminal.year <- startyear + nbaseyears - 1
  retro.first.year <- terminal.year - n.peels

  caaESS <- 200
  survey1ESS <- 200
  survey2ESS <- 200
  
  initial.dat <- ASAPplots::ReadASAP3DatFile("initial_file.dat")
  print("Beginning ASAP runs for base period...")
  
  for (ibrlz in 1:nbaserealizations){
    asap.dat <- initial.dat
    
    truecaa <- saveres[[ibrlz]]$CAA[1:nbaseyears, 1:nages]
    caaprops <- sweep(truecaa, 1, rowSums(truecaa), FUN="/")
    caaobs <- caaprops
    for (i in 1:nbaseyears){
      caaobs[i, ] <- rmultinom(1, caaESS, caaprops[i, ])
    }
    ctotwt <- saveres[[ibrlz]]$Yield[1:nbaseyears]
    asap.dat$dat$CAA_mats[[1]] <-  cbind(caaobs, ctotwt)
    asap.dat$dat$catch_Neff <- initial.dat$dat$catch_Neff + caaESS
    
    truesurvey1caa <- saveres[[ibrlz]]$survey1CAA[1:nbaseyears, ]
    survey1props <- sweep(truesurvey1caa, 1, rowSums(truesurvey1caa), FUN="/")
    survey1obs <- survey1props
    for (i in 1:nbaseyears){
      survey1obs[i, ] <- rmultinom(1, survey1ESS, survey1obs[i, ])
    }
    asap.dat$dat$IAA_mats[[1]][, 4:(4+nages-1)] <- survey1obs
    asap.dat$dat$IAA_mats[[1]][, 2] <- saveres[[ibrlz]]$survey1B[1:nbaseyears]
    asap.dat$dat$IAA_mats[[1]][, (4+nages)] <- survey1ESS
    
    truesurvey2caa <- saveres[[ibrlz]]$survey2CAA[1:nbaseyears, ]
    survey2props <- sweep(truesurvey2caa, 1, rowSums(truesurvey2caa), FUN="/")
    survey2obs <- survey2props
    for (i in 1:nbaseyears){
      survey2obs[i, ] <- rmultinom(1, survey2ESS, survey2obs[i, ])
    }
    asap.dat$dat$IAA_mats[[2]][, 4:(4+nages-1)] <- survey2obs
    asap.dat$dat$IAA_mats[[2]][, 2] <- saveres[[ibrlz]]$survey2B[1:nbaseyears]
    asap.dat$dat$IAA_mats[[2]][, (4+nages)] <- survey2ESS
    
    fname <- paste0("run2realization", ibrlz, ".dat")
    header.text <- paste("run 2 realization", ibrlz)
    ASAPplots::WriteASAP3DatFile(fname, asap.dat, header.text)
  }
  
  # run ASAP retrospective anlysis
  Mohnsrhossb <- rep(NA, nbaserealizations)
  for (ibrlz in 1:nbaserealizations){
    fname <- paste0("run2realization", ibrlz, ".dat")
    shell(paste("ASAPRETRO.exe", fname, retro.first.year), intern=TRUE)
    Mohnsrhossb[ibrlz] <- rose::calcSSBrho(fname, n.peels)
    print(paste0("Realization ", ibrlz, ": Mohn's rho for SSB = ", Mohnsrhossb[ibrlz]))
  }
  setwd(orig.dir)
}

## some plots to show what is going on under the hood

# compare runs 1 and 2 (run 1 has no misreporting, run 2 has misreporting)
mydir <- "C:\\Users\\chris.legault\\Documents\\Working\\Index-based Research Track 2020\\IBS\\ASAPruns\\"
Mohnsrhossb1 <- rep(NA, nbaserealizations)
Mohnsrhossb2 <- rep(NA, nbaserealizations)
for (ibrlz in 1:nbaserealizations){
  fname1 <- paste0(mydir, "run1realization", ibrlz, ".dat")
  fname2 <- paste0(mydir, "run2realization", ibrlz, ".dat")
  Mohnsrhossb1[ibrlz] <- rose::calcSSBrho(fname1, n.peels)
  Mohnsrhossb2[ibrlz] <- rose::calcSSBrho(fname2, n.peels)
}
rhodf <- data.frame(scenario = rep(c("No misreporting", "Misreporting"), each = nbaserealizations),
                    variable = "Mohn's Rho for SSB",
                    value = c(Mohnsrhossb1, Mohnsrhossb2))
plot1 <- ggplot(rhodf, aes(x=scenario, y=value)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "blue", linetype=2) +
  ylab("Mohn's Rho for SSB") +
  theme_bw()
print(plot1)
ggsave(filename="..\\plots\\compareMohnRhoSSB.png", plot=plot1)

# compare true catch in weight to reported catch in weight
trueY <- apply(saveres[[nbaserealizations]]$CAA[1:nbaseyears, ] * WAA[1:nbaseyears, ], 1, sum) * 0.001 # last term to convert to metric tons
reportedY <- saveres[[nbaserealizations]]$Yield[1:nbaseyears]
ydf <- data.frame(year = seq(startyear, (startyear + nbaseyears - 1)),
                  variable = "Catch in metric tons",
                  scenario = rep(c("True", "Reported"), each = nbaseyears),
                  value = c(trueY, reportedY))
plot2 <- ggplot(ydf, aes(x=year, y=value, color=scenario)) +
  geom_point() +
  geom_line() +
  ylab("Catch in metric tons") +
  expand_limits(y=0) +
  theme_bw()
print(plot2)
ggsave(filename="..\\plots\\compareReportedTrueCatch.png", plot=plot2)

# examine variability in the 10 realizations for Yield and both surveys
tsdf <- data.frame(realization = integer(),
                   variable = character(),
                   year = integer(),
                   value = double())
for (ibrlz in 1:nbaserealizations){
  thisdf <- data.frame(realization = ibrlz,
                       variable = rep(c("Yield", "Survey1B", "Survey2B"), each = nbaseyears),
                       year = seq(startyear, (startyear + nbaseyears - 1)),
                       value = c(saveres[[ibrlz]]$Yield[1:nbaseyears],
                                 saveres[[ibrlz]]$survey1B[1:nbaseyears],
                                 saveres[[ibrlz]]$survey2B[1:nbaseyears]))
  tsdf <- rbind(tsdf, thisdf)
}
plot3 <- ggplot(filter(tsdf, realization <=3), aes(x=year, y=value, color=as.factor(realization))) +
  geom_point() +
  geom_line() +
  facet_grid(variable~realization) +
  expand_limits(y=0) +
  theme_bw() +
  theme(legend.position = "none")
print(plot3)
ggsave(filename="..\\plots\\compareTimeSeriesRealizations.png", plot=plot3)

# take a look at the retros
retrodf <- data.frame(realization = integer(),
                      variable = character(),
                      peel = integer(),
                      year = integer(),
                      value = double())
for (ibrlz in 1:nbaserealizations){
  for (ipeel in 0:n.peels){
    fname <- paste0(mydir, "run2realization", ibrlz, "_00", ipeel, ".rdat")
    asap <- dget(fname)
    thisdf <- data.frame(realization = ibrlz,
                         variable = "SSB",
                         peel = ipeel,
                         year = asap$parms$styr:asap$parms$endyr,
                         value = asap$SSB)
    retrodf <- rbind(retrodf, thisdf)
  }
}
retrotextdf <- rhodf %>%
  filter(scenario == "Misreporting") %>%
  mutate(realization = as.numeric(row.names(.))) %>%
  mutate(rhotext = as.character(round(value, 2))) %>%
  mutate(year = 1980, value = 100000, peel=0)

plot4 <- ggplot(filter(retrodf, realization <=9), aes(x=year, y=value, group=peel)) +
  geom_line() +
  facet_wrap(~realization) +
  geom_text(data = filter(retrotextdf, realization <=9), aes(x=year, y=value, label=rhotext)) +
  expand_limits(y=0) +
  ylab("SSB") +
  scale_x_continuous(breaks = seq(1970, 2020, 20)) +
  theme_bw()
print(plot4)
ggsave(filename="..\\plots\\SSBretroplots.png", plot=plot4)

## still to do, if go down this path

# select index based approach(es?)

# run many realizations of feedback period (many from each base period?)

# collect summary data from each realization

# create summary tables and plots
