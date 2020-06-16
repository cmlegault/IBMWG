# demo_db.R
# demonstrate database approach to storage

# set working directory to source file location to begin

library(dplyr)

# create first data frame
db1 <- data.frame(LinkID = c("Scen1IBM1", "Scen1IBM2", "Scen2IBM1", "Scen2IBM2"),
                  Scenario = rep(c("missing catch-overfished-gfish", 
                                    "increased M-overfished-gfish"), each = 2),
                  IndexBasedMethod = rep(c("PlanBsmooth", "FSD"), 2),
                  ControlRule = "None",
                  Bmsy = 50000,
                  Fmsy = 0.25,
                  MSY = 12500,
                  RNGseed = rep(c(14, 39281), each = 2),
                  procerr = 0.0,
                  obserr = 0.2)
db1

# create some fake data just to see how big the files get
db2 <- data.frame(LinkID = character(),
                  Realization = integer(),
                  Year = integer(),
                  Catch = double(),
                  SSB = double(),
                  Fval = double())

proj.years <- seq(2021, 2060)
npy <- length(proj.years)
nrealz <- 1000
myCatch <- c(10000, 12000, 5000, 6000)
mySSB <- c(20000, 18000, 45000, 4000)
myFval <- c(0.4, 0.45, 0.2, 0.25)

for (i in 1:length(db1$LinkID)){
  LinkID <- db1$LinkID[i]
  for (ir in 1:nrealz){
    Catch <- rep(NA, npy)
    SSB <- rep(NA, npy)
    Fval <- rep(NA, npy)
    Catch[1] <- myCatch[i]
    SSB[1] <- mySSB[i]
    Fval[1] <- myFval[i]
    Crand <- rlnorm(npy, 0, 0.1)
    Srand <- rlnorm(npy, 0, 0.1)
    Frand <- rlnorm(npy, 0, 0.1)
    for (iy in 2:npy){
      Catch[iy] <- Catch[iy-1] * Crand[iy]
      SSB[iy] <- SSB[iy-1] * Srand[iy]
      Fval[iy] <- Fval[iy-1] * Frand[iy]
    }
    thisdb <- data.frame(LinkID = LinkID,
                         Realization = ir,
                         Year = proj.years,
                         Catch = Catch,
                         SSB = SSB,
                         Fval = Fval)
    db2 <- rbind(db2, thisdb)
  }
}
head(db2)
dim(db2)

# uncomment lines below to save both files 
#write.csv(db1, file = "db1.csv", row.names = FALSE)
#write.csv(db2, file = "db2.csv", row.names = FALSE)
# note: db2 creates a large (~11 MB) file

# some example sumamry statistics, could use purrr to speed these up
short.term.years <- seq(2021, 2026)
long.term.years <- seq(2041, 2060)

mean.short.terms <- db2 %>%
  filter(Year %in% short.term.years) %>%
  group_by(LinkID) %>%
  summarize(short.catch = mean(Catch),
            short.SSB = mean(SSB),
            short.Fval = mean(Fval))
mean.short.terms

mean.long.terms <- db2 %>%
  filter(Year %in% long.term.years) %>%
  group_by(LinkID) %>%
  summarize(long.catch = mean(Catch),
            long.SSB = mean(SSB),
            long.Fval = mean(Fval))
mean.long.terms
