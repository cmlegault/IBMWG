# pull_ASAP_data.R
# extract data from ASAP files for IBMWG

# set working directory to source file location to begin

library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

fnames <- list.files(pattern = ".RDAT")
stocks <- substr(fnames, 1, (nchar(fnames) - 5))
nstocks <- length(stocks)

# assign stocks to groups
sgroup <- rep("roundfish", nstocks)
sgroup[stocks %in% c("CCGOM_YT", "FLUKE", "GB_WINTER", "GB_YT", "PLAICE", "SNEMA_WINTER", "SNEMA_YT", "WITCH")] <- "flatfish"
sgroup[stocks %in% c("BUTTERFISH", "HERRING", "MACKEREL", "SCUP")] <- "pelagic"
sgroup[stocks == "REDFISH"] <- "rockfish"
sgroup

# Age of Plus Group
nagesdf <- data.frame(stock = character(),
                      variable = character(),
                      value = integer())
for (i in 1:nstocks){
  asap <- dget(fnames[i])
  thisdf <- data.frame(stock = stocks[i],
                       variable = "nages",
                       value = asap$parms$nages)
  nagesdf <- rbind(nagesdf, thisdf)
}
nagesdf
plot1 <- ggplot(nagesdf, aes(x=value, y=stock)) +
  geom_point() +
  xlab("Age of Plus Group") +
  ylab("") +
  theme_bw()
print(plot1)

# SSB weight at age in terminal year
wtdf <- data.frame(stock = character(),
                   sgroup = character(),
                   variable = character(),
                   age = integer(),
                   value = double())
for (i in 1:nstocks){
  asap <- dget(fnames[i])
  ages <- seq(1, asap$parms$nages)
  waa <- as.vector(asap$WAA.mats$WAA.ssb[asap$parms$nyears, ])
  thisdf <- data.frame(stock = stocks[i],
                       sgroup = sgroup[i],
                       variable = "ssbwaa",
                       age = ages,
                       value = waa)
  wtdf <- rbind(wtdf, thisdf)
}
wtdf
plot2 <- ggplot(wtdf, aes(x=age, y=value, color=stock)) +
  geom_point() +
  geom_line() +
  ylab("SSB Weight at Age") +
  theme_bw()
print(plot2)
plot3 <- plot2 +
  facet_wrap(~sgroup, scales = "free") +
  theme(legend.position = "none") 
print(plot3)

sgroups <- unique(sgroup)
nsgroups <- length(sgroups)
plot4s <- list()
for (i in 1:nsgroups){
  plot4 <- ggplot(filter(wtdf, sgroup == sgroups[i]), aes(x=age, y=value, color=stock)) +
    geom_point() +
    geom_line() +
    ylab("SSB Weight at Age") +
    ggtitle(sgroups[i]) +
    theme_bw()
  print(plot4)
  plot4s[[i]] <- plot4
}

summarywaa <- wtdf %>%
  group_by(sgroup, age) %>%
  summarize(meanwaa = mean(value))
summarywaa
ggplot(summarywaa, aes(x=age, y=meanwaa, color=sgroup)) +
  geom_point() +
  geom_line() +
  theme_bw()

# maturity at age in terminal year
matdf <- data.frame(stock = character(),
                    sgroup = character(),
                    variable = character(),
                    age = integer(),
                    value = double())
for (i in 1:nstocks){
  asap <- dget(fnames[i])
  ages <- seq(1, asap$parms$nages)
  mat <- as.vector(asap$maturity[asap$parms$nyears, ])
  thisdf <- data.frame(stock = stocks[i],
                       sgroup = sgroup[i],
                       variable = "maturity",
                       age = ages,
                       value = mat)
  matdf <- rbind(matdf, thisdf)
}
matdf
plot5 <- ggplot(matdf, aes(x=age, y=value, color=stock)) +
  geom_point() +
  geom_line() +
  ylab("Maturity at Age") +
  theme_bw()
print(plot5)
plot6 <- plot5 +
  facet_wrap(~sgroup, scales = "free") +
  theme(legend.position = "none") 
print(plot6)

plot7s <- list()
for (i in 1:nsgroups){
  plot7 <- ggplot(filter(matdf, sgroup == sgroups[i]), aes(x=age, y=value, color=stock)) +
    geom_point() +
    geom_line() +
    ylab("Maturity at Age") +
    ggtitle(sgroups[i]) +
    theme_bw()
  print(plot7)
  plot7s[[i]] <- plot7
}

summarymat <- matdf %>%
  group_by(sgroup, age) %>%
  summarize(meanmat = mean(value))
summarymat
ggplot(summarymat, aes(x=age, y=meanmat, color=sgroup)) +
  geom_point() +
  geom_line() +
  theme_bw()

# M
mdf <- data.frame(stock = character(),
                  variable = character(),
                  value = double())
for (i in 1:nstocks){
  asap <- dget(fnames[i])
  if (all(asap$M.age == asap$M.age[1,1]) == TRUE){
    myM <- asap$M.age[1,1]
  } else{
    myM <- mean(asap$M.age)
    # fluke M varies by age between 0.26 and 0.24
    # SNEMA YT varies by age between 0.405 and 0.231
    # striped bass varies by age between 1.13 and 0.15
  }
  thisdf <- data.frame(stock=stocks[i],
                       variable = "M",
                       value = myM)
  mdf <- rbind(mdf, thisdf)
}
mdf
plot8 <- ggplot(mdf, aes(x=value, y=stock)) +
  geom_point() +
  xlab("Natural Mortality Rate") +
  ylab("") +
  theme_bw()
print(plot8)

# fishery selectivity
seldf <- data.frame(stock = character(),
                    sgroup = character(),
                    variable = character(),
                    age = integer(),
                    value = double())
for (i in 1:nstocks){
  asap <- dget(fnames[i])
  ages <- seq(1, asap$parms$nages)
  sel <- as.vector(asap$fleet.sel.mats[[1]][asap$parms$nyears, ])
  thisdf <- data.frame(stock = stocks[i],
                       sgroup = sgroup[i],
                       variable = "selectivity",
                       age = ages,
                       value = sel)
  seldf <- rbind(seldf, thisdf)
}
seldf
plot9 <- ggplot(seldf, aes(x=age, y=value, color=stock)) +
  geom_point() +
  geom_line() +
  ylab("Selectivity at Age") +
  theme_bw()
print(plot9)
plot10 <- plot9 +
  facet_wrap(~sgroup, scales = "free") +
  theme(legend.position = "none") 
print(plot10)

plot11s <- list()
for (i in 1:nsgroups){
  plot11 <- ggplot(filter(seldf, sgroup == sgroups[i]), aes(x=age, y=value, color=stock)) +
    geom_point() +
    geom_line() +
    ylab("Selectivity at Age") +
    ggtitle(sgroups[i]) +
    theme_bw()
  print(plot11)
  plot11s[[i]] <- plot11
}

summarysel <- seldf %>%
  group_by(sgroup, age) %>%
  summarize(meansel = mean(value))
summarysel
ggplot(summarysel, aes(x=age, y=meansel, color=sgroup)) +
  geom_point() +
  geom_line() +
  theme_bw()

# relative diff between fishery selectivity and maturity
reldf <- full_join(matdf, seldf, by=c("stock", "sgroup", "age")) %>%
  mutate(diffsm = value.y - value.x)

plot12s <- list()
for (i in 1:nsgroups){
  plot12 <- ggplot(filter(reldf, sgroup == sgroups[i]), aes(x=age, y=diffsm, color=stock)) +
    geom_point() +
    geom_line() +
    ylab("Selectivity - Maturity at Age") +
    ggtitle(sgroups[i]) +
    theme_bw()
  print(plot12)
  plot12s[[i]] <- plot12
}

summarydiff <- reldf %>%
  group_by(sgroup, age) %>%
  summarize(meandiff = mean(diffsm))
summarydiff
ggplot(summarydiff, aes(x=age, y=meandiff, color=sgroup)) +
  geom_point() +
  geom_line() +
  theme_bw()

# roundfish summaries
rsums <- full_join(summarywaa, summarymat, by = c("sgroup", "age")) %>%
  full_join(., summarysel, by = c("sgroup", "age")) %>%
  full_join(., summarydiff, by = c("sgroup", "age")) %>%
  filter(sgroup == "roundfish")
rsums

# warning message
warndf <- data.frame(x = rep(1, 6),
                     y = seq(6, 1, -1),
                     z = c("The following plots are from ASAP rdat files I had handy",
                           "or created from VPA runs or other sources.",
                           "They should not be considered the best available information.",
                           "The plots are provided for comparative purposes only.",
                           "Details about any particular stock should be taken",
                           "from that specific stock assessment."))
warnplot <- ggplot(warndf, aes(x=x, y=y)) +
  geom_text(label = warndf$z) +
  theme_void()
print(warnplot)

# make pdf
pdf(file = "local_stocks_summary.pdf")

print(warnplot)
print(plot1)
print(plot8)
for (i in 1:nsgroups){
  print(plot4s[i])
}
for (i in 1:nsgroups){
  print(plot7s[i])
}
for (i in 1:nsgroups){
  print(plot11s[i])
}
for (i in 1:nsgroups){
  print(plot12s[i])
}

grid.newpage()
grid.table(rsums)

dev.off()

# compute autocorrelation in recruitment
recdf <- data.frame(stock = character(),
                    sgroup = character(),
                    variable = character(),
                    year = integer(),
                    value = double())
for (i in 1:nstocks){
  asap <- dget(fnames[i])
  years <- seq(asap$parms$styr, asap$parms$endyr)
  recruits <- asap$N.age[, 1]
  thisdf <- data.frame(stock = stocks[i],
                       sgroup = sgroup[i],
                       variable = "Recruits",
                       year = years,
                       value = recruits)
  recdf <- rbind(recdf, thisdf)
}
recdf
ggplot(recdf, aes(x=year, y=value, color=stock)) +
  geom_point() +
  geom_line() +
  facet_wrap(~stock, scales = "free") +
  theme_bw() +
  theme(legend.position = "none")

ardf <- recdf %>%
  group_by(stock, sgroup) %>%
  summarize(ar1 = acf(value, lag.max=1, plot=FALSE)$acf[, , 1][2])
ggplot(ardf, aes(x=ar1, y=stock)) +
  geom_point() +
  theme_bw()
mardf <- ardf %>%
  group_by(sgroup) %>%
  summarize(meanar1 = mean(ar1))
rfdf <- filter(ardf, sgroup %in% c("roundfish", "flatfish")) 
mean(rfdf$ar1)
# use 0.4 for autocorrelation from mean of roundfish and flatfish

# examine ESS for fishery and surveys
vpastocks <- c("CCGOM_YT", "GB_HADDOCK", "GB_WINTER", "PLAICE")
essdf <- data.frame(stock = character(),
                    sgroup = character(),
                    variable = character(),
                    gear = character(),
                    value = integer())
for (i in 1:nstocks){
  if (!(stocks[i] %in% vpastocks)){
    asap <- dget(fnames[i])
    ess1 <- as.vector(asap$fleet.catch.Neff.init)
    fisheryess <- mean(ess1[ess1 > 0])
    ess2 <- as.vector(asap$index.Neff.init)
    ess3 <- ifelse(any(ess2 > 0), mean(ess2[ess2 > 0]), 0)
    surveyess <- ess3
    
    if (fisheryess * surveyess > 0){
      thisdf <- data.frame(stock = stocks[i],
                           sgroup = sgroup[i],
                           variable = "ess",
                           gear = c("fishery", "survey"),
                           value = c(fisheryess, surveyess))
      essdf <- rbind(essdf, thisdf)
    }
  }
}
essdf
ggplot(essdf, aes(x=value, y=stock, color=gear)) +
  geom_point() +
  theme_bw()

essdf %>%
  group_by(sgroup, gear) %>%
  summarize(ess = mean(value))
# based on roundfish and flatfish, 
# fishery ess = 50, survey ess = 25 seems reasonable


# get survey CVs (don't worry about NEFSC vs others for now)
cvdf <- data.frame(stock = character(),
                    sgroup = character(),
                    variable = character(),
                    indmonth = integer(),
                    value = integer())
for (i in 1:nstocks){
  if (!(stocks[i] %in% vpastocks)){
    asap <- dget(fnames[i])
    cvlist <- asap$index.cv
    indmonth <- asap$control.parms$index.month
    myflag <- asap$control.parms$index.age.comp.flag
    for (j in 1:length(cvlist)){
      if (myflag[j] == 1){
        myval <- mean(cvlist[[j]])
        if(myval >10) myval <- NA
        thisdf <- data.frame(stock = stocks[i],
                             sgroup = sgroup[i],
                             variable = "cv",
                             indmonth = indmonth[j],
                             value = myval)
        cvdf <- rbind(cvdf, thisdf)
      }
    }
  }
}
cvdf
cvdfsum <- cvdf %>%
  mutate(season = case_when(
    indmonth <= 6.5 ~ "First Half",
    TRUE ~ "Second Half")) %>%
    group_by(stock, season) %>%
  summarise(meanval = mean(value, na.rm = TRUE))
cvdfsum
surveycvplot <- ggplot(cvdfsum, aes(x=meanval, y=stock, color=season)) +
  geom_point() +
  facet_wrap(~season) +
  geom_vline(xintercept = c(0.3, 0.4), linetype="dashed") +
  expand_limits(x = 0) +
  labs(x="Mean Survey CV", y="", title="Includes all surveys") +
  theme_bw() + 
  theme(legend.position = "none")
ggsave(filename = "surveycv.png", surveycvplot)
