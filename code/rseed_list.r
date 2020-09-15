# create csv file with random number seeds
#
# Liz Brooks
# 2017 June 01
#

# see https://www.tidyverse.org/blog/2017/12/workflow-vs-script/
# rm(list=ls(all=TRUE)) 

set.seed(799291)
nseeds <- 1000
r.seed.set <- trunc(1e7*runif(n=nseeds),7) + trunc(1e3*runif(n=nseeds),3)
write.table(r.seed.set, file="settings/RNG.seeds.csv", quote=F, row.names=F, col.names=F, sep=",")
