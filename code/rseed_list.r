# create csv file with random number seeds
#
# Liz Brooks
# 2017 June 01
#

rm(list=ls(all=TRUE))

set.seed(799291)
nseeds <- 1000
r.seed.set <- trunc(1e7*runif(n=nseeds),7) + trunc(1e3*runif(n=nseeds),3)
write.table(r.seed.set, file="RNG.seeds.csv", quote=F, row.names=F, col.names=F, sep=",")
