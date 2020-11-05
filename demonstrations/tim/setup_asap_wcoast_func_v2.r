#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Function to set-up asap 2.20 "west coast style"
# Liz Brooks
# Version 1.0
# Created 30 September 2010
# Last Modified: 18 September 2013
#                16 November 2017 for ices-wgmg
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

#_________________________________________________________________

"setup.asap.w" <-function(wd, pd, model.id="test", nyears, first.year, asap.nages, nfleets,
        nselblks, n.ind.avail, M.mat, fec.opt, t.spawn, mat.mat, n.waa.mats, waa.array, waa.pointer.vec,
        sel.blks, sel.types, sel.mats, fleet.age1, fleet.age2, F.report.ages, F.report.opt, 
        like.const, rel.mort.fleet, caa.mats, daa.mats, rel.prop, units.ind.agg, units.ind.aa, time.ind, 
        fish.ind, sel.ind, ind.age1, ind.age2, ind.use, ind.sel.mats, ind.mat, p.Fmult1,
        p.Fmult.dev, p.recr.dev, p.N1, p.q1, p.q.dev, p.SR, p.h, recr.CV, lam.ind,
        lam.c.wt, lam.disc, catch.CV, disc.CV, Neff.catch, Neff.disc, lam.Fmult.y1,
        CV.Fmult.y1, lam.Fmult.dev, CV.Fmult.dev, lam.N1.dev, CV.N1.dev, lam.recr.dev, 
        lam.q.y1, CV.q.y1, lam.q.dev, CV.q.dev,lam.h, CV.h, lam.SSB0, CV.SSB0,   
        naa.y1, Fmult.y1, q.y1,SSB0, h.guess, F.max, ignore.guess, 
        do.proj, fleet.dir, proj.yr, proj.specs, 
        do.mcmc, mcmc.nyr.opt, mcmc.nboot, mcmc.thin, mcmc.seed, 
        recr.agepro, recr.start.yr, recr.end.yr, test.val,
        fleet.names, survey.names, disc.flag )    {



#####   Define function variables
  # wd         working directory path (where file will be written)
  # model.id    model identifier
  # nyears
  # first.year
  # asap.nages
  # nfleets
  # nselblks
  # n.ind.avail
  # M.mat
  # fec.opt      0(use WAA*mat.age)  or 1 (use mat.age values)
  # t.spawn      fraction of year elapsed prior to ssb calcs
  # mat.mat
  # c.waa
  # ssb.waa
  # jan1.waa
  # sel.blks   a vertical vector of nselblks*nyears
  # sel.types  vector of length nselblks
  # sel.mats   nselblks X matrix(sel.specs, nrow= nages+6, ncol=4)
  # fleet.age1  starting age for selectivity by fleet
  # fleet.age2  ending age for selectivity by fleet
  # F.report.ages  vector of 2 ages for summarizing F trend
  # F.report.opt  option to report F as unweighted(1), Nweighted(2), Bweighted(3)
  # like.const    flag to use(1) or not(0) likelihood constants
  # rel.mort.fleet  flag for whether there is release mortality by fleet (nfleets entries)
  # caa.mats      nfleets X cbind(matrix(caa, nyears,nages), tot.cat.biomass)
  # daa.mats      nfleets X cbind(matrix(disc.aa, nyears, nages), tot.disc.biomass)
  # rel.prop      nfleets X matrix(release.prop.aa, nyears, nages)
  # units.ind.agg     n.ind.avail vector for units (1=biomass, 2=number)
  # units.ind.aa     n.ind.avail vector for units (1=biomass, 2=number)
  # time.ind      n.ind.avail vector for month index sampled
  # fish.ind      link to fleet (-1 if no link, fleet.number otherwise)
  # sel.ind       functional form for indices (n.ind.avail)
  # ind.age1      first age each index selects (n.ind.avail)
  # ind.age2      last age each index selects (n.ind.avail)
  # ind.use       flag to use(1) or not(0) each index
  # ind.sel.mats  n.ind.avail X matrix(sel.specs, nrow= nages+6, ncol=4)
  #     the 6 additional are: Units, month, sel.link.to.fleet, sel.start.age, sel.end.age, use.ind
  # ind.mat       n.ind.avail X matrix(index.stuff, nyears, ncol=nages+4)
  # p.Fmult1      phase for estimating F mult in 1st year
  # p.Fmult.dev   phase for estimating devs for Fmult
  # p.recr.dev    phase for estimating recruitment deviations
  # p.N1          phase for estimating N in 1st year
  # p.q1          phase for estimating q in 1st year
  # p.q.dev       phase for estimating q deviations
  # p.SR          phase for estimating SR relationship
  # p.h           phase for estimating steepness
  # recr.CV       vertical vector of CV on recruitment per year
  # lam.ind    lambda for each index
  # lam.c.wt      lambda for total catch in weight by fleet
  # lam.disc      lambda for total discards at age by fleet
  # catch.CV      matrix(CV.fleet, nyears, nfleets)
  # disc.CV       matrix(CV.fleet, nyears, nfleets)
  # Neff.catch    input effective sample size for CAA (matrix(Neff, nyears, nfleets)
  # Neff.disc     input effective sample size for disc.AA (matrix(Neff, nyears, nfleets)
  # lam.Fmult.y1  lambda for Fmult in first year by fleet (nfleets)
  # CV.Fmult.y1   CV for Fmult in first year by fleet (nfleets)
  # lam.Fmult.dev  lambda for Fmult devs by fleet (nfleets)
  # CV.Fmult.dev  CV for Fmult deviations by fleet (nfleets)
  # lam.N1.dev     lambda for N in 1st year devs
  # CV.N1.dev     CV for N in 1st year devs
  # lam.recr.dev  lambda for recruitment devs
  # lam.q.y1      lambda for q in 1st yr by index (n.ind.avail)
  # CV.q.y1       CV for q in 1st yr by index (n.ind.avail)
  # lam.q.dev      lambda for q devs (n.ind.avail)
  # CV.q.dev       CV for q devs (n.ind.avail)
  # lam.h          lambda for deviation from initial steepness
  # CV.h           CV for deviation from initial steepness
  # lam.SSB0       lambda for deviation from SSB0
  # CV.SSB0        CV for deviation from SSB0
  # naa.y1          vector(nages) of initial stock size
  # Fmult.y1       initial guess for Fmult in yr1 (nfleets)
  # q.y1           q in 1st year vector(n.ind.avail)
  # SSB0           initial unexploited stock size
  # h.guess        guess for initial steepness
  # F.max          upper bound on Fmult
  # ignore.guess   flag to ignore(1) or not(0) initial guesses
  # do.proj        flag to do(1) or not(0) projections
  # fleet.dir      rep(1,nfleets)
  # proj.yr        (nyears+2)
  # proj.specs     matrix(proj.dummy, nrow=2, ncol=5)
  # do.mcmc        0(no) or 1(yes)
  # mcmc.nyr.opt   0(use.NAA.last.yr), 1(use.NAA.T+1)
  # mcmc.nboot     number of mcmc iterations
  # mcmc.thin      thinning rate for mcmc
  # mcmc.seed      random number seed for mcmc routine
  # recr.agepro     0(use NAA), 1 (use S-R), 2(use geometric mean of previous years)
  # recr.start.yr  starting year for calculation of R
  # recr.end.yr    ending year for calculation of R
  # test.val       -23456   
  # disc.flag       T if discards present, F otherwise         
  

#####   SET-UP ASAP FILE  (Note: to execute it, .dat file must be named asap2.dat)

#_________________________________________________________________

#out.file = paste(wd,"asap.", model.id, ".dat", sep="")
out.file = paste0(wd, model.id)
write('# ASAP VERSION 3.0 setup by my_sim.r', file=out.file, append=F)
write(paste('# MODEL ID ', model.id, sep=''),file=out.file,append=T)
write( '# Number of Years' , file=out.file,append=T)   #, ncol=(nyears))
write(nyears, file=out.file,append=T )
write('# First year', file=out.file,append=T)  #proportion F before spawning
write(first.year, file=out.file,append=T )  #proportion M before spawning
write('# Number of ages', file=out.file,append=T)  #single value for M
write(asap.nages, file=out.file,append=T )  #last year of selectivity
write('# Number of fleets', file=out.file,append=T)   #, ncol=nages)  #last year of maturity
write(nfleets, file=out.file,append=T )  #last year of catch WAA
write('# Number of selectivity blocks', file=out.file,append=T)    #, ncol=nages)  #last year of stock biomass
write(nselblks, file=out.file,append=T )  #number of F grid values
write('# Number of available indices', file=out.file,append=T)  #
write(n.ind.avail, file=out.file,append=T )  #specifies BH or Ricker
write( '# M matrix' , file=out.file,append=T)   #, ncol=(nyears))
write(t(M.mat), file=out.file,append=T, ncol=asap.nages)
write('# Fecundity option', file=out.file,append=T)  #specifies normal or lognormal error
write(fec.opt, file=out.file,append=T)  #
write('# Fraction of year elapsed before SSB calculation', file=out.file,append=T)  #
write(t.spawn , file=out.file,append=T)  #
write( '# MATURITY matrix' , file=out.file,append=T)   #, ncol=(nyears))
write(t(mat.mat), file=out.file,append=T, ncol=asap.nages)
write( '# Number of WAA matrices' , file=out.file,append=T)   #, ncol=(nyears))
write(n.waa.mats, file=out.file,append=T, ncol=asap.nages)
write( '# WAA matrix-1' , file=out.file,append=T)   #, ncol=(nyears))
write(t(waa.array[,,1]), file=out.file,append=T, ncol=asap.nages)
if (n.waa.mats>1)  {
    for (j in 2:n.waa.mat)  {
    write(paste('# WAA matrix-',j, sep=""), file=out.file,append=T, ncol=asap.nages)
    write(t(waa.array[,,j]), file=out.file,append=T, ncol=asap.nages)

    } # end loop over j (for WAA matrices)
}  # end if-test for n.waa.mat
#write('# test', file=out.file,append=T)
write( '# WEIGHT AT AGE POINTERS' , file=out.file,append=T)   #, ncol=(nyears))
write(waa.pointer.vec, file=out.file,append=T, ncol=1)

#write( '# CATCH WEIGHT AT AGE' , file=out.file,append=T)   #, ncol=(nyears))
#write(t(c.waa), file=out.file,append=T, ncol=vpa.plus.age)
#write( '# SSB WEIGHT AT AGE' , file=out.file,append=T)   #, ncol=(nyears))
#write(t(ssb.waa), file=out.file,append=T, ncol=vpa.plus.age)
#write( '#Jan-1 STOCK WEIGHT AT AGE' , file=out.file,append=T)   #, ncol=(nyears))
#write(t(jan1.waa), file=out.file,append=T, ncol=vpa.plus.age)
write( '# Selectivity blocks (blocks within years)' , file=out.file,append=T)   #, ncol=(nyears))
write(sel.blks, file=out.file,append=T, ncol=1)
write( '# Selectivity options for each block' , file=out.file,append=T)   #, ncol=(nyears))
write(t(sel.types), file=out.file,append=T, ncol=nselblks)
write( '# Selectivity Block Matrices' , file=out.file,append=T)   #, ncol=(nyears))
write(t(sel.mats), file=out.file,append=T, ncol=4 )
write( '# Selectivity start age by fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(fleet.age1, file=out.file,append=T, ncol=nfleets )
write( '# Selectivity end age by fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(fleet.age2, file=out.file,append=T, ncol=nfleets )
write( '# Age range for average F' , file=out.file, append=T)   #, ncol=(nyears))
write(F.report.ages, file=out.file,append=T, ncol=2)
write( '# Average F report option ' , file=out.file,append=T)   #, ncol=(nyears))
write(F.report.opt, file=out.file,append=T, ncol=2)
write( '# Use likelihood constants?' , file=out.file,append=T)   #, ncol=(nyears))
write(like.const, file=out.file, append=T )
write( '# Release Mortality by fleet' , file=out.file,append=T)   #, ncol=(nyears))
write( rel.mort.fleet, file=out.file,append=T, ncol=nfleets)
write( '# Catch at age matrices (nyears*nfleets rows)' , file=out.file,append=T)   #, ncol=(nyears))
write(t(caa.mats), file=out.file,append=T, ncol= (asap.nages+1) )
write( '# Discard at age matrices' , file=out.file,append=T)   #, ncol=(nyears))
write(t(daa.mats), file=out.file,append=T, ncol=(asap.nages+1) )
write( '# Release proportion at age (nyears*nfleets rows)' , file=out.file,append=T)   #, ncol=(nyears))
write(t(rel.prop), file=out.file,append=T, ncol=asap.nages )
write( '# Index units' , file=out.file,append=T)   #, ncol=(nyears))
write(units.ind.agg, file=out.file,append=T, ncol=n.ind.avail )
write( '# Index Age comp. units' , file=out.file,append=T)   #, ncol=(nyears))
write(units.ind.aa, file=out.file,append=T, ncol=n.ind.avail )
write( '# Index WAA matrix' , file=out.file,append=T)   #, ncol=(nyears))
write(t(rep(1,n.ind.avail)), file=out.file,append=T, ncol=n.ind.avail )



write( '# Index month' , file=out.file, append=T)   #, ncol=(nyears))
write(time.ind, file=out.file,append=T, ncol=n.ind.avail )
write( '# Index link to fleet? ' , file=out.file,append=T)   #, ncol=(nyears))
write(fish.ind, file=out.file,append=T, ncol=n.ind.avail)
write( '# Index selectivity option ' , file=out.file,append=T)   #, ncol=(nyears))
write(sel.ind, file=out.file,append=T, ncol=n.ind.avail)
write( '# Index start age' , file=out.file,append=T)   #, ncol=(nyears))
write(ind.age1, file=out.file, append=T, ncol=n.ind.avail )
write( '# Index end age' , file=out.file,append=T)   #, ncol=(nyears))
write(ind.age2, file=out.file, append=T, ncol=n.ind.avail )

write( '# Index Estimate Proportion (YES=1)' , file=out.file,append=T)   #, ncol=(nyears))
write(t(rep(1,n.ind.avail)), file=out.file, append=T, ncol=n.ind.avail )


write( '# Use Index' , file=out.file,append=T)   #, ncol=(nyears))
write(ind.use, file=out.file, append=T, ncol=n.ind.avail )
write( '# Index selectivity block matrices' , file=out.file,append=T)   #, ncol=(nyears))
write(t(ind.sel.mats), file=out.file,append=T, ncol=4 )
write( '# Index data matrices (n.ind.avail.*nyears)' , file=out.file,append=T)   #, ncol=(nyears))
write(t(ind.mat), file=out.file,append=T, ncol=(asap.nages+ 4) )
write( '#########################################' , file=out.file,append=T)   #, ncol=(nyears))
write( '# Phase data' , file=out.file,append=T)   #, ncol=(nyears))
write( '# Phase for Fmult in 1st year' , file=out.file,append=T)   #, ncol=(nyears))
write(p.Fmult1, file=out.file,append=T  )
write( '# Phase for Fmult deviations' , file=out.file, append=T)   #, ncol=(nyears))
write(p.Fmult.dev, file=out.file,append=T  )
write( '# Phase for recruitment deviations ' , file=out.file,append=T)   #, ncol=(nyears))
write(p.recr.dev, file=out.file,append=T )
write( '# Phase for N in 1st year ' , file=out.file,append=T)   #, ncol=(nyears))
write(p.N1, file=out.file,append=T )
write( '# Phase for catchability in 1st year' , file=out.file,append=T)   #, ncol=(nyears))
write(p.q1, file=out.file, append=T  )
write( '# Phase for catchability deviations' , file=out.file,append=T)   #, ncol=(nyears))
write(p.q.dev, file=out.file, append=T )
write( '# Phase for stock recruit relationship' , file=out.file,append=T)   #, ncol=(nyears))
write(p.SR, file=out.file, append=T  )
write( '# Phase for steepness' , file=out.file,append=T)   #, ncol=(nyears))
write(p.h, file=out.file,append=T  )
write( '#########################################' , file=out.file,append=T)   #, ncol=(nyears))
write( '# Lambdas and CVs' , file=out.file,append=T)   #, ncol=(nyears))
write( '# Recruitment CV by year' , file=out.file,append=T)   #, ncol=(nyears))
write(recr.CV, file=out.file,append=T , ncol=1 )
write( '# Lambda for each index' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.ind, file=out.file,append=T, ncol=n.ind.avail  )
write( '# Lambda for Total catch in weight by fleet' , file=out.file, append=T)   #, ncol=(nyears))
write(lam.c.wt, file=out.file,append=T, ncol=nfleets  )
write( '# Lambda for total discards at age by fleet ' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.disc, file=out.file,append=T, ncol=nfleets )
write( '# Catch Total CV by year and fleet ' , file=out.file,append=T)   #, ncol=(nyears))
write(catch.CV, file=out.file,append=T, ncol=nfleets )
write( '# Discard total CV by year and fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(disc.CV, file=out.file, append=T, ncol=nfleets  )
write( '# Input effective sample size for catch at age by year and fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(Neff.catch, file=out.file, append=T, ncol=nfleets )
write( '# Input effective sample size for discards at age by year and fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(Neff.disc, file=out.file, append=T , ncol=nfleets )
write( '# Lambda for Fmult in first year by fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.Fmult.y1, file=out.file,append=T, ncol=nfleets  )
write( '# CV for Fmult in first year by fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(CV.Fmult.y1, file=out.file,append=T, ncol=nfleets  )
write( '# Lambda for Fmult deviations' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.Fmult.dev, file=out.file,append=T, ncol=nfleets  )
write( '# CV for Fmult deviations' , file=out.file,append=T)   #, ncol=(nyears))
write(CV.Fmult.dev, file=out.file,append=T, ncol=nfleets  )
write( '# Lambda for N in 1st year deviations ' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.N1.dev, file=out.file,append=T )
write( '# CV for N in 1st year deviations ' , file=out.file,append=T)   #, ncol=(nyears))
write(CV.N1.dev, file=out.file,append=T  )
write( '# Lambda for recruitment deviations' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.recr.dev, file=out.file, append=T  )
write( '# Lambda for catchability in first year by index' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.q.y1, file=out.file, append=T, ncol=n.ind.avail )
write( '# CV for catchability in first year by index' , file=out.file,append=T)   #, ncol=(nyears))
write(CV.q.y1, file=out.file, append=T , ncol=n.ind.avail )
write( '# Lambda for catchability deviations by index' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.q.dev, file=out.file,append=T, ncol=n.ind.avail  )
write( '# CV for catchability deviations by index' , file=out.file,append=T)   #, ncol=(nyears))
write(CV.q.dev, file=out.file,append=T  )
write( '# Lambda for deviation from initial steepness' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.h, file=out.file,append=T   )
write( '# CV for deviation from initial steepness' , file=out.file,append=T)   #, ncol=(nyears))
write(CV.h, file=out.file,append=T  )
write( '# Lambda for deviation from initial SSB0 ' , file=out.file,append=T)   #, ncol=(nyears))
write(lam.SSB0, file=out.file,append=T )
write( '# CV for deviation from initial SSB0 ' , file=out.file,append=T)   #, ncol=(nyears))
write(CV.SSB0, file=out.file,append=T  )

write( '# NAA Deviations flag (1=   , 0=  ) ' , file=out.file,append=T)   #, ncol=(nyears))
write(1, file=out.file,append=T  )

write('###########################################', file=out.file, append=T)
write('###  Initial Guesses', file=out.file, append=T)
write( '# NAA for year1' , file=out.file,append=T)   #, ncol=(nyears))
write(naa.y1, file=out.file, append=T, ncol=asap.nages  )
write( '# Fmult in 1st year by fleet' , file=out.file,append=T)   #, ncol=(nyears))
write(Fmult.y1, file=out.file, append=T, ncol=nfleets )
write( '# Catchability in 1st year by index' , file=out.file,append=T)   #, ncol=(nyears))
write(q.y1, file=out.file, append=T  )

write( '# S-R Unexploited specification (1=   0=)' , file=out.file,append=T)   #, ncol=(nyears))
write(1, file=out.file,append=T, ncol=n.ind.avail  )

write( '# Unexploited initial guess' , file=out.file,append=T)   #, ncol=(nyears))
write(SSB0, file=out.file,append=T, ncol=n.ind.avail  )
write( '# Steepness initial guess' , file=out.file,append=T)   #, ncol=(nyears))
write(h.guess, file=out.file,append=T  )
write( '# Maximum F (upper bound on Fmult)' , file=out.file,append=T)   #, ncol=(nyears))
write(F.max, file=out.file,append=T  )
write( '# Ignore guesses' , file=out.file,append=T)   #, ncol=(nyears))
write(ignore.guess, file=out.file,append=T  )
write('###########################################', file=out.file, append=T)
write('###  Projection Control data', file=out.file, append=T)
write( '# Do projections' , file=out.file,append=T)   #, ncol=(nyears))
write(do.proj, file=out.file, append=T   )
write( '# Fleet directed flag' , file=out.file,append=T)   #, ncol=(nyears))
write(fleet.dir, file=out.file, append=T, ncol=nfleets )
write( '# Final year of projections' , file=out.file,append=T)   #, ncol=(nyears))
write(proj.yr, file=out.file, append=T   )
write( '# Year, projected recruits, what projected, target, non-directed Fmult ' , file=out.file,append=T)   #, ncol=(nyears))
write(t(proj.specs), file=out.file,append=T, ncol=5 )
write('###########################################', file=out.file, append=T)
write('###  MCMC Control data', file=out.file, append=T)
write( '# do mcmc' , file=out.file,append=T)   #, ncol=(nyears))
write(do.mcmc, file=out.file,append=T  )
write( '# MCMC nyear option' , file=out.file,append=T)   #, ncol=(nyears))
write(mcmc.nyr.opt, file=out.file,append=T  )
write( '# MCMC number of saved iterations desired' , file=out.file,append=T)   #, ncol=(nyears))
write(mcmc.nboot, file=out.file,append=T  )
write( '# MCMC thinning rate' , file=out.file,append=T)   #, ncol=(nyears))
write(mcmc.thin, file=out.file,append=T  )
write( '# MCMC random number seed' , file=out.file,append=T)   #, ncol=(nyears))
write(mcmc.seed, file=out.file,append=T  )
write('###########################################', file=out.file, append=T)
write('###  A few AGEPRO specs', file=out.file, append=T)
write( '# R in agepro.bsn file' , file=out.file,append=T)   #, ncol=(nyears))
write(recr.agepro, file=out.file,append=T  )
write( '# Starting year for calculation of R' , file=out.file,append=T)   #, ncol=(nyears))
write(recr.start.yr, file=out.file,append=T  )
write( '# Ending year for calculation of R' , file=out.file,append=T)   #, ncol=(nyears))
write(recr.end.yr, file=out.file,append=T  )

write( '# Export to R flag (1=  0=)' , file=out.file,append=T)   #, ncol=(nyears))
write(1, file=out.file,append=T  )

write( '# test value' , file=out.file,append=T)   #, ncol=(nyears))
write(test.val, file=out.file,append=T  )
write('###########################################', file=out.file, append=T)
write('###### FINIS ######', file=out.file, append=T)
write( '# Fleet Names', file=out.file, append=T)
write(fleet.names, file=out.file, append=T, ncol=1)
write( '# Survey Names', file=out.file, append=T)
write(survey.names, file=out.file, append=T, ncol=1)














 }


