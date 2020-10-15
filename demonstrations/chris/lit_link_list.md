## Literature Links List
A collection of links to papers broken into separate groups with short synopsis of why each is included.

### Index Based Methods
* [DLMtool](https://www.datalimitedtoolkit.org/) provides a large number of possible index-based methods and a framework for conducting closed-loop simulations
* [Geromont and Butterworth](https://academic.oup.com/icesjms/article/72/1/262/821583) 2015b compares historical performance of simple index-based approaches to age-based methods for stocks in this region
* [Klaer](https://www.sciencedirect.com/science/article/abs/pii/S0165783612002561) et al. 2012 applies an average length-based approach in Australia
* [Little](https://doi:10.1093/icesjms/fsr019) et al. 2011 examines a number of index-based methods in closed-loop simulation for Southeast Australia stocks
* [Smith](https://www.sciencedirect.com/science/article/abs/pii/S0165783608001835) et al. 2008 lessons learned from SE Australia from applying index-based methods
* [Then](https://doi:10.1093/icesjms/fsx177) et al. 2018 introduces a length-based method
* [Wiedenmann](https://doi.org/10.1016/j.fishres.2018.09.018) et al. 2019 applies a number of index-based methods to actual stocks in this region and compares results to age-based methods

### Closed-loop simulations
* [Fay](https://www.sciencedirect.com/science/article/abs/pii/S0165783611001640) et al. 2011 examines spatial impact on MSE for a stock and includes a way to summarize large number of simulation results
* [Geromont and Butterworth](https://academic.oup.com/icesjms/article/72/1/251/815189) 2015a demonstrates how to test index-based methods through closed-loop simulation
* [Hart and Fay](https://doi.org/10.1016/j.fishres.2019.105466) 2020 tree-regression for summarizing large number of simulation results
* [Hordyk]( https://doi.org/10.1111/faf.12382) et al. 2019 uses DLMtool to examine risks of misspecifying a stock assessent model

### Retrospective patterns
* [Brooks and Legault](https://doi.org/10.1139/cjfas-2015-0163) 2016 uses NEFSC stocks to demonstrate a way to compare effects of different parts of projections - found starting point errors due to retrospective patterns biggest problem
* [Deroba](https://doi.org/10.1080/02755947.2014.882452) 2014 uses simulations based on Atlantic herring to compare ignoring and rho-adjusting assessment estimates
* [Hurtado-Ferro](https://doi.org/10.1093/icesjms/fsu198) et al. 2015 uses SS3sim to compare impacts of changes in M, growth, and selectivity on Mohn's rho and derives rules of thumb for significant retrospective pattern for different life histories
* [Legault](https://repository.library.noaa.gov/view/noaa/3611) 2009 examines wide range of factors that can cause retrospective patterns and some possible fixes
* [Miller and Legault](https://doi.org/10.1016/j.fishres.2016.08.002) 2019 provides variance of Mohn's rho through bootstrapping and recommends always adjusting for retrospective pattern
* [Mohn](https://doi.org/10.1006/jmsc.1999.0481) 1999 presents Mohn's rho - note the original formulation uses sum of the peels while current use is mean of the peels
* [Punt](https://doi.org/10.1016/j.fishres.2019.105465) et al. 2020 shows retrospective patterns a major reason stock assessments are rejected around the world
* [Stewart and Martell](https://doi.org/10.1016/j.fishres.2013.09.012) 2014 shows three times the retrospective pattern has been fixed for Pacific halibut by changing how selectivity modeled
* [Szuwalski](https://doi.org/10.1093/icesjms/fsx159) et al. 2018 conducts three by three experiment where M, growth, and selectivity cause or fix retrospective patterns and shows management advice can be wrong when incorrect source used as fix
* [Wiedenmann and Jensen](https://doi.org/10.1139/cjfas-2016-0484) 2018 shows that catch below quota results in F above target F in many New England groundfish stocks due to retrospective pattern
* [Wiedenmann and Jensen](https://doi.org/10.1139/cjfas-2018-0129) 2019 demonstrates that rho-adjustment alone insufficient to end overfishing for New England groundfish stocks, changes to harvest control rule also needed

### Autocorrelated Recruitment
* [Hawkshaw and Walters](https://doi.org/10.1139/cjfas-2014-0212) 2015 concluded that correlation in that parameter shifted the optimal policy away from fixed escapement toward fixed exploitation using a Ricker curve with AR1 deviations for salmon populations
* [Walters and Parma](https://doi.org/10.1139/f95-151) 1996 concluded that correlation in that parameter shifted the optimal policy away from fixed escapement toward fixed exploitation using autocorrelated changes in asymptote parameter of Bev-Holt curve for salmon populations
* [Wiedenmann et al.](https://doi.org/10.1139/cjfas-2016-0381) 2017 looked at autocorrelation in recruitment (0 or 0.44 which was the mean from the Thorson et al. RAM meta-analysis), and didn't find much of an effect on long term performance for a given control rule

### Programming Practices
* [Wilson et al.](https://doi.org/10.1371/journal.pbio.1001745) 2014 Best practices for scientific computing, short overview for large groups working with professionally trained computer programmers
* [Wilson et al.](https://doi.org/10.1371/journal.pcbi.1005510) 2017 Good enough practices in scientific computing, a pragmatic approach to programming practices for small groups and self-taught programmers

