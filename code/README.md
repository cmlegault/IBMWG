R code for the project. The main file to run the simulations should be clearly identified.  
Functions called by the main program can be grouped in separate files (e.g., inputfunctions.R, retrofunctions.R, plotfunctions.R, etc.).

Below is a descriptio the elements provided with the simulated data from a wham operating model. The index-based methods may use one or more of these to generate catch advice during the feedback period.

if model ← fit_wham(input, do.fit = FALSE)

model$years = the years of the base period

model$years_full the years of both the base and feedback period

Assuming y below is the object returned by model $simulate(complete=TRUE)
y = model$simulate(complete= TRUE)

y is a list with all the same elements as input$data as well as elements from model$report(). The pertinent elements of  the list y are:

agg_catch, agg_catch_proj: The matrices of observed aggregate catch in the base period and projection/feedback period. rbind these to get both periods together. (nyears x nfleets)

pred_catch: matrix of expected (true) catches. Base and feedback together.

pred_CAA: array of expected (true) catch at age in abundance. Base and feedback together. (nyears x nfleets x nages). 

catch_paa, catch_paa_proj: The arrays of observed proportions at age in the catch (in numbers). (nfleets x nyears x nages)

pred_catch_paa: array of expected (true) proportions at age in the catch. Base and feedback together. (nyears x nfleets x nages)

agg_indices, agg_indices_proj: The matrices of observed aggregate indices in the base period and projection/feedback period. rbind these to get both periods together. (nyears x nindices)

pred_indices: The matrix of expected (true) aggregate indices. Base and feedback together.

pred_IAA: array of expected (true) indices at age. Units (abundance/biomass) are the same as index_paa. Base and fedback together. (nyears x nindices x nages).

index_paa, index_paa_proj: The arrays of observed proportions at age in the indices (in numbers). (nindices x nyears x nages)

pred_index_paa: The array of expected proportions at age for indices. Base and feedback together. (nyears x nindices x nages)

waa: an array with different weights at age for fleets, total catch, indices, ssb, jan1 (see next item). The weight at age are taken as known so observed and true are the same thing. Base and feedback together. (ntypes x nyears x nages). Values for projection years are average of values in model$years[y$avg_years_ind+1].

waa_pointer_ssb, waa_pointer_catch, waa_pointer_indices: pointers to which parts of waa are for the respective types. I think all waa are the same so not really needed

mature: matrix of maturity at age for SSB. (nyears x nages). Base and projection years together. Taken as known so observed and true are the same thing. Values in projection years are determines the same way as weight at age.

selAA: list of matrices of selectivities at age and years for fleets and indices. Length is the total number of selectivity blocks. Each matrix is (nyears x n ages). Selectivity for fleets in projection years is defined from averaging F by age for each fleet (call this faa_f) and summing by age (call this faa_tot).  The years to average are defined the same way as weight at age. Selectivity for each fleet in the projection years is then saa_f = faa_f/faa_tot[y$which_F_age]. Then F at age for each fleet for projection years is saa_f * F_proj. Selectivity for indices in projection years is taken from the last year of the base period.

selblock_pointer_fleets, selblock_pointer_indices: which elements of the list selAA are for each fleet and index.

q: vector of catchabilities for each index.

NAA: matrix of yearly abundance at age. Base and feedback together. (nyears x nages)

MAA: matrix of natural mortality. Base and feedback together. Values in projection years determined the same way as weight at age. (nyears x nages).

F: matrix of fully selected F for each fleet. Base and feedback together. (nyears x nfleets)

FAA: array of F at age by fleet. Base and feedback together. (nyears x nfleets x nages).

FAA_tot: matrix of total F at age. Base and feedback together. (nyears x nages).s

ZAA: matrix of total mortality at age. Base and feedback together. (nyears x nages).

SSB: vector of yearly SSB. Base and feedback together.

log_SR_a: vector of yearly log(a) parameters for SR curve.

log_SR_b: vector of yearly log(b) parameters for SR curve.

log_FMSY: vector of yearly (log) Fmsy values (changes iif there are changes in waa, maturity, M, etc.)

log_MSY: vector of yearly (log) MSY values.

log_SSB_MSY: vector of yearly (log) SSB at Fmsy.

log_R_MSY: vector of annual (log) recruitment at Fmsy

log_SPR0: vector of annual (log) unexploited SSB/R.

log_SPR_MSY: vector of annual (log) SSB/R at Fmsy.

log_YPR_MSY: vector of annual (log) Y/R at Fmsy.

SR_h_tf: vector of annual transformed steepness. For BH: logit transformation: log(h-0.2) – log(1-h)

percent SPR: percentage of SPR to use for reference points

log_FXSPR: vector of yearly F at X%SPR

log_YPR_XSPR: vector of yearly Y/R at X%SPR

I know. Sometimes SPR is spawning potential ratio, sometimes it is SSB/R.
 
