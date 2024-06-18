#Likelihood function
likelihood.pois = function(parms = tstetse_params(), data = temp.fly_count){
  if (parms$a1 > 0.04 | parms$a1 < 0.01){
    like = -10000000000000                       # we do this to punish the likelihood if the mortality 
  }                                              # > 0.03 if temp < 25
  else {
    simulate = simPop(parms = parms)
    counts = dat$count[1:length(dat$temp)]
    times = seq(from = 0, to = length(counts)*30 -1, by = 30)
    # take out all the NA from the data
    times = times[!counts %in% NA]
    counts = counts[!counts %in% NA]
    # round observed counts
    mean.tsetse = round(count, 0)
    # rounded modeled out puts
    est = round(simulate[simulate$time %in% times, "A"] + simulate[simulate$time %in% times, "J"] , 0)
    # take the values from 1970 to 1990
    subset = simulate[124:363, "A"] + simulate[124:363, "A"]
    # average count
    mean.count = mean(subset)
    if (mean.count < 50) {
      like = -1000000000000        # Again we punish the likelihood if our count is less than 50
    }
    else {
      like = (dpois(mean.tsetse, lambda = est, log = T))
    }
    
  }
  return(-like)
}
# function to fit and substitute the parameters back into list
subsParm = function(fitparams, fixed.params = tsetse_params()) {
  within(fixed.params, {
    loggedParms = names(fitparams)[grepl('log_', names(fitparams))]
    unloggedParms = names(fitparams)[!grepl('log_'), names(fitparams)]
    for(nm in unloggedParms) assign(nm, as.numeric(fitparams[nm]))
    for(nm in loggedParms) assign(gsub('log_', '', nm), exp(as.numeric(fitparams[nm])))
    rm(nm, loggedParms, unloggedParms) 
  })
}

fit_and_fixed_like = function(fit.param, fixed.params = tesetse_params(), dat = temp.fly_count){
  parms = subsParms(fit.params, fixed.params)
  nll.pois(parms, dat = dat)    
}