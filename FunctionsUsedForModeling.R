# #Likelihood function
# likelihood.pois = function(parms = tstetse_params(), data = temp.fly_count){
#   if (parms$a1 > 0.04 | parms$a1 < 0.01){
#     like = -10000000000000                       # we do this to punish the likelihood if the mortality 
#   }                                              # > 0.03 if temp < 25
#   else {
#     simulate = simPop(parms = parms)
#     counts = data$count[1:length(data$temp)]
#     times = seq(from = 0, to = length(counts)*30 -1, by = 30)
#     # take out all the NA from the data
#     times = times[!counts %in% NA]
#     counts = counts[!counts %in% NA]
#     # round observed counts
#     mean.tsetse = round(counts, 0)
#     # rounded modeled out puts
#     est = round(simulate[simulate$time %in% times, "A"] + simulate[simulate$time %in% times, "J"] , 0)
#     # take the values from 1970 to 1990
#     subset = simulate[124:363, "A"] + simulate[124:363, "A"]
#     # average count
#     mean.count = mean(subset)
#     if (mean.count < 50) {
#       like = -1000000000000        # Again we punish the likelihood if our count is less than 50
#     }
#     else {
#       like = (dpois(mean.tsetse, lambda = est, log = T))
#     }
#     
#   }
#   return(-like)
# }
# # function to fit and substitute the parameters back into list
# subsParms <- function(fit.params, fixed.params=tsetse_params())
#   
#   within(fixed.params, {
#     loggedParms <- names(fit.params)[grepl('log_', names(fit.params))]
#     
#     unloggedParms <- names(fit.params)[!grepl('log_', names(fit.params))]  
#     
#     for(nm in unloggedParms) assign(nm, as.numeric(fit.params[nm]))
#     
#     for(nm in loggedParms) assign(gsub('log_','',nm), exp(as.numeric(fit.params[nm])))
#     
#     rm(nm, loggedParms, unloggedParms)
#   })           
# 
# fit_and_fixed_like = function(fit.params, 
#                               fixed.params = tsetse_params(), 
#                               dat = temp.fly_count) {
#   parms = subsParms(fit.params, fixed.params)
#   likelihood.pois(parms, data = dat)    
# }




# This script contains the functions for fitting the model to the
# data and for plotting the results
# Likelihood function takes the model output and the data and calculates the likelihood
# subsParms just enables multiple parameters to be fitted and substituted back into the parameter list



#***********Likelihood function**********************************
nll.pois <- function(parms = tstetse_params(), data = temp.fly_count){ 
  
  if (parms$a1 > 0.04 | parms$a1 < 0.01 ) {                                # penalise model fits where adult mortality at temperatures =< 25 is > 0.03
    ll <- -1000000000
  }else{                                                      # if mu.a.k1 parameter is < 0.03 proceed to run the model
    
    simulate <- simPop(parms=parms)                           # run the model and save output to object 'simulate'
    
    counts <- data$count[1:length(data$temp)]               # observed counts from bioassay catches
    
    times <- seq(from = 0, to = length(counts)*30-1
                 ,by = 30)                                   # select only the model times that we have data for
    
    times <- times[!counts %in% NA]                         # times have data for
    
    counts <- counts[!counts %in% NA]                       # get rid of NA rows in observed data
    
    mean.tsetse <- round(counts,0)                         # round observed counts
    
    est <- round(simulate[simulate$time %in% times,"A"]+simulate[simulate$time %in% times,"J"],0)  # model output numbers of adult females at timepoints have data for
    
    subset <- simulate[124:363,"A"] + simulate[124:363,"J"]   # take simulated counts between ~ 1970 and 1990
    
    mean.count <- mean(subset)                              # average count for this period
    
    if(mean.count < 50)                                  # if the average is below 50 then penalise model
    { ll <- -10000000
    }else{                                               # otherwise go ahead and calculate log likelihood
      
      ll <- sum(dpois(mean.tsetse,lambda=est,log=T))  # log likelihood assuming data are Poisson distributed
    }
  }
  return(-ll)
}
#**********************************************************



#********enable multiple parameters to be fit and substituted back in parameter list****
subsParms <- function(fit.params, fixed.params=tsetse_params())
  
  within(fixed.params, {
    loggedParms <- names(fit.params)[grepl('log_', names(fit.params))]
    
    unloggedParms <- names(fit.params)[!grepl('log_', names(fit.params))]  
    
    for(nm in unloggedParms) assign(nm, as.numeric(fit.params[nm]))
    
    for(nm in loggedParms) assign(gsub('log_','',nm), exp(as.numeric(fit.params[nm])))
    
    rm(nm, loggedParms, unloggedParms)
  })           

## Make likelihood a function of fixed and fitted parameters.
objFXN <- function(fit.params                                                          ## paramters to fit
                   , fixed.params =tsetse_params()                                     ## fixed paramters
                   , dat=temp.fly_count) {
  parms <- subsParms(fit.params, fixed.params)
  nll.pois(parms, dat = dat)                                                           ## then call likelihood function
}
#***************************************************************************************


