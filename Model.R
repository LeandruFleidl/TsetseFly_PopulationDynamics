require(bbmle)                 # required packages
require(deSolve)
library(zoo)
library(MASS)
library(reshape)
library(ggplot2)
library(gridExtra)
library(grid)

# scripts needed
source("CountAndTemp.R")
source("AdultMortallityAsFunctionOfTemp.R")
source("FunctionsUsedForModeling.R")
source("LarvipostionAsFunctionOfTemp.R")
source("PupalDurationAsAFunctionOfTemp.R")
source("PupalMortalityAsAFunctionOfTemp.R")
source("FunctionsUsedForModeling.R")
# 1 Model Parameters and Conditions
  # Mean temp for each month see CountAndTemp
temps = temp.fly_count$temp[1:length(temp.fly_count$temp)] 
  # Time steps to solve in days
times = seq(0, length(temps)*30 - 1, 30)
  # A function to save all the parameters 
tsetse_params = function(a1 = A.a1 # Adult Mortality  
                       , a2 = A.a2
                       , b1 = M.b1 # Pupal Mortality
                       , b2 = M.b2
                       , b3 = M.b3
                       , b4 = M.b4
                       , b5 = M.b5
                       , c1 = D.c1 # Pupal Duration
                       , c2 = D.c2
                       , c3 = D.c3
                       , d1 = non_sub.d1 # Larviposition
                       , d2 = non_sub.d2
                       , d3 = sub.d1
                       , d4 = sub.d2
                       , mud.p = 0.00005   # pupal density dependent mortality 
                       , adults.zero = 100 # number of parous adults at start
                       , juv.zero = 25     # number of nonparous adults at the starts
                       , pupae.zero = 100  # number of pupae at the start
                       ) 
return(as.list(environment()))

# Create a list with initial conditions 
initial = c( P = tsetse_params()$pupae.zero
           , J = tsetse_params()$juv.zero  
           , A = tsetse_params()$adults.zero)
# 2 Population Dynamics Model
tsetse_mod = function(tt, yy, parms) with(c(parms, as.list(yy)), {
  temp = temps[(tt/30) + 1] #Take temp for the time point in the model
  pup.dur = pdFunc(c1, c2, c3, temp = temp)
  pup.mort = pmFunc(b1, b2, b3, b4, b5, temp =temp)
  adult.mort = adult_mortalityFunc(a1, a2, temp = temp)
  larvi.par = lrFunc(d3, d4, temp = temp)
  larvi.non = lrFunc(d1, d2, temp = temp)
  
  # ODE
  deriv = rep(NA, 2)  # empty list to store ODE's
  
  deriv[1] = larvi.par*A + larvi.non*J - pup.dur*P - mud.p*P*P - pup.mort*P #change in pupae
  deriv[2] = pup.dur*P/2 - adult.mort*J - J*(larvi.non) #change in nonparous adults
  deriv[3] = J*(larvi.non) -  adult.mort*A  #change in parous adults
  
  return(list(deriv))
})

# 3 Function to Simulate model
simPop = function(init = initial, tseq = times, modFunction = tsetse_mod, parms = tsetse_params()){
  simDat = as.data.frame(lsoda(init, tseq, modFunction, parms = parms))
  return(simDat)
}

test = simPop(parms = tsetse_params(mud.p = 0.00006))

temp.fly_count$A = test$A + test$J
plot.dat = temp.fly_count

ggplot(plot.dat, aes(x=time,y=count)) +                                   # plot
  geom_line(mapping=aes(x=time,y=A),size=0.2,col="black") +
  geom_point(size=0.4,col="red") +
  labs( y = "Numbers of tsetse flies"
        , x="Date"
        , title = "First Attempt") + 
  scale_x_yearmon(
    limits = as.yearmon(c('1960-02','2017-12'))) +
  scale_y_continuous(trans='log10',limits=c(0.1, high=200)) +
  theme_set(theme_bw()) +
  theme( panel.border = element_blank()
         , axis.line = element_line(color = 'black')
         , plot.title = element_text(size = 14, hjust = 0.5)
         , text = element_text(size = 10)
         , plot.margin=unit(c(0.3,0.7,0.3,0.2), "cm")
         , axis.text=element_text(size=6)
  )

# Just population
ggplot(plot.dat, aes(x=time,y=count)) +                                   # plot
  geom_point(size=0.4,col="red") +
  labs( y = "Numbers of tsetse flies"
        , x="Date"
        , title = "Tsetse fly population over time") + 
  scale_x_yearmon(
    limits = as.yearmon(c('1991-02','2017-12'))) +
  scale_y_continuous(trans='log10',limits=c(0.1, high=200)) +
  theme_set(theme_bw()) +
  theme( panel.border = element_blank()
         , axis.line = element_line(color = 'black')
         , plot.title = element_text(size = 14, hjust = 0.5)
         , text = element_text(size = 10)
         , plot.margin=unit(c(0.3,0.7,0.3,0.2), "cm")
         , axis.text=element_text(size=6)
  )

