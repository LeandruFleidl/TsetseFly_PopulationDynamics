require(bbmle)                 # required packages
require(deSolve)
library(zoo)
library(MASS)
library(reshape)
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(readODS)
library(tidyr)
library(stringr)

# scripts needed
source("CountAndTemp.R")
source("AdultMortallityAsFunctionOfTemp.R")
source("FunctionsUsedForModeling.R")
source("LarvipostionAsFunctionOfTemp.R")
source("PupalDurationAsAFunctionOfTemp.R")
source("PupalMortalityAsAFunctionOfTemp.R")
source("FunctionsUsedForModeling.R")


k.temp = read_ods("kruger_temp.ods", sheet = "data")    # Temperature data
#split temp
k.temp = select(k.temp,-c("cnt",	"station_code",	
                          "station_name",	"latitude_dd",	
                          "longitude_dd",	"altitude_m")
                )

# split min and max temp
k.temp_max = filter(k.temp, type == "max temp")
k.temp_min = filter(k.temp, type == "min temp")


# delete type 
k.temp_max = select(k.temp_max, -c("type"))
k.temp_min = select(k.temp_min, -c("type"))

# create mean temp data frame
k.temp_mean <- data.frame(matrix(ncol = 3, nrow = 0))
k.temp_mean = k.temp_max
k.temp_mean$JAN = (k.temp_max$JAN + k.temp_min$JAN)/2 
k.temp_mean$FEB = (k.temp_max$FEB + k.temp_min$FEB)/2 
k.temp_mean$MAR = (k.temp_max$MAR + k.temp_min$MAR)/2 
k.temp_mean$APR = (k.temp_max$APR + k.temp_min$APR)/2 
k.temp_mean$MAY = (k.temp_max$MAY + k.temp_min$MAY)/2 
k.temp_mean$JUN = (k.temp_max$JUN + k.temp_min$JUN)/2 
k.temp_mean$JUL = (k.temp_max$JUL + k.temp_min$JUL)/2 
k.temp_mean$AUG = (k.temp_max$AUG + k.temp_min$AUG)/2 
k.temp_mean$SEP = (k.temp_max$SEP + k.temp_min$SEP)/2 
k.temp_mean$OCT = (k.temp_max$OCT + k.temp_min$OCT)/2 
k.temp_mean$NOV = (k.temp_max$NOV + k.temp_min$NOV)/2 
k.temp_mean$DEC = (k.temp_max$DEC + k.temp_min$DEC)/2 


# pivot data frame so temp is in chronological order
k.temp_mean = pivot_longer(k.temp_mean, cols = 2:13)

# Change Month strings
k.temp_mean$name <- str_replace(k.temp_mean$name, "JAN", "Jan")
k.temp_mean$name <- str_replace(k.temp_mean$name, "FEB", "Feb")
k.temp_mean$name <- str_replace(k.temp_mean$name, "MAR", "Mar")
k.temp_mean$name <- str_replace(k.temp_mean$name, "APR", "Apr")
k.temp_mean$name <- str_replace(k.temp_mean$name, "MAY", "May")
k.temp_mean$name <- str_replace(k.temp_mean$name, "JUN", "Jun")
k.temp_mean$name <- str_replace(k.temp_mean$name, "JUL", "Jul")
k.temp_mean$name <- str_replace(k.temp_mean$name, "AUG", "Aug")
k.temp_mean$name <- str_replace(k.temp_mean$name, "SEP", "Sep")
k.temp_mean$name <- str_replace(k.temp_mean$name, "OCT", "Oct")
k.temp_mean$name <- str_replace(k.temp_mean$name, "NOV", "Nov")
k.temp_mean$name <- str_replace(k.temp_mean$name, "DEC", "Dec")


# create a year.month column
k.temp_mean$Date = paste(k.temp_mean$name, k.temp_mean$Year)


# swap columns out of convention
k.temp_mean = k.temp_mean[c("Date", "value", "name")]

# convert to date

k.temp_mean$Date_time = as.yearmon(k.temp_mean$Date)
  


#create plot data
k.temp_plot = filter(k.temp_mean, name == "Jan" | name == "Mar" | name == "May" | name == "Jul" | name == "Sep" | name == "Nov")

# plot the mean temp at skukuza 
k_temp_plot = ggplot(k.temp_mean, aes(x = Date_time, y = value)) + 
  geom_line( mapping = aes(x = Date_time , y = value),size=0.2,col="black") +    
  labs( y = "Mean Temp of Skukuza"
        , x="Date"
        , title = "Temp of Skukaza") + 
  theme_set(theme_bw()) +
  theme( panel.border = element_blank()
         , axis.line = element_line(color = 'black')
         , plot.title = element_text(size = 14, hjust = 0.5)
         , text = element_text(size = 10)
         , plot.margin=unit(c(0.3,0.7,0.3,0.2), "cm")
         , axis.text=element_text(size=6)
  )
k_temp_with_lines = k_temp_plot +
  geom_line(data = k.temp_mean, mapping = aes(x = Date_time, y = 32)) +
  geom_line(data = k.temp_mean, mapping = aes(x = Date_time, y = 16))
k_temp_with_lines


# Try model for current data

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





kruger_temps = k.temp_mean$value[1:length(k.temp_mean$value)] 
# Time steps to solve in days
kruger_times = seq(0, length(kruger_temps)*30 - 1, 30)



# 2 Population Dynamics Model
tsetse_mod = function(tt, yy, parms) with(c(parms, as.list(yy)), {
  temp = kruger_temps[(tt/30) + 1] #Take temp for the time point in the model
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

simPop = function(init = initial, tseq = kruger_times, modFunction = tsetse_mod, parms = tsetse_params()){
  simDat = as.data.frame(lsoda(init, tseq, modFunction, parms = parms))
  return(simDat)
}

kruger_test = simPop(parms = tsetse_params(mud.p = 0.00006))

k.temp_mean$A = kruger_test$A + kruger_test$J

ggplot(k.temp_mean, aes(x=Date_time,y=A)) +                                   # plot
  geom_line(mapping=aes(x=Date_time,y=A),size=0.2,col="black") +
  labs( y = "Numbers of tsetse flies"
        , x="Date"
        , title = "First Attempt at Modeling Tsetse fly population dynamics (Skukuza)") + 
  scale_x_yearmon(
    limits = as.yearmon(c('1990-02','2010-12'))) +
  scale_y_continuous(trans='log10',limits=c(0.1, high=200)) +
  theme_set(theme_bw()) +
  theme( panel.border = element_blank()
         , axis.line = element_line(color = 'black')
         , plot.title = element_text(size = 14, hjust = 0.5)
         , text = element_text(size = 10)
         , plot.margin=unit(c(0.3,0.7,0.3,0.2), "cm")
         , axis.text=element_text(size=6)
  )
