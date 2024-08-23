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

# save the data in this form for later use
f.temp = k.temp_mean

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
        , title = "Modeling Tsetse fly population dynamics (Skukuza)") + 
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
# Now we attempt to create a data set that shows linear growth of 2C in 40 years

# First I create set of discrete variables from 1 to 480 each representing a month from 
# Jan 2010 to Dec 2049

f.months = c(0:479)

# I have shown each month is 0.00417 more than the next so I want to save a vector of these
# growth values as well(if we have a growth of 2C)
# (0.0083 if we have a growth of 4C)
# (0.00625 for 3C)
# (0.01 for 5C)
#(0.002 for 1C)


f.growth = c(0.004*f.months)

# from this we get 1.99C growth and that is good enough for our purposes

# Now the difficult part, we want to add these values to our given data
# we want to coppy our data twice



# We will replace the years in each group, merge them and add our growth to the temp

 future1.temp = f.temp  %>%
  mutate(Year = replace(Year, Year == 1990, 2010)) %>%
  mutate(Year = replace(Year, Year == 1991, 2011)) %>%
  mutate(Year = replace(Year, Year == 1992, 2012)) %>%
  mutate(Year = replace(Year, Year == 1993, 2013)) %>%
  mutate(Year = replace(Year, Year == 1994, 2014)) %>%
  mutate(Year = replace(Year, Year == 1995, 2015)) %>%
  mutate(Year = replace(Year, Year == 1996, 2016)) %>%
  mutate(Year = replace(Year, Year == 1997, 2017)) %>%
  mutate(Year = replace(Year, Year == 1998, 2018)) %>%
  mutate(Year = replace(Year, Year == 1999, 2019)) %>%
  mutate(Year = replace(Year, Year == 2000, 2020)) %>%
  mutate(Year = replace(Year, Year == 2001, 2021)) %>%
  mutate(Year = replace(Year, Year == 2002, 2022)) %>%
  mutate(Year = replace(Year, Year == 2003, 2023)) %>%
  mutate(Year = replace(Year, Year == 2004, 2024)) %>%
  mutate(Year = replace(Year, Year == 2005, 2025)) %>%
  mutate(Year = replace(Year, Year == 2006, 2026)) %>%
  mutate(Year = replace(Year, Year == 2007, 2027)) %>%
  mutate(Year = replace(Year, Year == 2008, 2028)) %>%
  mutate(Year = replace(Year, Year == 2009, 2029)) 

 future2.temp = f.temp  %>%
   mutate(Year = replace(Year, Year == 1990, 2030)) %>%
   mutate(Year = replace(Year, Year == 1991, 2031)) %>%
   mutate(Year = replace(Year, Year == 1992, 2032)) %>%
   mutate(Year = replace(Year, Year == 1993, 2033)) %>%
   mutate(Year = replace(Year, Year == 1994, 2034)) %>%
   mutate(Year = replace(Year, Year == 1995, 2035)) %>%
   mutate(Year = replace(Year, Year == 1996, 2036)) %>%
   mutate(Year = replace(Year, Year == 1997, 2037)) %>%
   mutate(Year = replace(Year, Year == 1998, 2038)) %>%
   mutate(Year = replace(Year, Year == 1999, 2039)) %>%
   mutate(Year = replace(Year, Year == 2000, 2040)) %>%
   mutate(Year = replace(Year, Year == 2001, 2041)) %>%
   mutate(Year = replace(Year, Year == 2002, 2042)) %>%
   mutate(Year = replace(Year, Year == 2003, 2043)) %>%
   mutate(Year = replace(Year, Year == 2004, 2044)) %>%
   mutate(Year = replace(Year, Year == 2005, 2045)) %>%
   mutate(Year = replace(Year, Year == 2006, 2046)) %>%
   mutate(Year = replace(Year, Year == 2007, 2047)) %>%
   mutate(Year = replace(Year, Year == 2008, 2048)) %>%
   mutate(Year = replace(Year, Year == 2009, 2049)) 
  



future.temp = rbind(future1.temp, future2.temp)
# Now we add our growth rate 
future.temp$value = future.temp$value + f.growth

# Round off so our data is comparable
future.temp$value = round(future.temp$value, digits = 2)

# Now we can try our model for this future data 

# create a year.month column
future.temp$Date = paste(future.temp$name, future.temp$Year)


# swap columns out of convention
future.temp = future.temp[c("Date", "value", "name")]

# convert to date

future.temp$Date_time = as.yearmon(future.temp$Date)



# plot the mean temp at skukuza 
k_temp_plot = ggplot(future.temp, aes(x = Date_time, y = value)) + 
  geom_line( mapping = aes(x = Date_time , y = value),size=0.2,col="black") +    
  labs( y = "Predicted Mean Temp at Skukuza"
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
  geom_line(data = future.temp, mapping = aes(x = Date_time, y = 32)) +
  geom_line(data = future.temp, mapping = aes(x = Date_time, y = 16))
k_temp_with_lines


# Test model for this new data
f_kruger_temps = k.temp_mean$value[1:length(k.temp_mean$value)] 
# Time steps to solve in days
f_kruger_times = seq(0, length(kruger_temps)*30 - 1, 30)



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

simPop = function(init = initial, tseq = f_kruger_times, modFunction = tsetse_mod, parms = tsetse_params()){
  simDat = as.data.frame(lsoda(init, tseq, modFunction, parms = parms))
  return(simDat)
}

# Test model for this new data
f_kruger_temps = future.temp$value[1:length(future.temp$value)] 
# Time steps to solve in days
f_kruger_times = seq(0, length(f_kruger_temps)*30 - 1, 30)



# 2 Population Dynamics Model
tsetse_mod = function(tt, yy, parms) with(c(parms, as.list(yy)), {
  temp = f_kruger_temps[(tt/30) + 1] #Take temp for the time point in the model
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
future.kruger.test = simPop(parms = tsetse_params(mud.p = 0.00006))

future.temp$A = future.kruger.test$A + future.kruger.test$J

ggplot(future.temp, aes(x=Date_time,y=A)) +                                   # plot
  geom_line(mapping=aes(x=Date_time,y=A),size=0.2,col="black") +
  labs( y = "Numbers of tsetse flies"
        , x="Date"
        , title = "Future Tsetse Population at Skukuza") + 
  scale_x_yearmon(
    limits = as.yearmon(c('2010-01','2049-12'))) +
  scale_y_continuous(trans='log10',limits=c(0.1, high=200)) +
  theme_set(theme_bw()) +
  theme( panel.border = element_blank()
         , axis.line = element_line(color = 'black')
         , plot.title = element_text(size = 14, hjust = 0.5)
         , text = element_text(size = 10)
         , plot.margin=unit(c(0.3,0.7,0.3,0.2), "cm")
         , axis.text=element_text(size=6)
  )
