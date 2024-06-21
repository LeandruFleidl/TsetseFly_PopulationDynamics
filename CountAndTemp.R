# This scripts reads in the data from data_bioassay_counts and data_bioassay temps
# and we then reformat the dates into year month format. We summaries the temp data 
# for a monthly mean and finally bind them into one database.

library("zoo")     # required packages
library("plyr")
library("ggplot2")

# 1 Read in the data
b.fly_count = read.csv("data_bioassay_counts.csv", header = T) # Count data
b.temp = read.csv("data_bioassay_temps.csv" , header = T)    # Temperature data

# 2 Reformat date to year-month 
b.fly_count$Mon.yr = as.yearmon(b.fly_count$Mon.yr, "%d/%m/%Y") 
b.temp$Date = as.Date(b.temp$Date, "%d/%m/%Y")
b.temp$YearMon = as.yearmon(b.temp$Date, "%Y/%m/%d")

# 3 Summaries temp data into monthly mean
b.temp_new = ddply(b.temp, .(YearMon), summarise, 
                   mean.temp_new = mean(MeanC, na.rm = T))  #calculates monthly mean temperature
names(b.temp_new) = c("Time", "Temperature")

# 4 Combine into one Database
temp.fly_count = cbind.data.frame(time = c(b.temp_new$Time),
                                  temp = c(b.temp_new$Temperature),
          count = c(rep(NA, 369), b.fly_count$Mean[1:(length(b.fly_count$Mean)-1)]),
          count.time = as.yearmon(c(rep(NA,369), b.fly_count$Mon.yr[1:(length(b.fly_count$Mean)-1)])))
# 5 Add 5 years of population at the start (the first population) to stabilize
temp.fly_count = rbind.data.frame(temp.fly_count[13:24,],
                                  temp.fly_count[13:24,],
                                  temp.fly_count[13:24,],
                                  temp.fly_count[13:24,],
                                  temp.fly_count[13:24,],
                                  temp.fly_count )

tempPlot = ggplot(data = temp.fly_count, mapping = aes(x = temp.fly_count$time, 
                                                       y = temp.fly_count$temp)
                  , geom_line(data = temp.fly_count, mapping = aes(x = temp.fly_count$time, 
                                                                  y = temp.fly_count$temp)))
tempPlot
