# Fit larviposition as a function of time, the data used is found in:
# ~ Hargrove (1994) Reproductive Rates of Tsetse Flies in the the 
# field of Zimbabwe
# ~ Physiological Entomology vol. 19 (4). 307-318
# ~ Hargrove (2004) Tsetse Population Dynamics In the Trypanosiomiases 

require(ggplot2)
#1 Read in data
d.larvi.rate = read.csv("data_larviposition.csv")

#2 Plot data
larvi_plot = ggplot(d.larvi.rate, aes(x = mean_temp, y = rate, col = stage)) +
             scale_color_manual(values = c("black", "red")) +
             xlim(low = 13, high = 33) +
             geom_point(size = 1) +
             ylim(low = 0, high=0.14) +
             labs(y = "Larviposition Rate (per day)", x = "Temperetature (\u00B0C)", title = "Larvipostion Rate") +
             theme_set(theme_bw()) +
             theme(panel.border = element_blank()
                   , plot.title = element_text(hjust = 0.5)
                   , axis.line = element_line(color = "black")
                   , legend.position = c(0.9, 0.1)
                   , legend.title = element_blank()
                   , legend.background = element_blank()
                   , legend.text = element_text(size = 6)
                   , legend.key.size = unit(0.5, "line")
                   , legend.key.height = unit(0.4, "line")
                   , plot.margin = unit(c(0.4, 0.2, 0.2, 0.1), "cm")
                  )
 larvi_plot
  
#3  Create temperature dependent function
 
 lrFunc = function(con.d1, con.d2, T1 = 24, temp){
         lr = con.d1 + con.d2*(temp - T1)
         return(lr)
 }
 
#4  Save our constants for both subsequent and non_subsequent larviposition
 non_sub.d1 = 0.061
 non_sub.d2 = 0.002
 sub.d1 = 0.1046
 sub.d2 = 0.0052
 
#5 Use function to create the predicted value dataframes
 non_sub.lr = lrFunc(con.d1 = non_sub.d1, con.d2 =non_sub.d2, temp = seq(15, 34, 0.1))
 non_sub.lr = cbind.data.frame(temp = seq(15, 34, 0.1)
                              , pred = non_sub.lr
                              , stage = rep("First larviposition"))
                             #, length(non_sub.lr))
 
 sub.lr = lrFunc(con.d1 = sub.d1, con.d2 = sub.d2, temp = seq(15, 34, 0.1))
 sub.lr = cbind.data.frame(temp = seq(15, 34, 0.1)
                           , pred = sub.lr
                           , stage = rep("Subsequent larvipositions"))
                           #, length(non_sub.lr))
 prediction.lr = rbind.data.frame(non_sub.lr, sub.lr)
 
#5 Add prediction to plot

 lrPlotFit <- larvi_plot +
   geom_line(data=prediction.lr
             ,mapping=aes(x=temp,y=pred))
 lrPlotFit
 
 