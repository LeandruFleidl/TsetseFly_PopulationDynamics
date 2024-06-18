# Here we fit the pupal duration (the time it take for the fly pupi to turn into a flies) using
# data and the fit obtained by:
# ~ Phelps (1969) Puporial Duriation in Glossina Morsitans Orientalis under conditions of
# constant temperature.
# ~ Entomologia Experimetalis et Applicata vol 12 33-43.

require(ggplot2)
#1 Read in the data
pupal.duration.data = read.csv("data_pupal_duration.csv")

#2 Plot the data 
pupal.duration.plot = ggplot(data = pupal.duration.data, aes(x = test_temp, y = females_r)) +
                       xlim(low = 15, high = 35) +
                       ylim(low = 0, high = 0.06) +
                       geom_point(size = 1) +
                       labs(y = "Pupal Duration"
                           ,x = "Temperature"
                           ,title = "Pupal emergence rate (per day)") +
                       theme_set(theme_bw()) +
                       theme(panel.border = element_blank()
                             , plot.title = element_text(hjust = 0.5)
                             , axis.line = element_line(colour = "black")
                             , text = element_text(size = 10)
                             , axis.text = element_text(size = 6))
pupal.duration.plot

#4 Set up function for pupal duration as a function of temp
# from the ref at the top we get the constants we need 
# c1 = 0.05884, c2 = 4.8829, c3 = -0.2159
pdFunc = function(c1 = 0.05884, c2 = 4.8829, c3 = -0.2159, temp){
       pupal.duration = c1/(1 + exp(c2 + c3*temp))
       return(pupal.duration)
}

#4 Generate the predicted value for the pupal duration
pdPred = pdFunc(temp = seq(14, 35, 0.1))
pdPred = cbind.data.frame(temp = seq(14, 35, 0.1), pdPred = pdPred)

#5 Add the predicted values to the plot
predicted.pd.plot = pupal.duration.plot +
                  geom_line(data = pdPred, mapping = aes(x = temp, y = pdPred))
predicted.pd.plot
