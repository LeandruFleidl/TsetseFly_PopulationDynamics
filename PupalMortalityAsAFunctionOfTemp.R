# We use data from:
# Phelps 1973 The effect of temperature on fat consumption during the puparial stages of
# Glossina morsitans morsitans Westw. (Dipt., Clossinidae) under laboratory conditions,
# and its implication in the field. Bull. Ent. Res. 62, 423-438

source("PupalDurationAsAFunctionOfTemp.R")
require(ggplot2)

#1 Read in the data
d.pupal.mortality = read.csv("data_pupal_mort.csv")

#2 Calculate total number of flies
emerged = d.pupal.mortality$m.emerge + d.pupal.mortality$f.emerge
died = round(d.pupal.mortality$mortality/100*emerged/(1 - d.pupal.mortality$mortality/100), 0)
tot_flies = emerged + died

#3 Convert proportion died to instant daily mortality
pupal.duration = 1/pdFunc(temp = d.pupal.mortality$temp)
instant.mort = -log(1 -(died/tot_flies))/pupal.duration

#4 Add instant mortality to the data frame
d.pupal.mortality$instant.mort = instant.mort

#5 Plot data 
pupal.mortalityPlot = ggplot(d.pupal.mortality, aes(x = temp, y = instant.mort)) +
                      xlim(low = 14, high = 35) +
                      geom_point(size = 1) +
                      ylim(low = 0, high = 0.06) +
                      labs(x = "Temperature", 
                           y = "Pupal Mortality", 
                           title = "Pupal Mortality at a certain temp") +
                      theme_set(theme_bw()) +
                      theme(panel.border = element_blank()
                          ,plot.title = element_text(size = 10, hjust = 0.5)
                          , axis.line = element_line(color = "black")
                          , text = element_text(size = 6)
                          , plot.margin = unit(c(0.4, 0.2, 0.2, 0.1), "cm")
                          , axis.text = element_text(size = 8))
pupal.mortalityPlot
                                         
