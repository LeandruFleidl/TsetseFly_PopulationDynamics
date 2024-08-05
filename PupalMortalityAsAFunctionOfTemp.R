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
                      labs(x = "Temperature (\u00B0C)", 
                           y = "Pupal Mortality (per day)", 
                           title = "Pupal Mortality") +
                      theme_set(theme_bw()) +
                      theme(panel.border = element_blank()
                          ,plot.title = element_text(size = 14, hjust = 0.5)
                          , axis.line = element_line(color = "black")
                          , text = element_text(size = 10)
                          , plot.margin = unit(c(0.4, 0.2, 0.2, 0.1), "cm")
                          , axis.text = element_text(size = 8))
pupal.mortalityPlot

#6 Create temp dependent function
pmFunc = function(b1, b2, b3, b4, b5, T1 = 16
                  , T2 = 32, temp){
  pred.mort = b1 + b2*exp(-(b3*(temp - T1))) + b4*exp(b5*(temp - T2))
  return(pred.mort)
}

#7 Fit function with nonlinear least square regression
pupal.mortalityFit = nls(instant.mort ~ b1 + b2*exp(-(b3*(temp-16))) + b4*exp(b5*(temp-32))
                         ,data=d.pupal.mortality
                         ,start=list(b1=0.002, b2=0.005, b3=1.5, b4=0.03, b5=1.2)
                         ,trace=T)
pmfsum = summary(pupal.mortalityFit)
pmfsum

#8 Save coefficients for use in time dependent function
M.b1 = coef(pmfsum)[1]
M.b2 = coef(pmfsum)[2]
M.b3 = coef(pmfsum)[3]
M.b4 = coef(pmfsum)[4]
M.b5 = coef(pmfsum)[5]

#9 Calculate the predicted values
pupal.mortality.pred = pmFunc(b1 = M.b1, b2 = M.b2, b3 = M.b3
                            , b4 = M.b4, b5 = M.b5, temp = seq(14, 35, 0.1))
pupal.mortality.pred = cbind.data.frame(temp = seq(14, 35, 0.1), pred = pupal.mortality.pred)

#10 Add predicted values to plot
predPlot = pupal.mortalityPlot +
          geom_line(data = pupal.mortality.pred
                  , mapping = aes(x = temp, y = pred))
predPlot

