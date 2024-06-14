# We use data collected by Prof John Hangrove in a catch, mark, release and re-catch experiment 
# at Rekommetjie station in Zimbabwe this experiment was used to estimate the mortality rate 
# of adult G. pallidipes flies compared to mean temperature. 
# The papers used are:
# Hargrove (2001) Factors affecting density-independent survival of an island population of tsetse flies in Zimbabwe.
# Hargrove (2004) Tsetse population dynamics. In: The Trypanosomiases.

require(minpack.lm)
require(ggplot2)

# 1 Read in the data
b.A_mortality = read.csv("data_adult_mort.csv")

# 2 Plot the raw data
aMortplot = ggplot(b.A_mortality, aes(x = mean_temp, y = mortality) ) +
                                 xlim(low = 15, high = 35) +
                                 geom_point(size = 1) +
                                 labs( y = "Adult Mortality Rate/day"
                                      , x = "Mean Temperature"
                                      , title = "Adult Mortality compared to Mean Temperature")
                                 theme_set(theme_bw()) +
                                 theme(panel.border = element_blank()
                                        , axis.line = element_line(colour = 'black')
                                        , text = element_text(size = 6)
                                        , plot.margin = unit(c(0.4, 0.2, 0.2, 0.1), "cm")
                                        , axis.text = element_text(size = 6)
                                 )
aMortplot

#3 Temperature dependent function as seen on page 5/18 0f lord et al
adult_mortalityFunc = function(con.a1, con.a2, temp){
  if (temp > 25){
    mortality.rate = con.a1*exp((con.a2)*(temp - 25))
  }else{
    mortality.rate = con.a1
  }
  return(mortality.rate)
}
# 4 Fit to a nonlinear curve using adult_mortalityFunc as our nonlinear curve and
# using nls.lm to minimize the difference between our real and expected value.
amFunctFit = function(params, dat = b.A_mortality){
           a1 = params[1]
           a2 = params[2]
           temp = dat$mean_temp
           mod.mort <- sapply(temp,function(x){ adult_mortalityFunc(    
              con.a1=a1
             ,con.a2=a2
             ,x)
           })
           diffrence = log(mod.mort) - log(dat$mortality)
           return(diffrence)
}

adult_mortalityFit = nls.lm(par = c(a1 = 0.01, a2 = 0.06),  fn = amFunctFit)

#5 Store the paramaters we found for later use
amParams = summary(adult_mortalityFit)
a1 = coef(adult_mortalityFit)[1]
a2 = coef(adult_mortalityFit)[2]

#6 Now we put the predicted values of the mortality into a data frame
adult_mortality.Predicted = sapply(seq(15, 34, 0.01), function(x){
                                          adult_mortalityFunc(con.a1 = a1
                                                              ,con.a2 = a2
                                                              ,x)
                                                        })
adult_mortality.Predicted = cbind.data.frame(temp = seq(15, 34, 0.01), 
                                      Predicted = adult_mortality.Predicted)
#7 Finally we can add it to the real value plot
plotFit = aMortplot +
          geom_line(data = adult_mortality.Predicted,
                          mapping = aes(x = temp, y = Predicted))
plotFit

