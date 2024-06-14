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

                                        