# TsetseFly_PopulationDynamics
 
This is a reproduction of the code used for the paper
> <span xmlns:cc="http://creativecommons.org/ns#" property="cc:attributionName">Lord J.S., Hargrove J.W., Torr S.J. and Vale, G.A. (2018). Climate change and African trypanosomiasis vector populations in Zimbabwe's Zambezi Valley: a mathematical modelling study.
PLoS Medicine. https://doi.org/10.1371/journal.pmed.1002675.

I intend to add some code later to project the Tsetse fly population of the Kruger National park
however the majority of the work I have done is simply to better understand the work done by lord et al.

CountAndTemp.R reads in the temperature data and the tsetse fly count and combines them into
a useful database.

AdultMortalityAsFunctionOfTemp.R reads in adult mortality data and plots it along with the predicted mortality as a function of temp.

LarvipostionAsFunctionOfTemp.R Reads in larviposition data and plots it along with the predicted
larviposition as a function of temp.

PupalDurationAsAfunctionOfTemp.R Reads in pupal duration data and and plots it along with the predicted pupal duration as a function of temp.

PupalMortalityAsAFunctionOfTemp.R Reads in pupal mortality data and sets up a instant pupal mortality compared to temp data base, we then plot this with a predicted pupal mortality rate as a function of time.