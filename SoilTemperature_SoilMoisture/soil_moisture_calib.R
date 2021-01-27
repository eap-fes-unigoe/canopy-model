# Soil moisture calib

pars_calib <- c(lambda=2260000, MWrat=0.622)

source("fun_soilmoisture.R")
source("fun_costmoisture.R")

myfit <- modFit(cost_soilmoisture, pars_calib)
