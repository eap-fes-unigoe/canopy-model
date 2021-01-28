# Soil temperature calibration

pars_calib <- c( Cv=1984500)

source("setup_soiltemperature_calib.R")
source("fun_soiltemperature.R")
source("fun_costtemp.R")
library("FME")

myFit <- modFit(cost_soiltemperature, pars_calib)
