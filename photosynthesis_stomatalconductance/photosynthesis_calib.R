# Soil moisture calib

library(FME)

#pars_calib <- c(theta.sat = 0.482, ra = 10)
pars_calib <- c(vcmax25 = 60 , g1 = 9)

source("setup_soilmoisture_calib.R")
source("fun_soilmoisture.R")
source("fun_costmoisture.R")

myfit <- modFit(cost_soilmoisture, pars_calib)
exp(coef(myfit))

# gs:
# 0-0.25 molcm^-2s^-1
# +-50% is okay
