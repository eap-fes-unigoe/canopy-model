# Soil moisture calib

library(FME)

#pars_calib <- c(theta.sat = 0.482, ra = 10)
pars_calib <- c(vcmax25 = 60 , g1 = 9)
pars_up <- c(vcmax25 = 90 , g1 = 13.5)
pars_low <- c(vcmax25 = 30 , g1 = 4.5)

source("photosynthesis_stomatalconductance/setup_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/fun_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/fun_costphoto.R")

myfit <- modFit(cost_photo, pars_calib, lower = pars_low , upper = pars_up )
exp(coef(myfit))

# gs:
# 0-0.25 molcm^-2s^-1
# +-50% is okay
