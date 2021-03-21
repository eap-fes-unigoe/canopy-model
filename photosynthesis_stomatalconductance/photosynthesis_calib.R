# photosynthesis and stomatal conductance calibration script

rm(list=ls()) # clear environment

library(FME) # load requirded FME package

# setting parameters to be calibrated with upper and lower limits
pars_calib <- c(vcmax25 = 60 , g1 = 9)
#pars_calib <- c(vcmax25 = 60)
pars_up <- c(vcmax25 = 80, g1 = 10)
pars_low <- c(vcmax25 = 40, g1 = 8)

source("photosynthesis_stomatalconductance/setup_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/fun_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/fun_costphoto.R")


myfit <- modFit(f = cost_photo, p = pars_calib, lower = pars_low , upper = pars_up )
#myfit <- modFit(f = cost_photo, p = pars_calib)

#exp(coef(myfit))
#exp(myfit$par)
#myfit$par
coef(myfit)
plot(myfit)
#myfit$ssr

summary(myfit)
