# clear environment
rm(list=ls())

# Soil moisture calib
library(FME)


#pars_calib <- c(theta.sat = 0.482, ra = 10)
# pars_calib <- c(vcmax25 = 60 , g1 = 9)
pars_calib <- c(vcmax25 = 60)
pars_up <- c(vcmax25 = 90) #, g1 = 13.5)
pars_low <- c(vcmax25 = 30) # , g1 = 4.5)

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
#gs:
# 0-0.25 molcm^-2s^-1
# +-50% is okay




##### First Calibration (1:20)
#Parameters:
 # Estimate Std. Error t value Pr(>|t|)
#vcmax25  83.8457    20.3836   4.113 4.34e-05 ***
#g1        5.8868     0.5968   9.864  < 2e-16 ***
  #---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.0001485 on 742 degrees of freedom

#Parameter correlation:
#  vcmax25      g1
#vcmax25  1.0000 -0.9761
#g1      -0.9761  1.0000
