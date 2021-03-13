# clear environment
rm(list=ls())

# Soil moisture calib
library(FME)


#pars_calib <- c(theta.sat = 0.482, ra = 10)
pars_calib <- c(vcmax25 = 60 , g1 = 9)
#pars_calib <- c(vcmax25 = 60)
pars_up <- c(vcmax25 = 80, g1 = 10)
pars_low <- c(vcmax25 = 40, g1 = 8)

source("photosynthesis_stomatalconductance/setup_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/fun_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/fun_costphoto.R")

o = 0

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
#started roughly 13:30 for one wwek
#done before 17:17
summary(myfit)


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

# one week, only vcmax25, including radiation and LAI, calculation time < 4h
#Parameters:
#  Estimate Std. Error t value Pr(>|t|)
#vcmax25  30.0000     0.7671   39.11   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.0005159 on 743 degrees of freedom

#Parameter correlation:
#  vcmax25
#vcmax25       1


# start: roughly 11:00 ?
# end  after <24 hours

#Parameters:
#  Estimate Std. Error t value Pr(>|t|)
#vcmax25  50.0000     4.0596  12.316   <2e-16 ***
#  g1        7.0000     0.7319   9.565   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.001157 on 742 degrees of freedom

#Parameter correlation:
#  vcmax25      g1
#vcmax25  1.0000 -0.9028
#g1      -0.9028  1.0000

#run on 12.3. ~13:00 - before 13.3. ~8:00
#over whole month

#Parameters:
#  Estimate Std. Error t value Pr(>|t|)
#vcmax25  40.0000     2.7862  14.357   <2e-16 ***
#  g1        8.0000     0.9337   8.568   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.0009061 on 742 degrees of freedom

#Parameter correlation:
#  vcmax25      g1
#vcmax25  1.0000 -0.9009
#g1      -0.9009  1.0000
