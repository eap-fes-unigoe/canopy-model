# Soil temperature calibration
rm(list = ls())

parsfile <- "pars_soils.csv"

pars_calib <- c(Cvsoil=1.9, Ko=3, Tsubsoil = 290)
pars_low   <- c(Cvsoil=1.0, Ko=1, Tsubsoil = 280)
pars_high  <- c(Cvsoil=4.0, Ko=4, Tsubsoil = 300)
source("setup_soiltemp.R")
source("fun_soiltemperature.R")
source("fun_costtemp.R")

myfit <- modFit(f = cost_soiltemperature1,p = pars_calib, lower = pars_low, upper = pars_high)
plot(myfit)

summary(myfit)




