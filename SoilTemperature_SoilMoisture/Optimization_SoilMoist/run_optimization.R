# Soil moisture calib


rm(list = ls())

parsfile <- "pars_soil.csv"

pars_calib <- c(b.sm = 11.4, theta.sat = 0.48)
pars_low <- c(b.sm = 5, theta.sat = 0.3)
pars_high <- c(b.sm = 13, theta.sat = 0.6)

source("setup_soilmoisture.R")
source("fun_soilmoisture.R")
source("fun_costmoisture.R")

myfit <- modFit(f = cost_soilmoisture, p = pars_calib, lower = pars_low, upper = pars_high)
summary(myfit)
