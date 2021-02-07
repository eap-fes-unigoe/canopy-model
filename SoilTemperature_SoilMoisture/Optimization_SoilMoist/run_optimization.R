# Soil moisture calib

# parameter to be calibrated

# theta.sat:  Volumetric water content at saturation [m3 m-3]; value taken from Bonan p. 120 (soil defined as clay based on grain size of climate data)
# ra:         aerodynamic resistance (s m-1)
rm(list = ls())

parsfile <- "pars_soil.csv"

pars_calib <- c(b.sm = 11.4, epmod = 1, psi.n1 = -3.4)
pars_low <- c(b.sm = 5, epmod = 0.01, psi.n1 = -150)
pars_high <- c(b.sm = 13, epmod = 2, psi.n1 = -0.4)

source("setup_soilmoisture.R")
source("fun_soilmoisture.R")
source("fun_costmoisture.R")

myfit <- modFit(f = cost_soilmoisture, p = pars_calib, lower = pars_low, upper = pars_high)
summary(myfit)
