# Setup soil moisture calibration

### This section is same as the start of the Main.R file ---------------
### It loads packages, defines time units, and sets up site data and parameters

## Required packages
library(dplyr)
library(FME)

# Setting some time unit variables in unit seconds ----
t_units <- list(hour  = 3600,
                halfh = 1800,
                day   = 86400,
                month = 2592000,
                year  = 31536000)
dt <- t_units$hour # delta time: model time step

## Load and prepare input ----
# required variables from measured data
# rh: relative humidity
# prec: precipitation
# temp.air: air temperature
source("setup_sitedata.R")

# example values for calibration
input$lw_out <- 0.95 * 5.670373e-8 * input$tair^4 # no data for lw_out, so making a rough approximation using tair
input$Rn <- input$sw_in + input$lw_in - fluxes$sw_out - input$lw_out

## Set initial values ----
initial_state <- list(theta=fluxes$swc[1])  # initial value for volumetric water content of soil [m3 m-3]

## Load parameters and adjust units ----
source("setup_parameters.R")
