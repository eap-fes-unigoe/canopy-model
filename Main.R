### Main

## This is the main script
## It runs all steps to carry out the simulation. This includes:
## 1. Loading packages, required functions, parameters, input data and initial state.
## 2. Initial calculations and definition of variables
## 3. Running a for loop that calculates fluxes and stocks over the simulation time
## 4. Writing out model output


##
rm(list=ls())

## Required packages

# Setting some time unit variables in unit seconds
t_units <- list(hour  = 3600,
                halfh = 1800,
                day   = 86400,
                month = 2592000,
                year  = 31536000)
dt <- t_units$hour # delta time: model time step

## Load parameters and adjust units ----
source("setup_parameters.R")

## Load and prepare input ----
source("setup_sitedata.R")

## Load functions ----
source("fun_calc_Cpools.R")
source("fun_calc_radiative_transfer.R")
source("photosynthesis_stomatalconductance/fun_calc_an_gs.R")
source("leafTemperature/fun_calc_LeafTemperature.R")

#function of Saturation vapor pressure and temperature derivative
source("satvap.R")
#function of Latent heat of vaporization
source("latvap.R")

## Load initial state ----
## This should be a dataframe with all the state variables and one row with initial values

initial_state <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Create output dataframe. Copy of initial_state but more variables can be added.
out <- initial_state

# Source setup scripts for different model components
source("setup_Cpools.R")
source("photosynthesis_stomatalconductance/setup_Photosynthesis_StomatalConductance.R")
source("leafTemperature/setup_LeafTemperature.R")

# Setup progress bar
library(progress)
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(input$time),
                       clear = FALSE) # set to true to remove bar after finishing

## Model run (for loop) ----
for(n in 1:length(input$time)) {

  if(n==1) {state_last <- initial_state[1,]} else state_last <- out[(n-1),] # state variable values at previous time step
  met <- input[n,]
  site <- fluxes[n,]

  # Calculate radiative transfer

  # This is a really temporary workaround, before we calculate the tsoil and tleaf from the other submodels
  radiation_state <- list(t_leaf = met$tair, t_soil = met$tair)
  radiation <- fun_calc_radiative_transfer(met, radiation_state, pars, dt)
  out[n, names(radiation)] <- radiation

  # Calculate soil hydrology


  # Calculate soil temperature


  # calculate photosynthesis an and stomatal conductance
  an_gs <- fun_calc_an_gs(met,state_last,pars,out[n,])
  out[n, names(an_gs)] <- an_gs

  # Calculate leaf temperature and latent and sensible heat fluxes
  Leafflux <- LeafTemperature(pars, state_last, vars_LeafTemperature)
  for(i in 1:length(flux$Date.Time)){out[n,flux$Date.Time[i]] <- flux[i]}

  # Calculate plant C pools, soil decomposition and soil C pools
  Cpools <- fun_calc_Cpools(pars, state_last, Cpools, vars_Cpools, fun_kmod_Ms, fun_kmod_Ts, site)
  for(ipool in 1:length(names_Cpools)) {out[n, names_Cpools[ipool]] <- Cpools[ipool]}

  # update progress bar
  pb$tick()

}

rm(met, site, state_last, names_Cpools, ipool)
# Write out output
# write.csv()
