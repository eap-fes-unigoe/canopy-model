### Main

## This is the main script
## It runs all steps to carry out the simulation. This includes:
## 1. Loading packages, required functions, parameters, input data and initial state.
## 2. Initial calculations and definition of variables
## 3. Running a for loop that calculates fluxes and stocks over the simulation time
## 4. Writing out model output
#testfun_photosynthesis = function()

photosynthesis_tests_loop <- function(met,state_last,pars,ps_sc) {


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
#source("fun_calc_radiative_transfer.R")
source("photosynthesis_stomatalconductance/calc_fun_Photosynthesis_StomatalConductance.R")

## Load initial state ----
## This should be a dataframe with all the state variables and one row with initial values

initial_state <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Create output dataframe. Copy of initial_state but more variables can be added.
out <- initial_state

# Source setup scripts for different model components
#source("setup_Cpools.R")
source("photosynthesis_stomatalconductance/setup_Photosynthesis_StomatalConductance.R")

# Setup progress bar
library(progress)
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(input$time),
                       clear = FALSE) # set to true to remove bar after finishing

## Model run (for loop) ----
for(n in 1:length(input$time)) {

  if(n==1) {state_last <- initial_state[1,]} else state_last <- out[(n-1),] # state variable values at previous time step
  met <- input[n,]
  #site <- fluxes[n,]

  # Calculate radiative transfer

  # This is a really temporary workaround, before we calculate the tsoil and tleaf from the other submodels
  #radiation_state <- list(t_leaf = met$tair, t_soil = met$tair)
 # radiation <- fun_calc_radiative_transfer(met, radiation_state, pars, dt)
 # out[n, names(radiation)] <- radiation

  # Calculate soil hydrology


  # Calculate soil temperatur


  # Calculate leaf temperature,


  # calculate photosynthesis and stomatal conductance

  # tleaf substitute
  state_last$tleaf <- met$tair
  state_last$gbw <- 0.702
  #ps_sc$leaftype <- 1 #sunlit leafs
  # put in out?
  photosynthesis_stomatalconductance <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,ps_sc)
  #state_last$PAR <- radiation$ic_sun
  #ps_sc$leaftype <- 2 #shaded leafs
  #photosynthesis_stomatalconductance <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,ps_sc)
  #photosynthesis_stomatalconductance$an <- photosynthesis_stomatalconductance$an * #LAI
  out[n, names(photosynthesis_stomatalconductance)] <- photosynthesis_stomatalconductance


  # Calculate plant C pools, soil decomposition and soil C pools
#  Cpools <- fun_calc_Cpools(pars, state_last, Cpools, vars_Cpools, fun_kmod_Ms, fun_kmod_Ts, site)
#  for(ipool in 1:length(names_Cpools)) {out[n, names_Cpools[ipool]] <- Cpools[ipool]}

  # update progress bar
  pb$tick()

}

rm(met, site, state_last, names_Cpools, ipool)
# Write out output
#write.csv(out, file="testoutput_calib_07_2018.csv")

}
