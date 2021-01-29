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
shour  <- 3800
shalfh <- shour/2
sday   <- shour * 24
smonth <- sday * 30
syear  <- sday * 365
dt     <- shour # delta time: model time step

## Load parameters and adjust units ----
source("setup_parameters.R")

## Load and prepare input ----
source("setup_sitedata.R")

## Load functions ----
source("fun_calc_Cpools.R")

## Load initial state ----
## This should be a dataframe with all the state variables and one row with initial values

initial_state <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Create output dataframe. Copy of initial_state but more variables can be added.
out <- initial_state

# Source setup scripts for different model components
source("setup_Cpools.R")

## Model run (for loop) ----
for(n in 1:length(input$time)) {

  if(n==1) {state_last <- initial_state[1,]} else state_last <- out[(n-1),] # state variable values at previous time step
  met <- input[n,]
  site <- fluxes[n,]

  # Calculate radiative transfer


  # Calculate soil hydrology


  # Calculate soil temperature


  # Calculate leaf temperature, photosynthesis and stomatal conductance


  # Calculate plant C pools, soil decomposition and soil C pools
  Cpools <- fun_calc_Cpools(pars, state_last, Cpools, vars_Cpools, fun_kmod_Ms, fun_kmod_Ts, site)
  for(ipool in 1:length(names_Cpools)) {out[n, names_Cpools[ipool]] <- Cpools[ipool]}

}

# Write out output
# write.csv()

