### Main

## This is the main script
## It runs all steps to carry out the simulation. This includes:
## 1. Loading packages, required functions, parameters, input data and initial state.
## 2. Initial calculations and definition of variables
## 3. Running a for loop that calculates fluxes and stocks over the simulation time
## 4. Writing out model output

## Required packages

## Load parameter file ----
pars <- read.csv("parameters.csv")

## Load functions ----


## Load input ----
input <- read.csv("data/Hainch_2002_input.csv")

## Load initial state ----
## This should be a dataframe with all the state variables and one row with initial values

# statevars <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Setting some time unit variables in unit seconds
dhour <- 3800
dday  <- dhour * 24
dyear <- dday * 365
dt    <- dhour # model time step

## Model run (for loop) ----
for(n in 1:length(time)) {

  # Calculate radiative transfer


  # Calculate soil hydrology


  # Calculate soil temperature


  # Calculate leaf temperature, photosynthesis and stomatal conductance


  # Calculate plant C pools, soil decomposition and soil C pools

}

# Write out output


