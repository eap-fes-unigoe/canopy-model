### Setup
#  rm(list=ls())

dt = 3600 # model time step of one hour

## Load parameters and adjust units ----
source("setup_parameters.R")

## Load and prepare input ----
source("setup_sitedata.R")

## Load functions ----
#source("fun_calc_Cpools.R")
source("fun_calc_radiative_transfer.R")
source("photosynthesis_stomatalconductance/calc_fun_Photosynthesis_StomatalConductance.R")

## Load initial state ----
initial_state <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Create output dataframe. Copy of initial_state but more variables can be added.
out <- initial_state

# Source setup scripts for different model components
source("photosynthesis_stomatalconductance/setup_Photosynthesis_StomatalConductance.R")
