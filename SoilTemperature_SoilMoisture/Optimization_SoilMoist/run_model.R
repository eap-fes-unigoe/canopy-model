## Run model ##


# Run model with uncalibrated parameters

parsfile <- "pars_soil.csv"
source("setup_soilmoisture.R")
source("fun_soilmoisture.R")

output <- get_theta_soil(input = input, pars = pars, initial_state = initial_state)


# Run model choosing calibrated parameter file

pars_soil_calib1[2,2] <- 9.85
pars_soil_calib1[33,2] <- 0.6

parsfile <- "pars_soil_calib1.csv"
source("setup_soilmoisture.R")
source("fun_soilmoisture.R")

output_cal <- get_theta_soil(input = input, pars = pars, initial_state = initial_state)
