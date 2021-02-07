# Run model chsing parameter file

parsfile <- "pars_soil_calib1.csv"
source("setup_soilmoisture.R")
source("fun_soilmoisture.R")

output <- get_theta_soil(input = input, pars = pars, initial_state = initial_state)
plot(fluxes$swc, ylim = c(0.19, 0.26))
plot(output$swc, ylim = c(0.19, 0.26))
