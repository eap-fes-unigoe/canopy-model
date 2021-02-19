# Run model cho0sing parameter file

parsfile <- "pars_soils_calib1.csv"
source("setup_soiltemperature_calib.R")
source("fun_soiltemperature1.R")
Tsubsoil <- 283
output <- get_soiltemp(input = input, initial_state = initial_state, pars = pars)

plot(fluxes$tsoil, ylim = c(284, 292))
plot(output$Tsoil, ylim = c(284, 292))
