# fun_cost_soilmoisture.R

cost_soilmoisture <- function(pars_calib) {
  output <- get_theta_soil(input = input, param_mincalib = param_mincalib, pars_calib = pars_calib, theta.in = fluxes$swc)

  resid <- output$theta - fluxes$swc
  resid <- resid[!is.na(resid)]
}

# run function

cost_soilmoisture(pars_calib = pars_calib)
