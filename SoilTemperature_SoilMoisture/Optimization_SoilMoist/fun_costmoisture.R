# fun_cost_soilmoisture.R

cost_soilmoisture <- function(pars_calib, params = pars) {
  
  print(pars_calib)
  # Calibrated pars replace default values
  for(i in names(pars_calib)) {params[[i]] <- pars_calib[[i]]}
  
  # Call the model function
  output <- get_theta_soil(input = input, pars = params, initial_state = initial_state)
  # browser()
  # Calculate residuals
  resid <- output$swc - fluxes$swc
  resid <- resid[!is.na(resid)]
  
  return(resid)
}
