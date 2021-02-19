# fun_cost_soiltemperature.R


cost_soiltemperature1 <- function(pars_calib, params = pars) {
  print(pars_calib)
  #browser()
  for(i in names(pars_calib)) {params[[i]] <- pars_calib[[i]]}
  
  output <- get_soiltemp(input = input, initial_state = initial_state, pars = params)
  
  # Calculate residuals
  resid <- output$Tsoil - fluxes$tsoil
  resid <- resid[!is.na(resid)]
  
  return(resid)

}
