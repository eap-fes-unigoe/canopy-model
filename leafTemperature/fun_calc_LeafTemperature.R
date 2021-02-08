LeafTemperature <- function(pars, state_last, vars_LeafTemperature){
  
  # --- Latent heat of vaporization (J/mol)
  
  lambda_val <- latvap ((atmos$tair-pars$tfrz), pars$mmh2o)
  
  # --- Newton-Raphson iteration until leaf energy balance is less than
  # f0_max or to niter_max iterations
  
  niter <- 0 # Number of iterations
  f0 <- 1e36 # Leaf energy balance (W/m2)
  
  niter_max <- 100 # Maximum number of iterations
  f0_max <- 1e-06  # Maximum energy imbalance (W/m2)
  
  while ((niter < niter_max) && (abs(f0) > f0_max)){
    
    # Increment iteration counter
    
    niter <- niter + 1
    
    # Saturation vapor pressure (Pa) and temperature derivative (Pa/K)
    
    satvap_out <- satvap (flux$tleaf-pars$tfrz)
    esat <- satvap_out[[1]]
    desat <- satvap_out[[2]];
    
    # Leaf conductance for water vapor (mol H2O/m2/s)
    
    gleaf <- gs * flux$gbw / (gs + flux$gbw)
    
    # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
    
    flux$lwrad <- 2 * leaf$emiss * pars$sigma * flux$tleaf^4
    dlwrad <- 8 * leaf$emiss * pars$sigma * flux$tleaf^3
    
    # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
    
    flux$shflx <- 2 * atmos$cpair * (flux$tleaf - atmos$tair) * flux$gbh
    dshflx <- 2 * atmos$cpair * flux$gbh
    
    # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
    
    flux$lhflx <- lambda_val / atmos$patm * (esat - atmos$eair) * gleaf
    dlhflx <- lambda_val / atmos$patm * desat * gleaf
    
    # Energy balance (W/m2) and temperature derivative (W/m2/K)
    
    f0 <- flux$qa - flux$lwrad - flux$shflx - flux$lhflx
    df0 <- -dlwrad - dshflx - dlhflx
    
    # Change in leaf temperature
    
    dtleaf <- -f0 / df0
    
    # Update leaf temperature
    
    flux$tleaf <- flux$tleaf + dtleaf
    
  }
  
  # --- Net radiation
  
  flux$rnet <- flux$qa - flux$lwrad
  
  
  # --- Error check
  
  err <- flux$rnet - flux$shflx - flux$lhflx
  if (abs(err) > f0_max){
    cat('err  = ', err, '\n',
        'qa  = ',flux$qa, '\n',
        'lwrad  = ',flux$lwrad, '\n',
        'sh  = ',flux$shflx, '\n',
        'lh  = ',flux$lhflx)
    stop ('LeafTemperature error')
  }
  
  # Water vapor flux: W/m2 -> mol H2O/m2/s
  
  flux$etflx <- flux$lhflx / lambda_val
  
  return(list(flux$tleaf, flux$rnet, flux$lwrad, flux$shflx, flux$lhflx, flux$etflx))
}