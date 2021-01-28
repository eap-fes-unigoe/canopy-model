for(i in 1:length(mydata$TIMESTAMP_START)){
  # Leaf temperature and energy fluxes
  
  # --- Latent heat of vaporization (J/mol)
  
  lambda_val<- latvap ((atmo[i,"tair"]-tfrz), mmh2o)
  
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
    
    satvap_out <- satvap (flux[i,"tleaf"]-tfrz)
    esat <- satvap_out[[1]]
    desat <- satvap_out[[2]]
    
    # Leaf conductance for water vapor (mol H2O/m2/s)
    
    gleaf <- flux[i,"gs"] * flux[i,"gbw"]/ (flux[i,"gs"] + flux[i,"gbw"])
    
    # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
    
    flux[i,"lwrad"] <- 2 * emiss * sigma* flux[i,"tleaf"]^4
    dlwrad <- 8 * emiss * sigma * flux[i,"tleaf"]^3
    
    # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
    
    flux[i,"shflx"] <- 2 * cpair * (flux[i,"tleaf"] - atmo[i,"tair"] )* flux[i,"gbh"]
    dshflx <- 2 * cpair * flux[i,"gbh"]
    
    # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
    
    flux[i,"lhflx"] <- lambda_val / atmo[i,"patm"] * (esat - atmo[i,"eair"]) * gleaf
    dlhflx <- lambda_val / atmo[i,"patm"] * desat * gleaf
    
    
    # Energy balance (W/m2) and temperature derivative (W/m2/K)
    
    f0 <- flux[i,"qa"] - flux[i,"lwrad"] - flux[i,"shflx"] - flux[i,"lhflx"]
    df0 <- -dlwrad - dshflx - dlhflx
    
    # Change in leaf temperature
    
    dtleaf <- -f0 / df0
    
    # Update leaf temperature
    
    flux[i,"tleaf"] <- flux[i,"tleaf"] + dtleaf
    
    
  }
  
  # --- Net radiation
  
  flux[i,"rnet"] <- flux[i,"qa"] - flux[i,"lwrad"]
  
  
  # --- Error check
  
  err <- flux[i,"rnet"] - flux[i,"shflx"] - flux[i,"lhflx"]
  if (abs(err) > f0_max){
    cat('err  = ', err, '\n',
        'qa  = ',flux[i,"qa"], '\n',
        'lwrad  = ',flux[i,"lwrad"], '\n',
        'sh  = ',flux[i,"shflx"], '\n',
        'lh  = ',flux[i,"lhflx"])
    stop ('LeafTemperature error')
  }
  
  # Water vapor flux: W/m2 -> mol H2O/m2/s
  
  flux[i,"etflx"] <- flux[i,"lhflx"] / lambda_val
  
  
  #print for every loop
  print(paste0("gleaf: ", gleaf[1]))
  print(paste("lambda_val:", lambda_val[1]))
  print(paste("esat:", esat[1]))
  print(paste("eair:", atmo[i,"eair"][1]))
  print(paste0("lhflx: ", flux[i,"lhflx"][1]))
  print(paste0("tleaf: ", flux[i,"tleaf"][1]))
  print(paste0("tair: ", atmo[i,"tair"][1]))
}