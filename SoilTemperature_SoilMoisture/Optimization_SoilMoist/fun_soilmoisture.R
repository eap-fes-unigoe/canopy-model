# Function calculating soil moisture changes

get_theta_soil <- function(input, pars, initial_state) {

  list2env(pars, envir = environment())
  
  # Getting variables used
  theta.in <- initial_state$theta
  prec <- input$p
  Rh <- input$rh
  temp <- input$tair - 273.15
  time <- input$time
  gs <- fluxes$g
  rn <- input$Rn
  
  # calculations
  ps <- 1 - (BD/PD)       # soil pore space [unitless]: bulk density = 1200 kg m-3 (climate data), particle density = 2650 kg m-3 (literature)
  V <- 1 * 1 * SD         # volume of soil layer [m3]
  
  # for evaporation
  gamma <- (cp * 101.325) / (lambda * MWrat)  # psycrometric constant for potential evaporation [kPa K-1]
  # for drainage
  # b.min <- 2.91 + 0.159 * (clay * 100)
  # f <- (SOC / (BD * SD)) * 1.72        # soil organic matter fraction; f = (%C * 1.72)/100
  # b.sm <- (1 - f) * b.min + f * b.om
  
  sps <- Theta.sat * SD
  
  # equations

  # evaporation from soil
  # Penman-Monteith potential evapotranspiration

  # Ep = (Δ(Rn – Gs) + ρ cp(es – ea)/ra) / (λ(Δ+γ))

  # where:

  # Ep: potential evapotranspiration (kg m−2 s−1)
  # Δ: slope of the es to T curve = 4098 * (0.6108 * exp( 17.27 * T / (T + 237.3))) / (T + 237.3)^2 (kPa ºC-1) (T = Tsoil?; T in Celsius!)
  # rn: net radiation (J m-2 s-1) (from radiation model)
  # gs: ground heat flux (J m-2 s-1) (from soil temperature model)
  # cp: specific heat of air (J kg-1)
  # ρ: the air density (kg m−3)
  # es: air saturation vapor pressure (kPa)
  # ea: air actual vapour pressure (kPa)
  # ra: aerodynamic resistance (s m-1) --> assumed to be 10
  # γ: psycrometric constant (kPa ◦C−1) --> has to be calculated, but then we assume it to be constant
  # γ = (cp * air_pressure) / (λ * MWrat)
  # λ: latent heat of vaporization (J kg−1)


  # Actual soil evaporation (adapted from Aydin et. al 2005)

  # Ea = Ep * ( (log[WP] – log[WPa]) / (log[WPfc] – log[WPa]) ) / (Vsoil/BD)

  # WP = soil water potential
  # WPa = water potential of air (-100MPa (average value))
  # WPfc = soil water potential at field capacity



  # drainage

  # drain.t = -(k / SD )*(psi - psi.n1) - k (equation 8.27 from Bonan p. 125 )
  # k: hydraulic conductivity  [m s-1]
  # SD: soil layer thickness  [m]
  # psi: matric potential of soil  [m]
  # psi.n1: matric potential of soil beneath soil layer [m]
  # psi.n1 <- psi.sat * (s^-B) (equation from CLM4.5 p. 172)
  # s <- 0.5 ((theta.sat + theta)/theta.sat) [unitless]
  # B <- (1 - f) * B.min + f * B.om  [unitless]
  # B.min <- 2.91 + 0.159 * clay  [unitless]
  # B.om <- 2.7  [unitless]
  # f <- (SOC(kg m-2) / (BD * SD)) * 1.72    # soil organic matter fraction; f = %C * 1.72
  # SOC(kg/m-2) = SOC (%)× BD (kg/m3)× SD (m) x 1000
  # where,  SOC - Concentration of soil organic carbon (%);   BD - Bulk density (kg/m3); SD-   soil sampling depth (m)


  # output variables

  theta <- rep(NA, length(time))    # soil moisture [m3 m-3]
  drain <- rep(NA, length(time))    # drainage [m s-1]
  runoff <- rep(NA, length(time))   # runoff [m s-1]
  k <- rep(NA, length(time))        # hydraulic conductivity [m s-1]
  evap <- rep(NA, length(time))     # evaporation [m s-1]
  psi <- rep(NA, length(time))      # matric potential [m]
  s <- rep(NA, length(time))        # coefficient for drainage
  # psi.n1 <- rep(NA, length(time))   # matric potential of soil beneath soil layer [m]


  # Iterative calculations over time

  for(t in time) {
    
    # if(t==1) browser()
    # print(t)

    # first water content is taken from climate data, then theta from previous time step is taken for calculation
    # theta.in is the measured soil water content (averaged for the whole soil) at the start of the time series
    if(t == 1) {theta.t <- theta.in} else {theta.t <- theta[t-1]}

    # precipitation is taken from climate data
    prec.t <- prec[t]  # [m3 dt-1]
    
    rn.t <- rn[t]
    gs.t <- gs[t]
    temp.t <- temp[t]

    # transpiration data is taken from Leaf Temperature Model
    # trans.t <- trans[t]

    # evaporation
    # Calculating potential evaporation
    delta <- 4098 * (0.6108 * exp( 17.27 * temp.t / (temp.t + 237.3))) / (temp.t + 237.3)^2  # (kPa K-1) temp must be in Celsius
    es <- 0.6108 * exp(17.27* temp.t / (temp.t + 237.3)) # (kPa)
    ea <- es * Rh[t] # (kPa)
    Ep <- (delta * (rn.t - gs.t) + d_air * cp * (es - ea) / ra) / (lambda * (delta + gamma)) # (kg m−2 s−1)
    Ep <- Ep / 1000 # transform units to m3 m-2 s-1

    # Calculating soil water potential
    psi.t <- psi.sat * (theta.t / theta.sat)^-b.sm   # (m)
    if(psi.t < psi.a) {psi.t <- psi.a} 

    # Calculating actual evaporation
    evap.t <- epmod * (Ep * ( (log(-psi.t) - log(-psi.a)) / (log(-psi.ep) - log(-psi.a)) ))  # (m s-1)

    # hydraulic conductivity
    k.t <- k.sat * ((theta.t/theta.sat)^(2*b.sm+3))  # (m s-1)

    # drainage
    
    # Calculating psi for soil beneath soil layer
    # s.t <- 0.5 * ((theta.sat + theta.t) / theta.sat)
    # if(s.t < 0.01) {s.t <- 0.01}; if(s.t > 1) {s.t <- 1}
    # psi.n1.t <- psi.sat * (s.t^-B)  # matric potential for layer N+1 (layer beneath layer N) -> equation taken from CLM4.5

    # Calculating drainage
    # drain.t <- - (k.t / SD) * (psi.t - psi.n1.t) - k.t
    drain.t <- (k.t / SD / 2) * (psi.t - psi.n1) - k.t # new approach using psi.n1 as parameter (F.)
    # if(drain.t > x) drain.t <- x
    
    theta.l <- theta.t * SD
    
    # theta (water content) is current water content plus infiltration minus drainage
    theta.l <- theta.l - (evap.t + drain.t) * dt  # multiplication with dt to get infiltration/drainage volume for model time step
    if(theta.l > sps) {theta.l <- sps}; if(theta.l < 0.03) {theta.l <- 0.03} # making sure there are no impossible results
    
    # Precipitation and runoff as excess water
    theta.l <- theta.l + prec.t # only precipitation can become running
    if(theta.l > sps) {
      runoff.t <- theta.l - sps      # (m3 dt-1)
      theta.l <- sps
    } else {runoff.t <- 0}
    

    
    theta.t <- theta.l / SD

    theta[t] <- theta.t
    runoff[t] <- runoff.t
    k[t] <- k.t
    evap[t] <- evap.t
    drain[t] <- drain.t
    psi[t] <- psi.t
  }

  out <- data.frame(theta, runoff, k, evap, drain, psi, psi.n1)
  out$swc <- out$theta
  return(out)

}
