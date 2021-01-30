# Function calculating soil moisture changes

get_theta_soil <- function(input, param_mincalib, theta.in, pars_calib) {


  # equations

  # evaporation from soil
  # Penman-Monteith potential evapotranspiration

  # Ep = (Δ(Rn – Gs) + ρ cp(es – ea)/ra) / (λ(Δ+γ))

  # where:

  # Ep: potential evapotranspiration (kg m−2 s−1)
  # Δ: slope of the es to T curve = 4098 * (0.6108 * exp( 17.27 * T / (T + 237.3))) / (T + 237.3)^2 (kPa ºC-1) (T in K?)
  # rn: net radiation (J m-2 s-1) (from radiation model)
  # gs: ground heat flux (J m-2 s-1) (from soil temperature model)
  # cp: specific heat of air (J kg-1)
  # ρair: the air density (kg m−3)
  # es: air saturation vapor pressure (kPa)
  # ea: air actual vapour pressure (kPa)
  # ra: aerodynamic resistance (s m-1) --> assumed to be 10
  # γ: psycrometric constant (kPa ◦C−1) --> has to be calculated, but then we assume it to be constant
  # γ = (cp * p) / (λ * MWrat)
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
  inf <- rep(NA, length(time))      # infiltration [m s-1]
  evap <- rep(NA, length(time))     # evaporation [m s-1]
  psi <- rep(NA, length(time))      # matric potential [m]
  s <- rep(NA, length(time))        # coefficient for drainage
  psi.n1 <- rep(NA, length(time))   # matric potential of soil beneath soil layer [m]



  # Iterative calculations over time

  for(t in time) {

    # first water content is taken from climate data, then theta from previous time step is taken for calculation
    # theta.in is the mean water content on 01.01.2017 00.00-00.30 over all 'layers'
    if(t == 1) {theta.t <- theta.in} else {theta.t <- theta[t-1]}

    # precipitation is taken from climate data
    p.t <- p[t]  # [m h-1]

    # transpiration data is taken from Leaf Temperature Model
    # trans.t <- trans[t]

    # evaporation
    # Calculating potential evaporation
    delta <- 4098 * (0.6108*10^3 * exp( 17.27 * tair[t] / (tair[t] + 237.3))) / (tair[t] + 237.3)^2  # (Pa K-1)
    es <- 0.6108*10^3 * exp(17.27* tair[t] / (tair[t] + 237.3)) # (Pa)
    ea <- es * rh[t] # (Pa)
    Ep <- (delta * (rn - gs) + pair * cp * (es - ea) / ra) / (lambda * (delta + gamma)) # (kg m−2 s−1)

    # Calculating soil water potential
    psi.t <- psi.sat * (theta.t / theta.sat)^-b   # (m)

    # Calculating actual evaporation
    evap.t <- (Ep[t] * ( (log(psi.t) - log(psi.a)) / (log(psi.fc) - log(psi.a)) )) * ((1 - theta.sat) / BD) *3600  # (m h-1)

    # runoff is the excess water; if runoff is negative, no runoff occurs
    runoff.t <- ((theta.t - ps) * V) / 1000  # (m h-1)
    if (runoff.t < 0) {runoff.t <- 0} else next

    # infiltration (without infiltration capacity -> if there's space, water will infiltrate)
    inf.t <- p.t - runoff.t - evap.t  # - trans.t  # (m h-1)
    if (inf.t < 0) {inf.t <- 0} else next

    # hydraulic conductivity
    k.t <- -k.sat * ((theta.t/theta.sat)^(2*b+3))  # (m h-1)

    # drainage
    # Calculating psi for soil beneath soil layer
    s.t <- 0.5 * ((theta.sat + theta.t) / theta.sat)
    psi.n1.t <- psi.sat * (s.t^-B)  # matric potential for layer N+1 (layer beneath layer N) -> equation taken from CLM4.5

    # Calculating drainage
    drain.t <- (- (k.t / SD) * (psi.t - psi.n1.t) - k.t)   # (m h-1)

    # theta (water content) is current water content plus infiltration minus drainage
    theta.t <- theta.t + (inf.t - drain.t)
    if(theta.t > ps) {theta.t <- ps} else {theta.t == theta.t}

    theta[t] <- theta.t
    runoff[t] <- runoff.t
    k[t] <- k.t
    evap[t] <- evap.t
    drain[t] <- drain.t
    psi[t] <- psi.t
    s[t] <- s.t
    psi.n1[t] <- psi.n1.t
    inf[t] <- inf.t

  }

  return(data.frame(theta, runoff, k, evap, drain, psi, psi.n1, inf))

}

evap

output <- get_theta_soil(input = input, param_mincalib = param_mincalib, pars_calib = pars_calib, theta.in = fluxes$swc)

plot(fluxes$time, output$inf)
plot(fluxes$time, output$theta)
plot(fluxes$time, output$drain)
