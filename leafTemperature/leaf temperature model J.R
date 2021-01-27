# install.packages("dplyr")
library(dplyr)

setwd("D:/Kathina/FES/2020-21 WS/Eco-Atmo Processes/Modelling/data")

#obtain data from the Hainich forest
input <-read.csv("Hainch_2002_input.csv",header=TRUE, sep=";", na.strings="NA", dec=",")

# select: air temperature (?C), air pressure (mb), vapor pressure of air (kPa)
# wind speed (m s-1), incoming shortwave radiation (W/m2)
myinput <- input%>%select(DOY,hour,Tair...C.,pressure..mb.,Vpact..kPa.,vhor..m.s.,SWDR..W.m2.,ST005...C.)

#change units from kPa to Pa and ?C to K
myinput<- myinput%>% 
  mutate(time = paste0(DOY, "_", hour))%>%   #create time column
  mutate(tair = Tair...C.+273.15)%>%         #temperature in K
  mutate(patm = pressure..mb.*100)%>%        #air pressure in Pa
  mutate(eair = Vpact..kPa.*1000)%>%         #vapor pressure of air in Pa
  mutate(u = vhor..m.s.)%>%                  #wind speed m/s
  mutate(SW = SWDR..W.m2.)%>%                #incoming shortwave radiation W/m2
  mutate(Ts = ST005...C.+273.15)%>%          #soil temperature (K)
  select(time,tair,patm,eair,u,SW,Ts)
  
#atmospheric parameters
atmo <- myinput%>%
    select(time,patm,tair,eair)

#physical constants
cpair <- 29.2                           # Specific heat of air at constant pressure (J/mol/K)
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(cpair,tfrz,mmh2o,sigma)


#leaf propperties
emiss <- 0.96                        #leaf emissivity (%)
leaf <- data.frame(emiss)


#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4a  (reference: Alexander Knohl)
LW <- runif(8760, min=200, max=400) # randomly created longwave radiation between 200-400 W/m2
LW <- 350
qa<- myinput%>%
  select(time,Ts,SW)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = 0.85*SW + LW + LW_S)%>% # radiative forcing
  select(time,qa)


### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection 
# gbh = (Nu*??m*Dh)/dl 

# required parameters:
# pm         #molar density (mol m-3) 
#ideal gas law: p = atmo$patm/R*atmo$tair   gas constant R: 0.167226 (J/Kg K)
# dl         #leaf dimension characteristic length (cm)
# Dh         #diffusivity of heat Dh (m^2 s-1)
# Nu         #Nusselt number for forced conv. 
#             Nu = 0,66* Pr^0,33* Re^0,5
# Pr         #Prandtl number (molecular diffusion properties of the fluid)
#             Pr = ??/Dh
# Re         #Reynolds number 
#             Re = u*dl/??
# u          #wind speed (M s-1)

# constants 
dl <- 0.1    #leaf dimension characteristic length (m)

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0?C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0?C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s-1) at 0?C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s-1) at 0?C
R <-  8.314        # gas constant, m^3*Pa*mol-1*K-1
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0?C (Mm? s-1)

conduct <- myinput%>%
  select(time,tair,patm,u)%>%
  mutate(pm = patm/(R*tair))%>%           # molar density (mol m-3) according to ideal gas law n=PV/RT, pm (n/volume, mol m-3)= P(Pa)/ R(m^3*Pa/mol*K)*T(K) 
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))%>%      # diffusivity of momentum (m^2 s-1)
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)%>%            # Boundary layer conductance for heat (mol/m2 leaf/s)
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)%>%          # Boundary layer conductance for heat (mol/m2 leaf/s)
  mutate(DW = DW0*(P0/patm)*(tair/T0))%>% # Molecular diffusivity of mass H2o (Mm? s-1)
  mutate(Sc = v/DW)%>%                    #Sherwood number
  mutate(gbw = 0.036*Sc^0.33*Re^0.50)%>%  #boundary layer conductance for water vapor (mol m-2 s-1) 
  select(time,gbh,gbw)

#  predefine leaf temperature
leaftemp<- myinput%>%
  select(time,tair)%>%
  mutate(tleaf = tair)               

flux <- merge(qa, conduct, by="time")
flux <- merge(flux, leaftemp, by="time")

#predefine fluxes from leaf temperature
flux$rnet<-rep(0,length(flux$time))   #Leaf net radiation (W/m2 leaf)
flux$lwrad<-rep(0,length(flux$time))  #Longwave radiation emitted from leaf (W/m2 leaf)
flux$shflx<-rep(0,length(flux$time))  #Leaf sensible heat flux (W/m2 leaf)
flux$lhflx<-rep(0,length(flux$time))  #Leaf latent heat flux (W/m2 leaf)
flux$etflx<-rep(0,length(flux$time))  #Leaf transpiration flux (mol H2O/m2 leaf/s)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)  
flux$gs <-  0.1

#function of Saturation vapor pressure and temperature derivative -- satvap()
satvap <- function (tc) {   
  # --- For water vapor (temperature range is 0C to 100C)  
  a0 =  6.11213476;        b0 =  0.444017302; 
  a1 =  0.444007856;       b1 =  0.286064092e-01;
  a2 =  0.143064234e-01;   b2 =  0.794683137e-03;
  a3 =  0.264461437e-03;   b3 =  0.121211669e-04; 
  a4 =  0.305903558e-05;   b4 =  0.103354611e-06; 
  a5 =  0.196237241e-07;   b5 =  0.404125005e-09; 
  a6 =  0.892344772e-10;   b6 = -0.788037859e-12; 
  a7 = -0.373208410e-12;   b7 = -0.114596802e-13;
  a8 =  0.209339997e-15;   b8 =  0.381294516e-16;  
  # --- For ice (temperature range is -75C to 0C)  
  c0 =  6.11123516;        d0 =  0.503277922; 
  c1 =  0.503109514;       d1 =  0.377289173e-01; 
  c2 =  0.188369801e-01;   d2 =  0.126801703e-02; 
  c3 =  0.420547422e-03;   d3 =  0.249468427e-04; 
  c4 =  0.614396778e-05;   d4 =  0.313703411e-06; 
  c5 =  0.602780717e-07;   d5 =  0.257180651e-08; 
  c6 =  0.387940929e-09;   d6 =  0.133268878e-10; 
  c7 =  0.149436277e-11;   d7 =  0.394116744e-13; 
  c8 =  0.262655803e-14;   d8 =  0.498070196e-16;    
  
  # --- Limit temperature to -75C to 100C
  
  tc = min(tc, 100);
  tc = max(tc, -75);
  
  #--- Saturation vapor pressure (esat, mb) and derivative (desat, mb)  
  if (tc >= 0){ 
    esat  = a0 + tc*(a1 + tc*(a2 + tc*(a3 + tc*(a4 + tc*(a5 + tc*(a6 + tc*(a7 + tc*a8))))))); 
    desat = b0 + tc*(b1 + tc*(b2 + tc*(b3 + tc*(b4 + tc*(b5 + tc*(b6 + tc*(b7 + tc*b8))))))); 
  } else {
    esat  = c0 + tc*(c1 + tc*(c2 + tc*(c3 + tc*(c4 + tc*(c5 + tc*(c6 + tc*(c7 + tc*c8))))))); 
    desat = d0 + tc*(d1 + tc*(d2 + tc*(d3 + tc*(d4 + tc*(d5 + tc*(d6 + tc*(d7 + tc*d8)))))));
  }
  esat  = esat  * 100 
  desat = desat * 100  
  return(list(esat, desat))
}


#function of Latent heat of vaporization -- latvap()
latvap<-function (tc, mmh2o){   ### same question here...
  # Latent heat of vaporization (J/mol) at temperature tc (degC)  
  val<- 2501.6 - 2.3773 * tc # Latent heat of vaporization (J/g) 
  val<- val * 1000           # Convert from J/g to J/kg 
  val<- val * mmh2o          # Convert from J/kg to J/mol  
  return(val) 
}

str(physcon)
str(atmo)
str(leaf)
str(flux)

for(i in 1:length(myinput$time)){
  # Leaf temperature and energy fluxes
  
  # ------------------------------------------------------
  # Input
  #   physcon.tfrz     ! Freezing point of water (K)
  #   physcon.mmh2o    ! Molecular mass of water (kg/mol)
  #   physcon.sigma    ! Stefan-Boltzmann constant (W/m2/K4)
  #   physcon.cpair    ! Specific heat of air at constant pressure (representable value: 29.2 J/mol/K)
  #   atmo.patm        ! Atmospheric pressure (Pa)
  #   atmo.tair        ! Air temperature (K)  
  #   atmos.eair       ! Vapor pressure of air (Pa)
  #   leaf.emiss       ! Leaf emissivity
  #   flux.gbh         ! Leaf boundary layer conductance, heat (mol/m2 leaf/s)
  #   flux.gbw         ! Leaf boundary layer conductance, H2O (mol H2O/m2 leaf/s)
  #   flux.gs          ! Leaf stomatal conductance (mol H2O/m2 leaf/s)
  #   flux.qa          ! Leaf radiative forcing (W/m2 leaf)
  #
  # Input/ouput
  #   flux.tleaf       ! Leaf temperature (K)
  #
  # Output
  #   flux.rnet        ! Leaf net radiation (W/m2 leaf)
  #   flux.lwrad       ! Longwave radiation emitted from leaf (W/m2 leaf)
  #   flux.shflx       ! Leaf sensible heat flux (W/m2 leaf)
  #   flux.lhflx       ! Leaf latent heat flux (W/m2 leaf)
  #   flux.etflx       ! Leaf transpiration flux (mol H2O/m2 leaf/s)
  # ------------------------------------------------------
  
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

plot(flux$tleaf, ylab = "leaf temperature (K)")
plot(flux$tleaf-flux$tair,ylab = "leaf-air temperature deficit (K)")
plot(flux$rnet, ylab = "Leaf net radiation (W/m^2 leaf)")
plot(flux$lwrad, ylab = "Longwave radiation emitted from leaf (W/m2 leaf)")
plot(flux$shflx, ylab = "Leaf sensible heat flux (W/m2 leaf)")
plot(flux$lhflx, ylab = "Leaf latent heat flux (W/m2 leaf)")
plot(flux$etflx, ylab = " Leaf transpiration flux (mol H2O/m2 leaf/s)")
plot(flux$tleaf~atmo$tair, ylab="Leaf temperature changes with air temperature") 
plot(flux$tleaf~flux$rnet)

