#Leaf temperature simulation setup
#for loop to simulate leaf temperature in denpendance of leaf energy balance
# Source: Bonan Ecosystem Modeling book, page 160

#create dataframes: atmo,leaf,flux
#atmospheric parameters

#select only necessary columns
# actual vapor pressure of air (Pa): eair = vpd/(1/rh -1)
atmo <- input%>%
  select(Date.Time,pa,tair,vpd,rh)%>%
  mutate(eair=vpd/(1/rh-1))%>%
  select(Date.Time,pa,tair,eair)

#leaf propperties
emiss <- 0.96                           #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = 85 % incoming shortwave + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4a (reference: Alexander Knohl)
qa<- merge(input,fluxes,by="Date.Time")%>%
  select(Date.Time,tsoil,sw_in,lw_in)%>%
  mutate(lw_s = 0.97*sigma*tsoil^4)%>% # Longwave radiation from soil
  mutate(qa = 0.85*sw_in + lw_in + lw_s)%>% # radiative forcing
  select(Date.Time,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection
### and boundary layer conductance for water vapor (mol m-2 s-1)

# constants needed for conductance & diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
dl <- 0.1    #leaf dimension characteristic length (cm)
T0<- 273.15  # Temperature 0?C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0?C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s-1) at 0?C
v0 <- 13.3*10^(-6)  # reference diffusivity of momentum v (m2 s-1) at 0?C
R <- 8.314          # gas constant, m^3*Pa*mol-1*K-1
DW0 <- 21.8*10^(-6) # Molecular diffusivity of mass H2o at 0?C (M m2 s-1)

conduct <- input%>%
  select(Date.Time,tair,pa,ws)%>%
  mutate(pm = pa/(R*tair))%>%           # molar density (mol m-3) according to ideal gas law n=PV/RT, pm (n/volume, mol m-3)= P(Pa)/ R(m^3*Pa/mol*K)*T(K) 
  mutate(Dh = Dh0*(P0/pa)*(tair/T0))%>% # diffusivity of heat (m2 s-1) 
  mutate(v = v0*(P0/pa)*(tair/T0))%>%      # diffusivity of momentum (m2 s-1)
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = ws*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)%>%            # Boundary layer conductance for heat (mol/m2 leaf/s)
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = ws*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)%>%          # Boundary layer conductance for heat (mol/m2 leaf/s)
  mutate(DW = DW0*(P0/pa)*(tair/T0))%>% # Molecular diffusivity of mass H2o (M m2 s-1)
  mutate(Sc = v/DW)%>%                    #Sherwood number
  mutate(gbw = 0.036*Sc^0.33*Re^0.50)%>%  #boundary layer conductance for water vapor (mol m-2 s-1) 
  select(Date.Time,gbh,gbw)

#  predefine leaf temperature
leaftemp<- input%>%
  select(Date.Time,tair)%>%
  mutate(tleaf = tair)%>%
  select(Date.Time,tleaf)

#merge all flux data in one dataframe
flux <- merge(qa, conduct, by="Date.Time")
flux <- merge(flux, leaftemp, by="Date.Time")

# Leaf stomatal conductance (mol H2O/m2 leaf/s)
flux$gs <- -0.1

#predefine fluxes from leaf temperature
flux$rnet<-rep(0,length(flux$Date.Time))   #Leaf net radiation (W/m2 leaf)
flux$lwrad<-rep(0,length(flux$Date.Time))  #Longwave radiation emitted from leaf (W/m2 leaf)
flux$shflx<-rep(0,length(flux$Date.Time))  #Leaf sensible heat flux (W/m2 leaf)
flux$lhflx<-rep(0,length(flux$Date.Time))  #Leaf latent heat flux (W/m2 leaf)
flux$etflx<-rep(0,length(flux$Date.Time))  #Leaf transpiration flux (mol H2O/m2 leaf/s)

#define variables Leaf Temperature
vars_LeafTemperature <- list(flux,atmo,leaf)


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


