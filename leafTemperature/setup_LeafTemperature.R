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



