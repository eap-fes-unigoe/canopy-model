#Leaf temperature simulation setup
#for loop to simulate leaf temperature in denpendance of leaf energy balance
# Source: Bonan Ecosystem Modeling book, page 160

# 1. Read Input data
library(dplyr)
input <-read.csv("data/Hainich_2018_input.csv",header=TRUE, sep=",", na.strings="NA", dec=".")
flux <-read.csv("data/Hainich_2018_fluxes.csv",header=TRUE, sep=",", na.strings="NA", dec=".")

# select: air temperature TA_F (?C), vapor pressure deficit VPD_F (hPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (?C)
# wind speed WS_F (m s-1), relative humidity RH 
input<- input%>%select(TIMESTAMP_START,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F,WS_F,RH)
flux<- flux%>%select(TIMESTAMP_START,TS_F_MDS_1)
mydata <- merge(input,flux, by="TIMESTAMP_START")

#2. change units from kPa to Pa and  degrees C to K
mydata<- mydata%>%
  mutate(tair = TA_F+273.15)%>%
  mutate(VPD_F = VPD_F*100)%>%
  mutate(patm = PA_F*1000)%>%
  mutate(u = WS_F)%>%
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F,RH)

#3. create dataframes physcon,atmo,leaf,flux
#atmospheric parameters

#select only necessary columns
# actual vapor pressure of air: avp = vpd/(1/rh -1)
atmo <- mydata%>%
  select(TIMESTAMP_START,patm,tair,VPD_F,RH)%>%
  mutate(eair=VPD_F/(1/RH-1))%>%
  select(TIMESTAMP_START,patm,tair,eair)

# physical constants
cpair <- 29.2                           # Specific heat of air at constant pressure (J/mol/K)
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(cpair,tfrz,mmh2o,sigma)

#leaf propperties
emiss <- 0.96                           #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4a (reference: Knohl)
qa<- mydata%>%
  select(TIMESTAMP_START,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection
### and boundary layer conductance for water vapor (mol m-2 s-1)

# constants
dl <- 0.1    #leaf dimension characteristic length (cm)

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0?C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0?C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s-1) at 0?C
v0 <- 13.3*10^(-6)  # reference diffusivity of momentum v (m2 s-1) at 0?C
R <- 8.314          # gas constant, m^3*Pa*mol-1*K-1
DW0 <- 21.8*10^(-6) # Molecular diffusivity of mass H2o at 0?C (M m2 s-1)

conduct <- mydata%>%
  select(TIMESTAMP_START,tair,patm,u)%>%
  mutate(pm = patm/(R*tair))%>%           # molar density (mol m-3) according to ideal gas law n=PV/RT, pm (n/volume, mol m-3)= P(Pa)/ R(m^3*Pa/mol*K)*T(K) 
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))%>%      # diffusivity of momentum (m2 s-1)
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)%>%            # Boundary layer conductance for heat (mol/m2 leaf/s)
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)%>%          # Boundary layer conductance for heat (mol/m2 leaf/s)
  mutate(DW = DW0*(P0/patm)*(tair/T0))%>% # Molecular diffusivity of mass H2o (M m2 s-1)
  mutate(Sc = v/DW)%>%                    #Sherwood number
  mutate(gbw = 0.036*Sc^0.33*Re^0.50)%>%  #boundary layer conductance for water vapor (mol m-2 s-1) 
  select(TIMESTAMP_START,gbh,gbw)

#  predefine leaf temperature
leaftemp<- mydata%>%
  select(TIMESTAMP_START,tair)%>%
  mutate(tleaf = tair)%>%
  select(TIMESTAMP_START,tleaf)

#merge all flux data in one dataframe
flux <- merge(qa, conduct, by="TIMESTAMP_START")
flux <- merge(flux, leaftemp, by="TIMESTAMP_START")

# Leaf stomatal conductance (mol H2O/m2 leaf/s)
flux$gs <- -0.1

#predefine fluxes from leaf temperature
flux$rnet<-rep(0,length(flux$TIMESTAMP_START))   #Leaf net radiation (W/m2 leaf)
flux$lwrad<-rep(0,length(flux$TIMESTAMP_START))  #Longwave radiation emitted from leaf (W/m2 leaf)
flux$shflx<-rep(0,length(flux$TIMESTAMP_START))  #Leaf sensible heat flux (W/m2 leaf)
flux$lhflx<-rep(0,length(flux$TIMESTAMP_START))  #Leaf latent heat flux (W/m2 leaf)
flux$etflx<-rep(0,length(flux$TIMESTAMP_START))  #Leaf transpiration flux (mol H2O/m2 leaf/s)

#define variables Leaf Temperature
vars_LeafTemperature <- list(flux,atmo,leaf,physcon)



