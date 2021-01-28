#Leaf temperature simulation setup
#for loop to simulate leaf temperature in denpendance of leaf energy balance
# Source: Bonan Ecosystem Modeling book, page 160

# 1. Read Input data
library(dplyr)
data <-read.csv("data/FLX_DE-Hainich1.csv",header=TRUE, sep=";", na.strings="NA", dec=",")
# select: air temperature TA_F (?C), vapor pressure deficit VPD_F (hPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (?C)
# wind speed WS_F (M s-1)
mydata<- data%>%select(TIMESTAMP_START,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F,TS_F_MDS_1,WS_F)

#2. change units from kPa to Pa and  degrees C to K
mydata<- mydata%>%
  mutate(tair = TA_F+273.15)%>%
  mutate(VPD_F = VPD_F*100)%>%
  mutate(patm = PA_F*1000)%>%
  mutate(u = WS_F)%>%
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F)

#atmospheric parameters

#select only necessary columns
atmo <- mydata%>%
  select(TIMESTAMP_START,patm,tair,VPD_F)

#create list
atmos <- as.list(atmo)
# Specific heat of air at constant pressure (J/mol/K)
atmos$cpair <- 29.2

#physical constants
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(tfrz,mmh2o,sigma)
physcon <- as.list(physcon)

#leaf propperties
emiss <- 0.96                        #leaf emissity
leaf <- data.frame(emiss)
leaf <- as.list(leaf)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4a
qa<- mydata%>%
  select(TIMESTAMP_START,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,qa)



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
dl <- 0.1    #leaf dimension characteristic length (cm)

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0?C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0?C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s-1) at 0?C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s-1) at 0?C
R <- 0.0821        # gas constant R: 0.167226 (J/Kg K)
d <- 1.225         #density of dry air at 15?C and sea level (g/L)
conduct1 <- mydata%>%
  select(TIMESTAMP_START,tair,patm,u)%>%
  mutate(pm = d*R*tair)%>%                # molar density (mol m-3) according to ideal gas law ???
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1)
  mutate(v = v0*(P0/patm)*(tair/T0))      # diffusivity of momentum (m^2 s-1)

conduct2 <- conduct1%>%
  select(TIMESTAMP_START,tair,patm,u,pm,Dh,v)%>%
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)             # Boundary layer conductance for heat (mol/m2 leaf/s)

#boundary layer conductance for water vapor gbw (mol m-2 s-1)
#              gbw = 0,036* Sc^0,33* Re^0,50
# Sc         #Sherwood number
#             Sc = ??/Dj

#constants
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0?C (Mm? s-1)

conduct3 <- conduct2%>%
  mutate(DW = DW0*(P0/patm)*(tair/T0)) #Molecular diffusivity of mass H2o (Mm? s-1)
conduct4 <- conduct3%>%
  mutate(Sc = v/DW)  #Sherwood number
conduct5 <- conduct4%>%
  mutate(gbw = 0.036*Sc^0.33*Re^0.50) #boundary layer conductance for water vapor (mol m-2 s-1)

conduct <- conduct5%>%
  select(TIMESTAMP_START,gbh,gbw)

#  predefine leaf temperature
leaftemp<- mydata%>%
  select(TIMESTAMP_START,tair)%>%
  mutate(tleaf = tair)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)
gs <- seq(from=-0.2,to=0.1,length.out = 52608)

flux <- merge(qa, conduct, by="TIMESTAMP_START")
flux <- merge(flux, leaftemp, by="TIMESTAMP_START")
flux <- as.list(flux)

#define variables Leaf Temperature
vars_Cpools <- flux



