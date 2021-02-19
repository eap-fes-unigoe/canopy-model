df4 <- read.csv("data/Hainich_2018_input.csv")
df5 <- read.csv("data/Hainich_2018_fluxes.csv")
df6 <- merge.data.frame(df4, df5)
head(df4)
str(df4)
View(df6)

#5. The most unprofessional sensitivity analysis you could possibly imagine
#run model twice with different input parameters (randomized and fixed)
#run while loop first with Initial_Tl, then with Initial_Tl1
#1.1 leaf emmissivity randomized
time <- seq(1,17520)
Initial_Tl_i <- rep(0.5/1000, length(time)) #example value, dont forget units
psych <- 66.5 #psychromatic constant, unit Pa K–1
latheatvap <- 2260 #latent heat of vaporisation, unit kJ/kg, constant
watvapsurrounding <- (df6$VPD_F*(df6$RH/100))/(1-(df6$RH/100)) #water vapor pressure of surrounding air, unit hPa
Ta <- df6$TA_F + 273.15 #temperature of surrounding air, unit K
Pa <- df6$PA_F #unit hPa, air pressure of surrounding air
u<- df6$WS_F #wind speed, unit m/s
molheat <- 29.2 #molar specific heat of moist air, unit J mol-1 K-1, constant
leafemiss <- runif(17520, min=0.75, max=1) #, leaf emissivity, unit ratio
dl <- 6 #characteristic leaf dimension, unit cm
sigma <- 5.76*(10^-8) #Boltzman constant, unit W m^-2 K^-4
g <- 9.80665 #gravtation, unit m/s-2
gsw<- rep(300, 17520) #stomatal conductance, unit mmol m-2 s-1
Qa_max <- 1e36
Initial_Tl <- rep(NA, length(time)) #define intial_value as state variable, necessary?
f0_max <- 1e-6
f1 <- Qa-LE_L-SHF_L-LHF_L
niter <- 0
niter_max <- 100

#formulas Felix code
pm<- Pa/(8.314 + Ta) #molar density, unit mol m-3
Theta_a <- Ta*((1000/Pa)^0.286) #equation potential temperature of air
physv <- (13.3*(10^-6)*(1013.25/Pa))*((Ta/273.15)^1.81) #molar diffusivity for momentum, unit M2 s-1
Dh <- (18.9*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) 
satvap <- SVP(Ta, isK = TRUE, formula = c("Clausius-Clapeyron", "Murray")) #saturation vapor pressure of free air at Ta, unit hPa
watvapleaf <- rep(5,17520) #example value, real function function(Ta){(6.113*exp(5423((1/273.15)-(1/Ta))))/Pa}, uni hPa
Qa <- 3.15*((0.52+0.13/60*abs(53)+(0.082-0.03/60*abs(53))*sqrt(watvapleaf))*(5.67e-8*(Ta)^4))
Dj <- (21.8*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) #molecular diffusivity of mass, unit Mm? s-1
gr <- (4*leafemiss*sigma*Ta^3)/molheat #radiative conductance to heat transfer, unit  W/m^2?
Gr <- g*(dl^3)*max(300-Ta)/(Ta*(physv^2)) #Grashof number, dimensionless
PrFo <- physv/Dh #Prandtl number forced convection, dimensionless
ReFo <- u*dl/physv #Reynolds number forced convection, dimensionless
NuFo <- 0.66*PrFo^0.33*ReFo^0.5 #Nusselt number forced convection, dimensionless
gbhFo <- (NuFo*pm*Dh)/dl #gbh forced convection, dimensionless
NuFree <- 0.54*(PrFo^0.25)*(Gr^0.25) #Nusslet number free convection, dimensionless
gbhFree <- (Dh*NuFree)/(dl*pm)
Sc <- physv/Dj #Sherwood number
gbw <- 0.036*(Sc^0.33)*(ReFo^0.50)
glw <- 1/((1/gbw) + (1/gsw))
s1 <- 10e-15*2264705*(watvapsurrounding)^2*(461.52*(Ta)^2) #example equation, real function delta(watvapsurrounding)/delta(Ta), unit dimensionless?
s <- rep(-10, 17520)
Initial_Tl <- Ta+((Qa-2*leafemiss*sigma*(Ta^4))-latheatvap*(watvapleaf-watvapsurrounding)*glw)/(2*molheat*gr*(0.5*gbhFo+0.5*gbhFree)+s*latheatvap*glw)
diff_T <- Ta-Initial_Tl
LE_L <- 2*leafemiss*sigma* (Initial_Tl)^4 #calculate long wave emission from leaf, unit W m⁻2
SHF_L<- 2*molheat * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000) #sensible heat flux from leaf, unit W m⁻2  
LHF_L<- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1))) #latenet heat flux , usingSVP() to calculate saturation water pressure in package"humidity"
plot(SHF_L)
#2. Input matlab code
# select: air temperature TA_F (°C), vapor pressure deficit VPD_F (kPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (°C)
# wind speed WS_F (M s-1)
df1 <- df6%>%select(TIMESTAMP_START,TIMESTAMP_END,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F, TS_F_MDS_1,WS_F)

#change units from kPa to Pa and °C to K
df1 <- df1%>% 
  mutate(tair = TA_F+273.15)%>% 
  mutate(VPD_F = VPD_F*1000)%>% 
  mutate(patm = PA_F*1000)%>% 
  mutate(u = WS_F)%>% 
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F)

#atmospheric parameters

#select only necessary columns
atmo <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,patm,tair,VPD_F)

# Specific heat of air at constant pressure (J/mol/K)
cpair <- 29.2 

atmos <- list(atmo,cpair)

#physical constants
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(tfrz,mmh2o,sigma)

#leaf propperties
emiss <- 0.96                        #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4
qa <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,TIMESTAMP_END,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection 
# gbh = (Nu*ρm*Dh)/dl 

# required parameters:
# pm         #molar density (mol m-3) 
#ideal gas law: p = atmo$patm/R*atmo$tair   gas constant R: 0.167226 (J/Kg K)
# dl         #leaf dimension characteristic length (cm)
# Dh         #diffusivity of heat Dh (m^2 s–1)
# Nu         #Nusselt number for forced conv. 
#             Nu = 0,66* Pr^0,33* Re^0,5
# Pr         #Prandtl number (molecular diffusion properties of the fluid)
#             Pr = ν/Dh
# Re         #Reynolds number 
#             Re = u*dl/ν
# u          #wind speed (M s-1)

# constants 
dl <- 0.1    #leaf dimension characteristic length (cm)

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0°C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0°C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s–1) at 0°C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s–1) at 0°C
R <- 0.167226      # gas constant R: 0.167226 (J/Kg K)

conduct1 <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u)%>%
  mutate(pm = patm/R*tair)%>%             # molar density (mol m-3) according to ideal gas law ???
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))      # diffusivity of momentum (m^2 s-1)

conduct2 <- conduct1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u,pm,Dh,v)%>%
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)             # Boundary layer conductance for heat (mol/m2 leaf/s)

#boundary layer conductance for water vapor gbw (mol m-2 s-1) 
#              gbw = 0,036* Sc^0,33* Re^0,50
# Sc         #Sherwood number
#             Sc = ν/Dj

#constants
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0°C (Mm² s-1)

conduct3 <- conduct2%>%
  mutate(DW = DW0*(P0/patm)*(tair/T0)) #Molecular diffusivity of mass H2o (Mm² s-1)
conduct4 <- conduct3%>%
  mutate(Sc = v/DW)  #Sherwood number
conduct5 <- conduct4%>%
  mutate(gbw = 0.036*Sc^0.33*Re^0.50) #boundary layer conductance for water vapor (mol m-2 s-1) 

conduct <- conduct5%>%
  select(TIMESTAMP_START,TIMESTAMP_END,gbh,gbw)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)  
gs <- seq(from=-0.2,to=0.1,length.out = 100)

#merge data in one list
flux <- list(qa,conduct,gs)

alldata<-list(atmos,physcon,leaf, flux)

#3. Functions matlab code
#function of Saturation vapor pressure and temperature derivative -- satvap()
satvap2 <- function (tc) {   
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
  return(list(esat, desat))}

#4. Iterations matlab code
# --- Newton-Raphson iteration until leaf energy balance is less than
# f0_max or to niter_max iterations

niter <- 0 # Number of iterations
f0 <- 1e36 # Leaf energy balance (W/m2)

niter_max <- 100 # Maximum number of iterations
f0_max <- 9  # Maximum energy imbalance (W/m2)

fluxtleaf <- Initial_Tl #initial leaf temperature

psych <- 66.5 #psychromatic constant, unit Pa K–1

while ((niter < niter_max) & (abs(mean(f0, na.rm = TRUE)) > f0_max)){
  abs(mean(f0, na.rm = TRUE))
  # Increment iteration counter
  
  niter <- niter + 1
  
  # Latent heat of vaporization (J/mol)
  lambda_val <- latheatvap           # J/g -> J/kg -> J/mol
  
  # Saturation vapor pressure (Pa) and temperature derivative (Pa/K)
  
  esat <- 167.803645898318
  desat <- 15.4639490550184
  
  # Leaf conductance for water vapor (mol H2O/m2/s)
  
  gleaf <- gsw * gbw / (gsw + gbw)
  
  # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
  
  flux$lwrad <- 2 * leaf$emiss * physcon$sigma * fluxtleaf^4
  dlwrad <- 8 * leafemiss * sigma * fluxtleaf^3
  
  # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$shflx <- 2 * cpair * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000)
  dshflx <- 2 * molheat * (gbhFo*0.5+gbhFree*0.5)
  
  # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$lhflx <- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1)))
  dlhflx <- lambda_val / Pa * desat * gleaf
  
  # Energy balance (W/m2) and temperature derivative (W/m2/K)
  
  f0 <- Qa - flux$lwrad - flux$shflx - flux$lhflx
  df0 <- -dlwrad - dshflx - dlhflx
  # Change in leaf temperature
  
  dtleaf <- (-f0 / df0)
  
  # Update leaf temperature
  
  fluxtleaf <- fluxtleaf + dtleaf
  
}

#1.2 fixed leaf emmissivity
time <- seq(1,17520)
Initial_Tl_i <- rep(0.5/1000, length(time)) #example value, dont forget units
psych <- 66.5 #psychromatic constant, unit Pa K–1
latheatvap <- 2260 #latent heat of vaporisation, unit kJ/kg, constant
watvapsurrounding <- (df6$VPD_F*(df6$RH/100))/(1-(df6$RH/100)) #water vapor pressure of surrounding air, unit hPa
Ta <- df6$TA_F + 273.15 #temperature of surrounding air, unit K
Pa <- df6$PA_F #unit hPa, air pressure of surrounding air
u<- df6$WS_F #wind speed, unit m/s
molheat <- 29.2 #molar specific heat of moist air, unit J mol-1 K-1, constant
leafemiss <- 0.96 #, leaf emissivity, unit ratio
dl <- 6 #characteristic leaf dimension, unit cm
sigma <- 5.76*(10^-8) #Boltzman constant, unit W m^-2 K^-4
g <- 9.80665 #gravtation, unit m/s-2
gsw<- rep(300, 17520) #stomatal conductance, unit mmol m-2 s-1
Qa_max <- 1e36
Initial_Tl <- rep(NA, length(time)) #define intial_value as state variable, necessary?
f0_max <- 1e-6
f1 <- Qa-LE_L-SHF_L-LHF_L
niter <- 0
niter_max <- 100

#formulas Felix code
pm<- Pa/(8.314 + Ta) #molar density, unit mol m-3
Theta_a <- Ta*((1000/Pa)^0.286) #equation potential temperature of air
physv <- (13.3*(10^-6)*(1013.25/Pa))*((Ta/273.15)^1.81) #molar diffusivity for momentum, unit M2 s-1
Dh <- (18.9*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) 
satvap <- SVP(Ta, isK = TRUE, formula = c("Clausius-Clapeyron", "Murray")) #saturation vapor pressure of free air at Ta, unit hPa
watvapleaf <- rep(5,17520) #example value, real function function(Ta){(6.113*exp(5423((1/273.15)-(1/Ta))))/Pa}, uni hPa
Qa <- 3.15*((0.52+0.13/60*abs(53)+(0.082-0.03/60*abs(53))*sqrt(watvapleaf))*(5.67e-8*(Ta)^4))
Dj <- (21.8*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) #molecular diffusivity of mass, unit Mm? s-1
gr <- (4*leafemiss*sigma*Ta^3)/molheat #radiative conductance to heat transfer, unit  W/m^2?
Gr <- g*(dl^3)*max(300-Ta)/(Ta*(physv^2)) #Grashof number, dimensionless
PrFo <- physv/Dh #Prandtl number forced convection, dimensionless
ReFo <- u*dl/physv #Reynolds number forced convection, dimensionless
NuFo <- 0.66*PrFo^0.33*ReFo^0.5 #Nusselt number forced convection, dimensionless
gbhFo <- (NuFo*pm*Dh)/dl #gbh forced convection, dimensionless
NuFree <- 0.54*(PrFo^0.25)*(Gr^0.25) #Nusslet number free convection, dimensionless
gbhFree <- (Dh*NuFree)/(dl*pm)
Sc <- physv/Dj #Sherwood number
gbw <- 0.036*(Sc^0.33)*(ReFo^0.50)
glw <- 1/((1/gbw) + (1/gsw))
s1 <- 10e-15*2264705*(watvapsurrounding)^2*(461.52*(Ta)^2) #example equation, real function delta(watvapsurrounding)/delta(Ta), unit dimensionless?
s <- rep(-10, 17520)
Initial_Tl <- Ta+((Qa-2*leafemiss*sigma*(Ta^4))-latheatvap*(watvapleaf-watvapsurrounding)*glw)/(2*molheat*gr*(0.5*gbhFo+0.5*gbhFree)+s*latheatvap*glw)
diff_T <- Ta-Initial_Tl
LE_L <- 2*leafemiss*sigma* (Initial_Tl)^4 #calculate long wave emission from leaf, unit W m⁻2
SHF_L<- 2*molheat * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000) #sensible heat flux from leaf, unit W m⁻2  
LHF_L<- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1))) #latenet heat flux , usingSVP() to calculate saturation water pressure in package"humidity"
plot(SHF_L)
#2. Input matlab code
# select: air temperature TA_F (°C), vapor pressure deficit VPD_F (kPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (°C)
# wind speed WS_F (M s-1)
df1 <- df6%>%select(TIMESTAMP_START,TIMESTAMP_END,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F, TS_F_MDS_1,WS_F)

#change units from kPa to Pa and °C to K
df1 <- df1%>% 
  mutate(tair = TA_F+273.15)%>% 
  mutate(VPD_F = VPD_F*1000)%>% 
  mutate(patm = PA_F*1000)%>% 
  mutate(u = WS_F)%>% 
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F)

#atmospheric parameters

#select only necessary columns
atmo <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,patm,tair,VPD_F)

# Specific heat of air at constant pressure (J/mol/K)
cpair <- 29.2 

atmos <- list(atmo,cpair)

#physical constants
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(tfrz,mmh2o,sigma)

#leaf propperties
emiss <- 0.96                        #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4
qa <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,TIMESTAMP_END,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection 
# gbh = (Nu*ρm*Dh)/dl 

# required parameters:
# pm         #molar density (mol m-3) 
#ideal gas law: p = atmo$patm/R*atmo$tair   gas constant R: 0.167226 (J/Kg K)
# dl         #leaf dimension characteristic length (cm)
# Dh         #diffusivity of heat Dh (m^2 s–1)
# Nu         #Nusselt number for forced conv. 
#             Nu = 0,66* Pr^0,33* Re^0,5
# Pr         #Prandtl number (molecular diffusion properties of the fluid)
#             Pr = ν/Dh
# Re         #Reynolds number 
#             Re = u*dl/ν
# u          #wind speed (M s-1)

# constants 
dl <- 0.1    #leaf dimension characteristic length (cm)

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0°C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0°C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s–1) at 0°C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s–1) at 0°C
R <- 0.167226      # gas constant R: 0.167226 (J/Kg K)

conduct1 <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u)%>%
  mutate(pm = patm/R*tair)%>%             # molar density (mol m-3) according to ideal gas law ???
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))      # diffusivity of momentum (m^2 s-1)

conduct2 <- conduct1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u,pm,Dh,v)%>%
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)             # Boundary layer conductance for heat (mol/m2 leaf/s)

#boundary layer conductance for water vapor gbw (mol m-2 s-1) 
#              gbw = 0,036* Sc^0,33* Re^0,50
# Sc         #Sherwood number
#             Sc = ν/Dj

#constants
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0°C (Mm² s-1)

conduct3 <- conduct2%>%
  mutate(DW = DW0*(P0/patm)*(tair/T0)) #Molecular diffusivity of mass H2o (Mm² s-1)
conduct4 <- conduct3%>%
  mutate(Sc = v/DW)  #Sherwood number
conduct5 <- conduct4%>%
  mutate(gbw = 0.036*Sc^0.33*Re^0.50) #boundary layer conductance for water vapor (mol m-2 s-1) 

conduct <- conduct5%>%
  select(TIMESTAMP_START,TIMESTAMP_END,gbh,gbw)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)  
gs <- seq(from=-0.2,to=0.1,length.out = 100)

#merge data in one list
flux <- list(qa,conduct,gs)

alldata<-list(atmos,physcon,leaf, flux)

#3. Functions matlab code
#function of Saturation vapor pressure and temperature derivative -- satvap()
satvap2 <- function (tc) {   
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
  return(list(esat, desat))}

#4. Iterations matlab code
# --- Newton-Raphson iteration until leaf energy balance is less than
# f0_max or to niter_max iterations

niter <- 0 # Number of iterations
f0 <- 1e36 # Leaf energy balance (W/m2)

niter_max <- 100 # Maximum number of iterations
f0_max <- 9  # Maximum energy imbalance (W/m2)

fluxtleaf <- Initial_Tl #initial leaf temperature

psych <- 66.5 #psychromatic constant, unit Pa K–1

while ((niter < niter_max) & (abs(mean(f0, na.rm = TRUE)) > f0_max)){
  abs(mean(f0, na.rm = TRUE))
  # Increment iteration counter
  
  niter <- niter + 1
  
  # Latent heat of vaporization (J/mol)
  lambda_val <- latheatvap           # J/g -> J/kg -> J/mol
  
  # Saturation vapor pressure (Pa) and temperature derivative (Pa/K)
  
  esat <- 167.803645898318
  desat <- 15.4639490550184
  
  # Leaf conductance for water vapor (mol H2O/m2/s)
  
  gleaf <- gsw * gbw / (gsw + gbw)
  
  # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
  
  flux$lwrad <- 2 * leafemiss * physcon$sigma * fluxtleaf^4
  dlwrad <- 8 * leafemiss * sigma * fluxtleaf^3
  
  # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$shflx <- 2 * cpair * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000)
  dshflx <- 2 * molheat * (gbhFo*0.5+gbhFree*0.5)
  
  # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$lhflx <- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1)))
  dlhflx <- lambda_val / Pa * desat * gleaf
  
  # Energy balance (W/m2) and temperature derivative (W/m2/K)
  
  f0 <- Qa - flux$lwrad - flux$shflx - flux$lhflx
  df0 <- -dlwrad - dshflx - dlhflx
  # Change in leaf temperature
  
  dtleaf <- (-f0 / df0)
  
  # Update leaf temperature
  
  fluxtleaf1 <- fluxtleaf + dtleaf
  
}
plot(fluxtleaf-fluxtleaf1, ylab="residuals fix-random emmissivity")

#2.1 randomized leaf dimensions
time <- seq(1,17520)
Initial_Tl_i <- rep(0.5/1000, length(time)) #example value, dont forget units
psych <- 66.5 #psychromatic constant, unit Pa K–1
latheatvap <- 2260 #latent heat of vaporisation, unit kJ/kg, constant
watvapsurrounding <- (df6$VPD_F*(df6$RH/100))/(1-(df6$RH/100)) #water vapor pressure of surrounding air, unit hPa
Ta <- df6$TA_F + 273.15 #temperature of surrounding air, unit K
Pa <- df6$PA_F #unit hPa, air pressure of surrounding air
u<- df6$WS_F #wind speed, unit m/s
molheat <- 29.2 #molar specific heat of moist air, unit J mol-1 K-1, constant
leafemiss <- 0.96 #, leaf emissivity, unit ratio
dl <- runif(17520, min=1, max=10) #characteristic leaf dimension, unit cm
sigma <- 5.76*(10^-8) #Boltzman constant, unit W m^-2 K^-4
g <- 9.80665 #gravtation, unit m/s-2
gsw<- rep(300, 17520) #stomatal conductance, unit mmol m-2 s-1
Qa_max <- 1e36
Initial_Tl <- rep(NA, length(time)) #define intial_value as state variable, necessary?
f0_max <- 1e-6
f1 <- Qa-LE_L-SHF_L-LHF_L
niter <- 0
niter_max <- 100

#formulas Felix code
pm<- Pa/(8.314 + Ta) #molar density, unit mol m-3
Theta_a <- Ta*((1000/Pa)^0.286) #equation potential temperature of air
physv <- (13.3*(10^-6)*(1013.25/Pa))*((Ta/273.15)^1.81) #molar diffusivity for momentum, unit M2 s-1
Dh <- (18.9*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) 
satvap <- SVP(Ta, isK = TRUE, formula = c("Clausius-Clapeyron", "Murray")) #saturation vapor pressure of free air at Ta, unit hPa
watvapleaf <- rep(5,17520) #example value, real function function(Ta){(6.113*exp(5423((1/273.15)-(1/Ta))))/Pa}, uni hPa
Qa <- 3.15*((0.52+0.13/60*abs(53)+(0.082-0.03/60*abs(53))*sqrt(watvapleaf))*(5.67e-8*(Ta)^4))
Dj <- (21.8*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) #molecular diffusivity of mass, unit Mm? s-1
gr <- (4*leafemiss*sigma*Ta^3)/molheat #radiative conductance to heat transfer, unit  W/m^2?
Gr <- g*(dl^3)*max(300-Ta)/(Ta*(physv^2)) #Grashof number, dimensionless
PrFo <- physv/Dh #Prandtl number forced convection, dimensionless
ReFo <- u*dl/physv #Reynolds number forced convection, dimensionless
NuFo <- 0.66*PrFo^0.33*ReFo^0.5 #Nusselt number forced convection, dimensionless
gbhFo <- (NuFo*pm*Dh)/dl #gbh forced convection, dimensionless
NuFree <- 0.54*(PrFo^0.25)*(Gr^0.25) #Nusslet number free convection, dimensionless
gbhFree <- (Dh*NuFree)/(dl*pm)
Sc <- physv/Dj #Sherwood number
gbw <- 0.036*(Sc^0.33)*(ReFo^0.50)
glw <- 1/((1/gbw) + (1/gsw))
s1 <- 10e-15*2264705*(watvapsurrounding)^2*(461.52*(Ta)^2) #example equation, real function delta(watvapsurrounding)/delta(Ta), unit dimensionless?
s <- rep(-10, 17520)
Initial_Tl <- Ta+((Qa-2*leafemiss*sigma*(Ta^4))-latheatvap*(watvapleaf-watvapsurrounding)*glw)/(2*molheat*gr*(0.5*gbhFo+0.5*gbhFree)+s*latheatvap*glw)
diff_T <- Ta-Initial_Tl
LE_L <- 2*leafemiss*sigma* (Initial_Tl)^4 #calculate long wave emission from leaf, unit W m⁻2
SHF_L<- 2*molheat * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000) #sensible heat flux from leaf, unit W m⁻2  
LHF_L<- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1))) #latenet heat flux , usingSVP() to calculate saturation water pressure in package"humidity"
plot(SHF_L)
#2. Input matlab code
# select: air temperature TA_F (°C), vapor pressure deficit VPD_F (kPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (°C)
# wind speed WS_F (M s-1)
df1 <- df6%>%select(TIMESTAMP_START,TIMESTAMP_END,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F, TS_F_MDS_1,WS_F)

#change units from kPa to Pa and °C to K
df1 <- df1%>% 
  mutate(tair = TA_F+273.15)%>% 
  mutate(VPD_F = VPD_F*1000)%>% 
  mutate(patm = PA_F*1000)%>% 
  mutate(u = WS_F)%>% 
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F)

#atmospheric parameters

#select only necessary columns
atmo <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,patm,tair,VPD_F)

# Specific heat of air at constant pressure (J/mol/K)
cpair <- 29.2 

atmos <- list(atmo,cpair)

#physical constants
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(tfrz,mmh2o,sigma)

#leaf propperties
emiss <- 0.96                        #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4
qa <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,TIMESTAMP_END,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection 
# gbh = (Nu*ρm*Dh)/dl 

# required parameters:
# pm         #molar density (mol m-3) 
#ideal gas law: p = atmo$patm/R*atmo$tair   gas constant R: 0.167226 (J/Kg K)
# dl         #leaf dimension characteristic length (cm)
# Dh         #diffusivity of heat Dh (m^2 s–1)
# Nu         #Nusselt number for forced conv. 
#             Nu = 0,66* Pr^0,33* Re^0,5
# Pr         #Prandtl number (molecular diffusion properties of the fluid)
#             Pr = ν/Dh
# Re         #Reynolds number 
#             Re = u*dl/ν
# u          #wind speed (M s-1)

# constants 

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0°C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0°C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s–1) at 0°C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s–1) at 0°C
R <- 0.167226      # gas constant R: 0.167226 (J/Kg K)

conduct1 <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u)%>%
  mutate(pm = patm/R*tair)%>%             # molar density (mol m-3) according to ideal gas law ???
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))      # diffusivity of momentum (m^2 s-1)

conduct2 <- conduct1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u,pm,Dh,v)%>%
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)             # Boundary layer conductance for heat (mol/m2 leaf/s)

#boundary layer conductance for water vapor gbw (mol m-2 s-1) 
#              gbw = 0,036* Sc^0,33* Re^0,50
# Sc         #Sherwood number
#             Sc = ν/Dj

#constants
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0°C (Mm² s-1)

conduct3 <- conduct2%>%
  mutate(DW = DW0*(P0/patm)*(tair/T0)) #Molecular diffusivity of mass H2o (Mm² s-1)
conduct4 <- conduct3%>%
  mutate(Sc = v/DW)  #Sherwood number
conduct5 <- conduct4%>%
  mutate(gbw = 0.036*Sc^0.33*Re^0.50) #boundary layer conductance for water vapor (mol m-2 s-1) 

conduct <- conduct5%>%
  select(TIMESTAMP_START,TIMESTAMP_END,gbh,gbw)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)  
gs <- seq(from=-0.2,to=0.1,length.out = 100)

#merge data in one list
flux <- list(qa,conduct,gs)

alldata<-list(atmos,physcon,leaf, flux)

#3. Functions matlab code
#function of Saturation vapor pressure and temperature derivative -- satvap()
satvap2 <- function (tc) {   
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
  return(list(esat, desat))}

#4. Iterations matlab code
# --- Newton-Raphson iteration until leaf energy balance is less than
# f0_max or to niter_max iterations

niter <- 0 # Number of iterations
f0 <- 1e36 # Leaf energy balance (W/m2)

niter_max <- 100 # Maximum number of iterations
f0_max <- 9  # Maximum energy imbalance (W/m2)

fluxtleaf <- Initial_Tl #initial leaf temperature

psych <- 66.5 #psychromatic constant, unit Pa K–1

while ((niter < niter_max) & (abs(mean(f0, na.rm = TRUE)) > f0_max)){
  abs(mean(f0, na.rm = TRUE))
  # Increment iteration counter
  
  niter <- niter + 1
  
  # Latent heat of vaporization (J/mol)
  lambda_val <- latheatvap           # J/g -> J/kg -> J/mol
  
  # Saturation vapor pressure (Pa) and temperature derivative (Pa/K)
  
  esat <- 167.803645898318
  desat <- 15.4639490550184
  
  # Leaf conductance for water vapor (mol H2O/m2/s)
  
  gleaf <- gsw * gbw / (gsw + gbw)
  
  # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
  
  flux$lwrad <- 2 * leafemiss * physcon$sigma * fluxtleaf^4
  dlwrad <- 8 * leafemiss * sigma * fluxtleaf^3
  
  # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$shflx <- 2 * cpair * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000)
  dshflx <- 2 * molheat * (gbhFo*0.5+gbhFree*0.5)
  
  # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$lhflx <- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1)))
  dlhflx <- lambda_val / Pa * desat * gleaf
  
  # Energy balance (W/m2) and temperature derivative (W/m2/K)
  
  f0 <- Qa - flux$lwrad - flux$shflx - flux$lhflx
  df0 <- -dlwrad - dshflx - dlhflx
  # Change in leaf temperature
  
  dtleaf <- (-f0 / df0)
  
  # Update leaf temperature
  
  fluxtleaf2 <- fluxtleaf + dtleaf
  
}

#2.2 fixed leaf dimension
time <- seq(1,17520)
Initial_Tl_i <- rep(0.5/1000, length(time)) #example value, dont forget units
psych <- 66.5 #psychromatic constant, unit Pa K–1
latheatvap <- 2260 #latent heat of vaporisation, unit kJ/kg, constant
watvapsurrounding <- (df6$VPD_F*(df6$RH/100))/(1-(df6$RH/100)) #water vapor pressure of surrounding air, unit hPa
Ta <- df6$TA_F + 273.15 #temperature of surrounding air, unit K
Pa <- df6$PA_F #unit hPa, air pressure of surrounding air
u<- df6$WS_F #wind speed, unit m/s
molheat <- 29.2 #molar specific heat of moist air, unit J mol-1 K-1, constant
leafemiss <- 0.96 #, leaf emissivity, unit ratio
dl <- 6 #characteristic leaf dimension, unit cm
sigma <- 5.76*(10^-8) #Boltzman constant, unit W m^-2 K^-4
g <- 9.80665 #gravtation, unit m/s-2
gsw<- rep(300, 17520) #stomatal conductance, unit mmol m-2 s-1
Qa_max <- 1e36
Initial_Tl <- rep(NA, length(time)) #define intial_value as state variable, necessary?
f0_max <- 1e-6
f1 <- Qa-LE_L-SHF_L-LHF_L
niter <- 0
niter_max <- 100

#formulas Felix code
pm<- Pa/(8.314 + Ta) #molar density, unit mol m-3
Theta_a <- Ta*((1000/Pa)^0.286) #equation potential temperature of air
physv <- (13.3*(10^-6)*(1013.25/Pa))*((Ta/273.15)^1.81) #molar diffusivity for momentum, unit M2 s-1
Dh <- (18.9*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) 
satvap <- SVP(Ta, isK = TRUE, formula = c("Clausius-Clapeyron", "Murray")) #saturation vapor pressure of free air at Ta, unit hPa
watvapleaf <- rep(5,17520) #example value, real function function(Ta){(6.113*exp(5423((1/273.15)-(1/Ta))))/Pa}, uni hPa
Qa <- 3.15*((0.52+0.13/60*abs(53)+(0.082-0.03/60*abs(53))*sqrt(watvapleaf))*(5.67e-8*(Ta)^4))
Dj <- (21.8*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) #molecular diffusivity of mass, unit Mm? s-1
gr <- (4*leafemiss*sigma*Ta^3)/molheat #radiative conductance to heat transfer, unit  W/m^2?
Gr <- g*(dl^3)*max(300-Ta)/(Ta*(physv^2)) #Grashof number, dimensionless
PrFo <- physv/Dh #Prandtl number forced convection, dimensionless
ReFo <- u*dl/physv #Reynolds number forced convection, dimensionless
NuFo <- 0.66*PrFo^0.33*ReFo^0.5 #Nusselt number forced convection, dimensionless
gbhFo <- (NuFo*pm*Dh)/dl #gbh forced convection, dimensionless
NuFree <- 0.54*(PrFo^0.25)*(Gr^0.25) #Nusslet number free convection, dimensionless
gbhFree <- (Dh*NuFree)/(dl*pm)
Sc <- physv/Dj #Sherwood number
gbw <- 0.036*(Sc^0.33)*(ReFo^0.50)
glw <- 1/((1/gbw) + (1/gsw))
s1 <- 10e-15*2264705*(watvapsurrounding)^2*(461.52*(Ta)^2) #example equation, real function delta(watvapsurrounding)/delta(Ta), unit dimensionless?
s <- rep(-10, 17520)
Initial_Tl <- Ta+((Qa-2*leafemiss*sigma*(Ta^4))-latheatvap*(watvapleaf-watvapsurrounding)*glw)/(2*molheat*gr*(0.5*gbhFo+0.5*gbhFree)+s*latheatvap*glw)
diff_T <- Ta-Initial_Tl
LE_L <- 2*leafemiss*sigma* (Initial_Tl)^4 #calculate long wave emission from leaf, unit W m⁻2
SHF_L<- 2*molheat * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000) #sensible heat flux from leaf, unit W m⁻2  
LHF_L<- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1))) #latenet heat flux , usingSVP() to calculate saturation water pressure in package"humidity"
plot(SHF_L)
#2. Input matlab code
# select: air temperature TA_F (°C), vapor pressure deficit VPD_F (kPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (°C)
# wind speed WS_F (M s-1)
df1 <- df6%>%select(TIMESTAMP_START,TIMESTAMP_END,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F, TS_F_MDS_1,WS_F)

#change units from kPa to Pa and °C to K
df1 <- df1%>% 
  mutate(tair = TA_F+273.15)%>% 
  mutate(VPD_F = VPD_F*1000)%>% 
  mutate(patm = PA_F*1000)%>% 
  mutate(u = WS_F)%>% 
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F)

#atmospheric parameters

#select only necessary columns
atmo <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,patm,tair,VPD_F)

# Specific heat of air at constant pressure (J/mol/K)
cpair <- 29.2 

atmos <- list(atmo,cpair)

#physical constants
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(tfrz,mmh2o,sigma)

#leaf propperties
emiss <- 0.96                        #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4
qa <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,TIMESTAMP_END,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection 
# gbh = (Nu*ρm*Dh)/dl 

# required parameters:
# pm         #molar density (mol m-3) 
#ideal gas law: p = atmo$patm/R*atmo$tair   gas constant R: 0.167226 (J/Kg K)
# dl         #leaf dimension characteristic length (cm)
# Dh         #diffusivity of heat Dh (m^2 s–1)
# Nu         #Nusselt number for forced conv. 
#             Nu = 0,66* Pr^0,33* Re^0,5
# Pr         #Prandtl number (molecular diffusion properties of the fluid)
#             Pr = ν/Dh
# Re         #Reynolds number 
#             Re = u*dl/ν
# u          #wind speed (M s-1)

# constants 

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0°C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0°C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s–1) at 0°C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s–1) at 0°C
R <- 0.167226      # gas constant R: 0.167226 (J/Kg K)

conduct1 <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u)%>%
  mutate(pm = patm/R*tair)%>%             # molar density (mol m-3) according to ideal gas law ???
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))      # diffusivity of momentum (m^2 s-1)

conduct2 <- conduct1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u,pm,Dh,v)%>%
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)             # Boundary layer conductance for heat (mol/m2 leaf/s)

#boundary layer conductance for water vapor gbw (mol m-2 s-1) 
#              gbw = 0,036* Sc^0,33* Re^0,50
# Sc         #Sherwood number
#             Sc = ν/Dj

#constants
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0°C (Mm² s-1)

conduct3 <- conduct2%>%
  mutate(DW = DW0*(P0/patm)*(tair/T0)) #Molecular diffusivity of mass H2o (Mm² s-1)
conduct4 <- conduct3%>%
  mutate(Sc = v/DW)  #Sherwood number
conduct5 <- conduct4%>%
  mutate(gbw = 0.036*Sc^0.33*Re^0.50) #boundary layer conductance for water vapor (mol m-2 s-1) 

conduct <- conduct5%>%
  select(TIMESTAMP_START,TIMESTAMP_END,gbh,gbw)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)  
gs <- seq(from=-0.2,to=0.1,length.out = 100)

#merge data in one list
flux <- list(qa,conduct,gs)

alldata<-list(atmos,physcon,leaf, flux)

#3. Functions matlab code
#function of Saturation vapor pressure and temperature derivative -- satvap()
satvap2 <- function (tc) {   
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
  return(list(esat, desat))}

#4. Iterations matlab code
# --- Newton-Raphson iteration until leaf energy balance is less than
# f0_max or to niter_max iterations

niter <- 0 # Number of iterations
f0 <- 1e36 # Leaf energy balance (W/m2)

niter_max <- 100 # Maximum number of iterations
f0_max <- 9  # Maximum energy imbalance (W/m2)

fluxtleaf <- Initial_Tl #initial leaf temperature

psych <- 66.5 #psychromatic constant, unit Pa K–1

while ((niter < niter_max) & (abs(mean(f0, na.rm = TRUE)) > f0_max)){
  abs(mean(f0, na.rm = TRUE))
  # Increment iteration counter
  
  niter <- niter + 1
  
  # Latent heat of vaporization (J/mol)
  lambda_val <- latheatvap           # J/g -> J/kg -> J/mol
  
  # Saturation vapor pressure (Pa) and temperature derivative (Pa/K)
  
  esat <- 167.803645898318
  desat <- 15.4639490550184
  
  # Leaf conductance for water vapor (mol H2O/m2/s)
  
  gleaf <- gsw * gbw / (gsw + gbw)
  
  # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
  
  flux$lwrad <- 2 * leafemiss * physcon$sigma * fluxtleaf^4
  dlwrad <- 8 * leafemiss * sigma * fluxtleaf^3
  
  # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$shflx <- 2 * cpair * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000)
  dshflx <- 2 * molheat * (gbhFo*0.5+gbhFree*0.5)
  
  # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$lhflx <- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1)))
  dlhflx <- lambda_val / Pa * desat * gleaf
  
  # Energy balance (W/m2) and temperature derivative (W/m2/K)
  
  f0 <- Qa - flux$lwrad - flux$shflx - flux$lhflx
  df0 <- -dlwrad - dshflx - dlhflx
  # Change in leaf temperature
  
  dtleaf <- (-f0 / df0)
  
  # Update leaf temperature
  
  fluxtleaf3 <- fluxtleaf + dtleaf
  
}
plot(fluxtleaf-fluxtleaf1, ylab="residuals fix-random leaf dimensions")

#3. randomized latent heat of vaporization
time <- seq(1,17520)
Initial_Tl_i <- rep(0.5/1000, length(time)) #example value, dont forget units
psych <- 66.5 #psychromatic constant, unit Pa K–1
latheatvap <- runif(17520, min=1500, max=3000) #latent heat of vaporisation, unit kJ/kg, constant
watvapsurrounding <- (df6$VPD_F*(df6$RH/100))/(1-(df6$RH/100)) #water vapor pressure of surrounding air, unit hPa
Ta <- df6$TA_F + 273.15 #temperature of surrounding air, unit K
Pa <- df6$PA_F #unit hPa, air pressure of surrounding air
u<- df6$WS_F #wind speed, unit m/s
molheat <- 29.2 #molar specific heat of moist air, unit J mol-1 K-1, constant
leafemiss <- 0.96 #, leaf emissivity, unit ratio
dl <- runif(17520, min=1, max=10) #characteristic leaf dimension, unit cm
sigma <- 5.76*(10^-8) #Boltzman constant, unit W m^-2 K^-4
g <- 9.80665 #gravtation, unit m/s-2
gsw<- rep(300, 17520) #stomatal conductance, unit mmol m-2 s-1
Qa_max <- 1e36
Initial_Tl <- rep(NA, length(time)) #define intial_value as state variable, necessary?
f0_max <- 1e-6
f1 <- Qa-LE_L-SHF_L-LHF_L
niter <- 0
niter_max <- 100

#formulas Felix code
pm<- Pa/(8.314 + Ta) #molar density, unit mol m-3
Theta_a <- Ta*((1000/Pa)^0.286) #equation potential temperature of air
physv <- (13.3*(10^-6)*(1013.25/Pa))*((Ta/273.15)^1.81) #molar diffusivity for momentum, unit M2 s-1
Dh <- (18.9*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) 
satvap <- SVP(Ta, isK = TRUE, formula = c("Clausius-Clapeyron", "Murray")) #saturation vapor pressure of free air at Ta, unit hPa
watvapleaf <- rep(5,17520) #example value, real function function(Ta){(6.113*exp(5423((1/273.15)-(1/Ta))))/Pa}, uni hPa
Qa <- 3.15*((0.52+0.13/60*abs(53)+(0.082-0.03/60*abs(53))*sqrt(watvapleaf))*(5.67e-8*(Ta)^4))
Dj <- (21.8*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) #molecular diffusivity of mass, unit Mm? s-1
gr <- (4*leafemiss*sigma*Ta^3)/molheat #radiative conductance to heat transfer, unit  W/m^2?
Gr <- g*(dl^3)*max(300-Ta)/(Ta*(physv^2)) #Grashof number, dimensionless
PrFo <- physv/Dh #Prandtl number forced convection, dimensionless
ReFo <- u*dl/physv #Reynolds number forced convection, dimensionless
NuFo <- 0.66*PrFo^0.33*ReFo^0.5 #Nusselt number forced convection, dimensionless
gbhFo <- (NuFo*pm*Dh)/dl #gbh forced convection, dimensionless
NuFree <- 0.54*(PrFo^0.25)*(Gr^0.25) #Nusslet number free convection, dimensionless
gbhFree <- (Dh*NuFree)/(dl*pm)
Sc <- physv/Dj #Sherwood number
gbw <- 0.036*(Sc^0.33)*(ReFo^0.50)
glw <- 1/((1/gbw) + (1/gsw))
s1 <- 10e-15*2264705*(watvapsurrounding)^2*(461.52*(Ta)^2) #example equation, real function delta(watvapsurrounding)/delta(Ta), unit dimensionless?
s <- rep(-10, 17520)
Initial_Tl <- Ta+((Qa-2*leafemiss*sigma*(Ta^4))-latheatvap*(watvapleaf-watvapsurrounding)*glw)/(2*molheat*gr*(0.5*gbhFo+0.5*gbhFree)+s*latheatvap*glw)
diff_T <- Ta-Initial_Tl
LE_L <- 2*leafemiss*sigma* (Initial_Tl)^4 #calculate long wave emission from leaf, unit W m⁻2
SHF_L<- 2*molheat * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000) #sensible heat flux from leaf, unit W m⁻2  
LHF_L<- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1))) #latenet heat flux , usingSVP() to calculate saturation water pressure in package"humidity"
plot(SHF_L)
#2. Input matlab code
# select: air temperature TA_F (°C), vapor pressure deficit VPD_F (kPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (°C)
# wind speed WS_F (M s-1)
df1 <- df6%>%select(TIMESTAMP_START,TIMESTAMP_END,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F, TS_F_MDS_1,WS_F)

#change units from kPa to Pa and °C to K
df1 <- df1%>% 
  mutate(tair = TA_F+273.15)%>% 
  mutate(VPD_F = VPD_F*1000)%>% 
  mutate(patm = PA_F*1000)%>% 
  mutate(u = WS_F)%>% 
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F)

#atmospheric parameters

#select only necessary columns
atmo <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,patm,tair,VPD_F)

# Specific heat of air at constant pressure (J/mol/K)
cpair <- 29.2 

atmos <- list(atmo,cpair)

#physical constants
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(tfrz,mmh2o,sigma)

#leaf propperties
emiss <- 0.96                        #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4
qa <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,TIMESTAMP_END,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection 
# gbh = (Nu*ρm*Dh)/dl 

# required parameters:
# pm         #molar density (mol m-3) 
#ideal gas law: p = atmo$patm/R*atmo$tair   gas constant R: 0.167226 (J/Kg K)
# dl         #leaf dimension characteristic length (cm)
# Dh         #diffusivity of heat Dh (m^2 s–1)
# Nu         #Nusselt number for forced conv. 
#             Nu = 0,66* Pr^0,33* Re^0,5
# Pr         #Prandtl number (molecular diffusion properties of the fluid)
#             Pr = ν/Dh
# Re         #Reynolds number 
#             Re = u*dl/ν
# u          #wind speed (M s-1)

# constants 

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0°C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0°C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s–1) at 0°C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s–1) at 0°C
R <- 0.167226      # gas constant R: 0.167226 (J/Kg K)

conduct1 <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u)%>%
  mutate(pm = patm/R*tair)%>%             # molar density (mol m-3) according to ideal gas law ???
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))      # diffusivity of momentum (m^2 s-1)

conduct2 <- conduct1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u,pm,Dh,v)%>%
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)             # Boundary layer conductance for heat (mol/m2 leaf/s)

#boundary layer conductance for water vapor gbw (mol m-2 s-1) 
#              gbw = 0,036* Sc^0,33* Re^0,50
# Sc         #Sherwood number
#             Sc = ν/Dj

#constants
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0°C (Mm² s-1)

conduct3 <- conduct2%>%
  mutate(DW = DW0*(P0/patm)*(tair/T0)) #Molecular diffusivity of mass H2o (Mm² s-1)
conduct4 <- conduct3%>%
  mutate(Sc = v/DW)  #Sherwood number
conduct5 <- conduct4%>%
  mutate(gbw = 0.036*Sc^0.33*Re^0.50) #boundary layer conductance for water vapor (mol m-2 s-1) 

conduct <- conduct5%>%
  select(TIMESTAMP_START,TIMESTAMP_END,gbh,gbw)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)  
gs <- seq(from=-0.2,to=0.1,length.out = 100)

#merge data in one list
flux <- list(qa,conduct,gs)

alldata<-list(atmos,physcon,leaf, flux)

#3. Functions matlab code
#function of Saturation vapor pressure and temperature derivative -- satvap()
satvap2 <- function (tc) {   
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
  return(list(esat, desat))}

#4. Iterations matlab code
# --- Newton-Raphson iteration until leaf energy balance is less than
# f0_max or to niter_max iterations

niter <- 0 # Number of iterations
f0 <- 1e36 # Leaf energy balance (W/m2)

niter_max <- 100 # Maximum number of iterations
f0_max <- 9  # Maximum energy imbalance (W/m2)

fluxtleaf <- Initial_Tl #initial leaf temperature

psych <- 66.5 #psychromatic constant, unit Pa K–1

while ((niter < niter_max) & (abs(mean(f0, na.rm = TRUE)) > f0_max)){
  abs(mean(f0, na.rm = TRUE))
  # Increment iteration counter
  
  niter <- niter + 1
  
  # Latent heat of vaporization (J/mol)
  lambda_val <- latheatvap           # J/g -> J/kg -> J/mol
  
  # Saturation vapor pressure (Pa) and temperature derivative (Pa/K)
  
  esat <- 167.803645898318
  desat <- 15.4639490550184
  
  # Leaf conductance for water vapor (mol H2O/m2/s)
  
  gleaf <- gsw * gbw / (gsw + gbw)
  
  # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
  
  flux$lwrad <- 2 * leafemiss * physcon$sigma * fluxtleaf^4
  dlwrad <- 8 * leafemiss * sigma * fluxtleaf^3
  
  # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$shflx <- 2 * cpair * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000)
  dshflx <- 2 * molheat * (gbhFo*0.5+gbhFree*0.5)
  
  # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$lhflx <- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1)))
  dlhflx <- lambda_val / Pa * desat * gleaf
  
  # Energy balance (W/m2) and temperature derivative (W/m2/K)
  
  f0 <- Qa - flux$lwrad - flux$shflx - flux$lhflx
  df0 <- -dlwrad - dshflx - dlhflx
  # Change in leaf temperature
  
  dtleaf <- (-f0 / df0)
  
  # Update leaf temperature
  
  fluxtleaf4 <- fluxtleaf + dtleaf
  
}

#3.2 fixed latent heat of vaporization
time <- seq(1,17520)
Initial_Tl_i <- rep(0.5/1000, length(time)) #example value, dont forget units
psych <- 66.5 #psychromatic constant, unit Pa K–1
latheatvap <- 2260 #latent heat of vaporisation, unit kJ/kg, constant
watvapsurrounding <- (df6$VPD_F*(df6$RH/100))/(1-(df6$RH/100)) #water vapor pressure of surrounding air, unit hPa
Ta <- df6$TA_F + 273.15 #temperature of surrounding air, unit K
Pa <- df6$PA_F #unit hPa, air pressure of surrounding air
u<- df6$WS_F #wind speed, unit m/s
molheat <- 29.2 #molar specific heat of moist air, unit J mol-1 K-1, constant
leafemiss <- 0.96 #, leaf emissivity, unit ratio
dl <- 6 #characteristic leaf dimension, unit cm
sigma <- 5.76*(10^-8) #Boltzman constant, unit W m^-2 K^-4
g <- 9.80665 #gravtation, unit m/s-2
gsw<- rep(300, 17520) #stomatal conductance, unit mmol m-2 s-1
Qa_max <- 1e36
Initial_Tl <- rep(NA, length(time)) #define intial_value as state variable, necessary?
f0_max <- 1e-6
f1 <- Qa-LE_L-SHF_L-LHF_L
niter <- 0
niter_max <- 100

#formulas Felix code
pm<- Pa/(8.314 + Ta) #molar density, unit mol m-3
Theta_a <- Ta*((1000/Pa)^0.286) #equation potential temperature of air
physv <- (13.3*(10^-6)*(1013.25/Pa))*((Ta/273.15)^1.81) #molar diffusivity for momentum, unit M2 s-1
Dh <- (18.9*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) 
satvap <- SVP(Ta, isK = TRUE, formula = c("Clausius-Clapeyron", "Murray")) #saturation vapor pressure of free air at Ta, unit hPa
watvapleaf <- rep(5,17520) #example value, real function function(Ta){(6.113*exp(5423((1/273.15)-(1/Ta))))/Pa}, uni hPa
Qa <- 3.15*((0.52+0.13/60*abs(53)+(0.082-0.03/60*abs(53))*sqrt(watvapleaf))*(5.67e-8*(Ta)^4))
Dj <- (21.8*(10^-6))*(1013.25/Pa)*((Ta/273.15)^1.81) #molecular diffusivity of mass, unit Mm? s-1
gr <- (4*leafemiss*sigma*Ta^3)/molheat #radiative conductance to heat transfer, unit  W/m^2?
Gr <- g*(dl^3)*max(300-Ta)/(Ta*(physv^2)) #Grashof number, dimensionless
PrFo <- physv/Dh #Prandtl number forced convection, dimensionless
ReFo <- u*dl/physv #Reynolds number forced convection, dimensionless
NuFo <- 0.66*PrFo^0.33*ReFo^0.5 #Nusselt number forced convection, dimensionless
gbhFo <- (NuFo*pm*Dh)/dl #gbh forced convection, dimensionless
NuFree <- 0.54*(PrFo^0.25)*(Gr^0.25) #Nusslet number free convection, dimensionless
gbhFree <- (Dh*NuFree)/(dl*pm)
Sc <- physv/Dj #Sherwood number
gbw <- 0.036*(Sc^0.33)*(ReFo^0.50)
glw <- 1/((1/gbw) + (1/gsw))
s1 <- 10e-15*2264705*(watvapsurrounding)^2*(461.52*(Ta)^2) #example equation, real function delta(watvapsurrounding)/delta(Ta), unit dimensionless?
s <- rep(-10, 17520)
Initial_Tl <- Ta+((Qa-2*leafemiss*sigma*(Ta^4))-latheatvap*(watvapleaf-watvapsurrounding)*glw)/(2*molheat*gr*(0.5*gbhFo+0.5*gbhFree)+s*latheatvap*glw)
diff_T <- Ta-Initial_Tl
LE_L <- 2*leafemiss*sigma* (Initial_Tl)^4 #calculate long wave emission from leaf, unit W m⁻2
SHF_L<- 2*molheat * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000) #sensible heat flux from leaf, unit W m⁻2  
LHF_L<- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1))) #latenet heat flux , usingSVP() to calculate saturation water pressure in package"humidity"
plot(SHF_L)
#2. Input matlab code
# select: air temperature TA_F (°C), vapor pressure deficit VPD_F (kPa) and air pressure PA_F (kPa)
# incoming solar SW_IN_F and longwave radiation LW_IN_F (W m-2), soil Temp.in 2cm depth TS_F_MDS_1 (°C)
# wind speed WS_F (M s-1)
df1 <- df6%>%select(TIMESTAMP_START,TIMESTAMP_END,TA_F,VPD_F,PA_F,SW_IN_F,LW_IN_F, TS_F_MDS_1,WS_F)

#change units from kPa to Pa and °C to K
df1 <- df1%>% 
  mutate(tair = TA_F+273.15)%>% 
  mutate(VPD_F = VPD_F*1000)%>% 
  mutate(patm = PA_F*1000)%>% 
  mutate(u = WS_F)%>% 
  mutate(Ts = TS_F_MDS_1+273.15)%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,VPD_F,patm,u,Ts,SW_IN_F,LW_IN_F)

#atmospheric parameters

#select only necessary columns
atmo <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,patm,tair,VPD_F)

# Specific heat of air at constant pressure (J/mol/K)
cpair <- 29.2 

atmos <- list(atmo,cpair)

#physical constants
tfrz <- 273.15                          # Freezing point of water (K)
mmh2o <- 0.01801528                     # Molecular mass of water (kg/mol)
sigma <- 5.670374419*10^-8              # Stefan-Boltzmann constant (W/m2/K4)
physcon <- data.frame(tfrz,mmh2o,sigma)

#leaf propperties
emiss <- 0.96                        #leaf emissity
leaf <- data.frame(emiss)

#fluxes

# Leaf radiative forcing qa (W/m2)
# radiative forcing Qa = incoming shortwave - 15% + incoming longwave radiation + longwave soil (Boltzman law)
# longwave radiation soil = 0.97*Boltzman constant (sigma)* soil temperature (K)^4
qa <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,Ts,SW_IN_F,LW_IN_F)%>%
  mutate(LW_S = 0.97*physcon$sigma*Ts^4)%>% # Longwave radiation from soil
  mutate(qa = SW_IN_F-0.15*SW_IN_F + LW_IN_F + LW_S)%>% # radiative forcing
  select(TIMESTAMP_START,TIMESTAMP_END,qa)

### boundary layer conductancee for heat gbh (mol/m2 leaf/s) for FORCED convection 
# gbh = (Nu*ρm*Dh)/dl 

# required parameters:
# pm         #molar density (mol m-3) 
#ideal gas law: p = atmo$patm/R*atmo$tair   gas constant R: 0.167226 (J/Kg K)
# dl         #leaf dimension characteristic length (cm)
# Dh         #diffusivity of heat Dh (m^2 s–1)
# Nu         #Nusselt number for forced conv. 
#             Nu = 0,66* Pr^0,33* Re^0,5
# Pr         #Prandtl number (molecular diffusion properties of the fluid)
#             Pr = ν/Dh
# Re         #Reynolds number 
#             Re = u*dl/ν
# u          #wind speed (M s-1)

# constants 

# constants needed for diffusivity calculation (from 29.0 appendices in Eco-Atmo Bonan)
T0<- 273.15  # Temperature 0°C in Kelvin
P0 <- 101325 # air Pressure (Pa) at 0°C
Dh0 <- 18.9*10^(-6) # reference diffusivity of heat Dh (m^2 s–1) at 0°C
v0 <- 13.3*10^(-6) # reference diffusivity of momentum v (m^2 s–1) at 0°C
R <- 0.167226      # gas constant R: 0.167226 (J/Kg K)

conduct1 <- df1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u)%>%
  mutate(pm = patm/R*tair)%>%             # molar density (mol m-3) according to ideal gas law ???
  mutate(Dh = Dh0*(P0/patm)*(tair/T0))%>% # diffusivity of heat (m^2 s-1) 
  mutate(v = v0*(P0/patm)*(tair/T0))      # diffusivity of momentum (m^2 s-1)

conduct2 <- conduct1%>%
  select(TIMESTAMP_START,TIMESTAMP_END,tair,patm,u,pm,Dh,v)%>%
  mutate(Pr = v/Dh)%>%                    # Prandtl number
  mutate(Re = u*dl/v)%>%                  # Reynolds number 
  mutate(Nu = 0.66*Pr^0.33*Re^0.5)%>%     # Nusselt number for forced conv.
  mutate(gbh = (Nu*pm*Dh)/dl)             # Boundary layer conductance for heat (mol/m2 leaf/s)

#boundary layer conductance for water vapor gbw (mol m-2 s-1) 
#              gbw = 0,036* Sc^0,33* Re^0,50
# Sc         #Sherwood number
#             Sc = ν/Dj

#constants
DW0 <- 21.8*10^(-6)       #Molecular diffusivity of mass H2o at 0°C (Mm² s-1)

conduct3 <- conduct2%>%
  mutate(DW = DW0*(P0/patm)*(tair/T0)) #Molecular diffusivity of mass H2o (Mm² s-1)
conduct4 <- conduct3%>%
  mutate(Sc = v/DW)  #Sherwood number
conduct5 <- conduct4%>%
  mutate(gbw = 0.036*Sc^0.33*Re^0.50) #boundary layer conductance for water vapor (mol m-2 s-1) 

conduct <- conduct5%>%
  select(TIMESTAMP_START,TIMESTAMP_END,gbh,gbw)

# Leaf stomatal conductance (mol H2O/m2 leaf/s)  
gs <- seq(from=-0.2,to=0.1,length.out = 100)

#merge data in one list
flux <- list(qa,conduct,gs)

alldata<-list(atmos,physcon,leaf, flux)

#3. Functions matlab code
#function of Saturation vapor pressure and temperature derivative -- satvap()
satvap2 <- function (tc) {   
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
  return(list(esat, desat))}

#4. Iterations matlab code
# --- Newton-Raphson iteration until leaf energy balance is less than
# f0_max or to niter_max iterations

niter <- 0 # Number of iterations
f0 <- 1e36 # Leaf energy balance (W/m2)

niter_max <- 100 # Maximum number of iterations
f0_max <- 9  # Maximum energy imbalance (W/m2)

fluxtleaf <- Initial_Tl #initial leaf temperature

psych <- 66.5 #psychromatic constant, unit Pa K–1

while ((niter < niter_max) & (abs(mean(f0, na.rm = TRUE)) > f0_max)){
  abs(mean(f0, na.rm = TRUE))
  # Increment iteration counter
  
  niter <- niter + 1
  
  # Latent heat of vaporization (J/mol)
  lambda_val <- latheatvap           # J/g -> J/kg -> J/mol
  
  # Saturation vapor pressure (Pa) and temperature derivative (Pa/K)
  
  esat <- 167.803645898318
  desat <- 15.4639490550184
  
  # Leaf conductance for water vapor (mol H2O/m2/s)
  
  gleaf <- gsw * gbw / (gsw + gbw)
  
  # Emitted longwave radiation (W/m2) and temperature derivative (W/m2/K)
  
  flux$lwrad <- 2 * leafemiss * physcon$sigma * fluxtleaf^4
  dlwrad <- 8 * leafemiss * sigma * fluxtleaf^3
  
  # Sensible heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$shflx <- 2 * cpair * (Ta-Initial_Tl)*(gbhFree*0.5+gbhFo*0.5*runif(17520, min=0.1, max=0.9)*4000)
  dshflx <- 2 * molheat * (gbhFo*0.5+gbhFree*0.5)
  
  # Latent heat flux (W/m2) and temperature derivative (W/m2/K)
  
  flux$lhflx <- -(molheat/psych)*((watvapsurrounding-satvap)/((gbw^-1)+(gsw^-1)))
  dlhflx <- lambda_val / Pa * desat * gleaf
  
  # Energy balance (W/m2) and temperature derivative (W/m2/K)
  
  f0 <- Qa - flux$lwrad - flux$shflx - flux$lhflx
  df0 <- -dlwrad - dshflx - dlhflx
  # Change in leaf temperature
  
  dtleaf <- (-f0 / df0)
  
  # Update leaf temperature
  
  fluxtleaf5 <- fluxtleaf + dtleaf
  
}
plot(fluxtleaf2-fluxtleaf3, ylab="residuals fix-random latent heat of vaporization")

#6. model calibration using a training model
#calculate leaf temperature with sensible heat flux
Initial_Tl2 <- Ta-((df6$H_F_MDS)/((2*molheat*(0.5*gbhFo*1000))+(2*molheat+0.5*gbhFree*1000))) #actual leaf temperature calculated from given sensible heat flux
plot(Initial_Tl2)
plot(df6$H_F_MDS-flux$shflx)
print(df6$H_F_MDS-flux$shflx)
plot(fluxtleaf-Initial_Tl2)

#plot random parameters against residuals
#run respective code chunks again before plotting
plot(x=leafemiss, y=(fluxtleaf-Initial_Tl2), xlab="random leaf emmissivity", ylab="residuals model output/Leaf temp")
mean(fluxtleaf-Initial_Tl2, na.rm = TRUE)
plot(x=dl, y=(fluxtleaf2-Initial_Tl2), xlab="random leaf dimension", ylab="residuals model output/Leaf temp")
mean(fluxtleaf2-Initial_Tl2, na.rm = TRUE)
plot(x=latheatvap, y=(fluxtleaf4-Initial_Tl2), xlab="random lat heat vaporization", ylab="residuals model output/Leaf temp")
mean(fluxtleaf4-Initial_Tl2, na.rm = TRUE)
