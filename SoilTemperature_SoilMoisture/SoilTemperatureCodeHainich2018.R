#Here the start of the code that we created to model the temperature of soil, including 
# other calculations along the process. First we start indicating the data that we have 
#available to use coming from the file (FLX_DE-Hai). After this, we cancel all the values represented as -9999
#since this is not available data. 

rm(list = ls())

  input.all <- read.csv("Hainich_2018_input.csv", header = TRUE, sep = ",")
    input.all[input.all == -9999] <- NA

  input.var <- input.all

  Tsubsoil <- mean(input.all$TA_F) + 273.15

    View(input.all)
rm(input.all)

#Here inputs needed are indicated to later be able to implement them within the formulas

Depth <- 0.6    #Depth of soil layer
theta.sat <- 0.482 # m3 m-3 Volumetric water content at saturation value taken from Bonan p. 120 (soil defined as clay based on grain size of climate data)
theta.liq <- 0.35  # This value needs to be taken from the outcomes of soil moisture model
SoilBD <- 1.2 # g cm-3
Tsoil0 <- 286 # K Initial Temperature of soil
tstep <- 1800 # time step in seconds to change time interval

Cvwat <- 4.19  #MJ
Cvsoil <- 1.926 #MJ

#Thermal conductivity of dry soils 

Kdry <- ((0.135*SoilBD)+ 64.7)/(2700 -(0.947*SoilBD))

Kwat <- 0.57 #W/m*K units and this is a representative value given by theory
Kq <- 7.7 #W/m*K    Value from theory chapter 5
q <- 3.0 #W/m*K     
Ko <- 2.0 #W/m*K

#The thermal conductivity of soil solids varies with the quartz content of soil. 
#The Johansen method described by Farouki:

Ksol <- Kq^q * Ko^1 - q

#Thermal conductivity of saturated soil for unfrozen condicitions
Ksat <- (Ksol^(1 - theta.sat)) * Kwat^theta.sat 
#dimensionless Kersten number Ke
Se <- 0.06  #0.06 makes reference to the coarse-texture soil (Se>0.05)
Ke <- 1.0 + 0.7*log10(Se) 

#Finally, after calculating all components Thermal conductity of soil is calculated

K <- Kdry + (Ksat- Kdry)* Ke # W m-1 K-1                                         

Tsoil <- Tsoil0

#Following Heat capacity of our soil is calculated using equation 5.32


Cv  = ((1-theta.sat)+(theta.liq*Cvwat))*1000000 #MJ m-3 k-1 * 1000000 to change to J

for(i in 1:nrow(input.var)) {
  
  Tair <- input.var$TA_F[i] + 273.15 #theta air reference height (K) it may be zero i think
  G <- K / (Depth / 2) * (Tair - Tsoil[i]) # W m-3 s-1  Equation 5.22 taken from Bonan p. 69 used for calculation of heat flux going into soil
  S <- K/(Depth / 2) * (Tsubsoil - Tsoil[i]) # W m-3 s-1 Same equation used for the calculation of the heat flux at the botton of the soil layer.
  
  
  Tsoil[i+1]  <- Tsoil[i] + ( G / (Cv * Depth) + S / (Cv * Depth) ) * tstep # K units, Equation 5.15 used for the calculation of change of T in soil
}
# create function

source("fun_soiltemperature.R")

output <- get_soiltemp(input.test = input.test, stat.var = stat.var, param = param)

plot(Tsoil, main = 'Change of T in soil',sub = 'Year 2017',xlab = '(30 min Intervals/Days)',ylab = 'Temperature soil (K)')

Tairk <- input.var$TA_F+273.15
plot(Tairk, main = 'Change of air T',sub = 'Year 2017',xlab = '(30 min Intervals/Days)',ylab = 'Air T (K)')

