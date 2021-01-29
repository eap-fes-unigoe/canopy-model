## setup soiltemperature calibration

# input data from hainich forest
input.data <- read.csv("Hainich_2018_input.csv", header = TRUE, sep = ",")

# required variables from measured data
# Temperature of air


# parameter without parameter for calibration
param_mincalib <- read.csv("Hainich_2018_input.csv", header = TRUE, sep = ",")


Depth <- 0.6    #Depth of soil layer
theta.sat <- 0.482 # m3 m-3 Volumetric water content at saturation value taken from Bonan p. 120 (soil defined as clay based on grain size of climate data)
#theta.liq <- 0.35  # This value needs to be taken from the outcomes of soil moisture model
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


# parameter to be calibrated
# Cv at theta.sat -> Volumetric water content at saturation [m3 m-3]; value taken from Bonan p. 120 (soil defined as clay based on grain size of climate data)



# calculations
Cv  = ((1-theta.sat)+(theta.liq*Cvwat))*1000000 #MJ m-3 k-1 * 1000000 to change to J

