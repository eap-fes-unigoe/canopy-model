# Function calculating soil moisture changes

get_soiltemp <- function(input, pars, initial_state) {


#Here inputs needed are indicated to later be able to implement them within the formulas

# SD <- 0.6    #SD of soil layer
# theta.sat <- 0.482 # m3 m-3 Volumetric water content at saturation value taken from Bonan p. 120 (soil defined as clay based on grain size of climate data)
#theta.liq <- 0.35  # This value needs to be taken from the outcomes of soil moisture model
# BD <- 1.2 # g cm-3
Tsoil0 <- initial_state$Tsoil # K Initial Temperature of soil
# dt <- 1800 # time step in seconds to change time interval
# 
# Cvwat <- 4.19  #MJ
# Cvsoil <- 1.926 #MJ
  
  list2env(pars, envir = environment())
  theta.liq <- fluxes$swc
#Thermal conductivity of dry soils 

Kdry <- ((0.135*BD)+ 64.7)/(2700 -(0.947*BD))


#The thermal conductivity of soil solids varies with the quartz content of soil. 
#The Johansen method described by Farouki:

Ksol <- (Kq^q) * (Ko^(1 - q))

#Thermal conductivity of saturated soil for unfrozen condicitions
Ksat <- (Ksol^(1 - theta.sat)) * Kwat^theta.sat
#dimensionless Kersten number Ke


#Following Heat capacity of our soil is calculated using equation 5.32

Tsoil <- rep(NA, nrow(input))

for(i in 1:nrow(input)) {
  
  #browser()
 
  Se <- theta.liq[i]/theta.sat #0.06 makes reference to the coarse-texture soil (Se>0.05)
  Ke <- 1.0 + (0.7*(log10(Se))) 
  
  #Finally, after calculating all components Thermal conductivity of soil is calculated
  
  K <- Kdry + (Ksat - Kdry) * Ke # W m-1 K-1                                         
  
  Cv  = (((1-theta.sat)*Cvsoil)+(theta.liq[i]*Cvwat))*1000000 #MJ m-3 K-1 * 1000000 to change to J m-3 K-1
  
  if(i==1){Tsoil.i <- Tsoil0} else{Tsoil.i <- Tsoil[i-1]}
  
  Tair <- input$tair[i]  #theta air reference height (K) it may be zero i think
  G <- K / (SD / 2) * (Tair - Tsoil.i) # W m-3 s-1  Equation 5.22 taken from Bonan p. 69 used for calculation of heat flux going into soil
  S <- K/(SD / 2) * (Tsubsoil - Tsoil.i) # W m-3 s-1 Same equation used for the calculation of the heat flux at the botton of the soil layer.
  
  
  Tsoil[i]  <- Tsoil.i + ( G / (Cv * SD) + S / (Cv * SD) ) * dt # K units, Equation 5.15 used for the calculation of change of T in soil

  
  }
output <- data.frame(Tsoil=Tsoil)
return(output)
}

