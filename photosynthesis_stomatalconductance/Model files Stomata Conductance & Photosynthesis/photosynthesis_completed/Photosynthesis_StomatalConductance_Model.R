#StomatalConductanceLeafPhotosynthesis = function(flux,leaf,physcon,atmos){


# call these:
source("sp_12_02.R")
source("satvap.R")
source("LeafPhotosynthesis.R")
source("PAR.R")

# read in Data
Hainich5Days = read.csv("5_days.csv",header = T, dec = ",", sep = ";")
#Hainich5Days = read.csv("5days_winter.csv",header = T, dec = ",", sep = ";")
iter = length(Hainich5Days$Date.Time)

#create empty lists
flux = list()
leaf = list()
physcon = list()
atmos = list()

#Create physical constants, inital atmos values, leafphysiology params, inital fluxes
InitalValues = sp_12_02(flux,leaf,physcon,atmos)
flux = InitalValues[[1]]
leaf = InitalValues[[2]]
physcon = InitalValues[[3]]
atmos = InitalValues[[4]]

# Read input data from read in Data
atmos$tair_i = Hainich5Days$TA_F + physcon$tfrz #air temperature in Kelvin
atmos$co2air_i = Hainich5Days$CO2 #! Atmospheric CO2 (umol/mol)
atmos$relhum_i = Hainich5Days$RH
atmos$eair_i = Hainich5Days$ea * 1000
#atmos$wind_i = Hainich5Days$WS_F ! Wind speed (m/s)
#atmos$patm_i = Hainich5Days$PA_F * 1000 ! Atmospheric pressure (Pa)
#atmos$irsky_i = Hainich5Days$LW_IN_F  #from group 3 #! Atmospheric longwave radiation (W/m2)

# short wave radiation testing  values

#atmos$swsky_i = Hainich5Days$SW_IN_F    #from group 3 #! SW radiation (W/m2)
fsds = Hainich5Days$SW_IN_F    #from group 3 #! SW radiation (W/m2)
#atmos$swsky[params$vis] = 0.5 * atmos$swr;   # short wave sky
#atmos$swsky[params$nir] = 0.5 * atmos$swr;   # short wave sky

# Vapor pressure (Pa) and specific humidity (kg/kg)
#atmos$eair = 0.61094*exp((17.625*atmos$tair_i)/(atmos$tair_i+243.04))  # magnus_formula - use satvap instead? ! Vapor pressure of air (Pa)
#atmos$qair = physcon$mmh2o / physcon$mmdry * atmos$eair / (atmos$patm - (1 - physcon$mmh2o/physcon$mmdry) * atmos$eair);  #! specific humidity (kg/kg)


# Test values for PAR
#   flux$apar           ! Leaf absorbed PAR (umol photon/m2 leaf/s)
#flux$apar_i = flux$apar
flux$apar_i = PAR(leaf,fsds)



# Test value leaf temperature
flux$tleaf_i = atmos$tair_i;

#####

# eair calculation
#for (i in 1:240){
#  esat[i] = satvap (atmos$tair_i[i]-physcon$tfrz); # vector
#  atmos$eair_i[i] = esat[i] * (atmos$relhum_i[i] / 100); #! Vapor pressure of air (Pa)
#}

#creating output files for an and gs
output_an = c()
output_gs = c()

#running time loop over five days,
#calculating an and gs with different values for co2air, tair, tleaf, apar, eair
# _i are the vectors from the input data,
# without _i are the singular value taken from those vectors at the specific timestep i
# add _i to list input() instead ?

# length of dataset

for (i in 1:iter){

# testing shorter loop times
#for (i in 1:50){
  # loop counter
  print(i)

  #co2air and tair for every time step
  atmos$co2air = atmos$co2air_i[i]
  atmos$tair = atmos$tair_i[i]

  #tleaf from group 4
  flux$tleaf = flux$tleaf_i[i]

  # Entropy for rd, vcmax, jmax in combination with tair (J/mol/K)
  # move these inside LeafPhotosynthesis.R - done
  # leaf$rdse =
  leaf$vcmaxse = 668.39 - 1.07 * atmos$tair
  leaf$jmaxse = 659.7 - 0.75 * atmos$tair

  # add radiation PAR (from group 3)
  flux$apar = flux$apar_i[i]
  #print(flux$apar)
  # testing PAR values == hainich shortwave radiation
  #flux$apar = atmos$swsky_i[i]
  #print(flux$apar)

  # loop for eair (not working right now, stopping at loop 180 with:
  # "Fehler in if (flux$an > 0) { : Argument hat Länge 0 )"
  atmos$relhum = atmos$relhum_i[i]
  esat = satvap ((atmos$tair-physcon$tfrz));
  atmos$eair = esat * (atmos$relhum / 100); #! Vapor pressure of air (Pa)
  atmos$eair = atmos$eair_i[i]
  #print(esat)
  #print(atmos$relhum)
  #print(atmos$eair)

  # Calculation of an and gs
  flux = LeafPhotosynthesis(physcon, atmos, leaf, flux)

  # writing outputs
  output_an[i] = flux$an
  output_gs[i] = flux$gs
  #output_ci[i] = flux$ci

}

# create time vector
hainich_time = Hainich5Days$Date.Time

output_angs = data.frame(
  Time = hainich_time,
  an = output_an,
  gs = output_gs
  )

#write.csv(output_angs,file = "testoutputs8_Hanich_winter")

plot(output_an ~ output_gs)
#plot(output_an ~ atmos$tair_i)
#plot(output_an ~ atmos$eair)
#plot(output_an ~ Hainich5Days$NIGHT)
#plot(output_an ~ Hainich5Days$TIMESTAMP_END, type = "l")

#plot(output_gs ~ output_gs)
#plot(output_gs ~ atmos$tair_i)
#plot(output_gs ~ atmos$eair)
#plot(output_gs ~ Hainich5Days$NIGHT)
#plot(output_gs ~ Hainich5Days$TIMESTAMP_END, type = "l")


#return(flux)

#}

