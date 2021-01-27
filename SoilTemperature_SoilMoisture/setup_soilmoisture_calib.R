## setup soilmoisture calibration

# input data from hainich forest
input.data <- read.csv("D:/Universität/Master/git_clone/canopy-model/data/Hainich_2018_input.csv")

    # required variables from measured data
      # rh: relative humidity
      # prec: precipitation
      # temp.air: air temperature


# parameter without parameter for calibration
param_mincalib <- read.csv("")

psi.sat <- stat.var[1,2]      # Matric potential at saturation [m]; value taken from Bonan p. 120
k.sat <- stat.var[1,3]        # Hydraulic conductivity at saturation [m s-1]; value taken from Bonan p. 120
BD <- stat.var[1,4]           # bulk density [kg m-3]; from climate data
SOC <- stat.var[1,5]          # soil organic carbon [kg m-2]; mean over layers from climate data
SD <- stat.var[1,6]           # soil sampling depth/depth of soil layer [m]; from climate data
clay <- stat.var[1,7]         # % of clay in the soil; mean over layers from climate data
PD <- stat.var[1,8]           # particle density [kg m-3]; from literature [kg m-3]
B.om <- stat.var[1,9]         # given by Lett et al. 2000 (in CLM4.5)
p <- stat.var[1,10]           # air density (kg m-3)
psi.a <- stat.var[1,11]       # water potential of air (Pa)
psi.fc <-  stat.var[1,12]     # soil water potential at field capacity (Pa)
b <- param[1,1]               # Exponent
cp <- param[1,2]              # specific heat of air (J kg-1)
lambda <- param[1,3]          # latent heat of vaporization (J kg-1)
MWrat <- param[1,4]           # ratio molecular weight of water vapor/dry air


# parameter to be calibrated
  # theta.sat -> Volumetric water content at saturation [m3 m-3]; value taken from Bonan p. 120 (soil defined as clay based on grain size of climate data)
  # ra -> aerodynamic resistance (s m-1)


# calculations
ps <- 1 - (BD/PD)            # soil pore space [unitless]: bulk density = 1200 kg m-3 (climate data), particle density = 2650 kg m-3 (literature)
V <- 1 * 1 *SD               # volume of soil layer [m3]
