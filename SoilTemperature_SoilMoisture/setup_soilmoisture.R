## setup soilmoisture


# load packages
library(dplyr)


# input data from hainich forest

input.data <- read.csv("D:/Universität/Master/git_clone/canopy-model/data/Hainich_2018_input.csv")

  # required variables from measured data
    # rh: relative humidity
    # prec: precipitation
    # temp.air: air temperature

prec <-                     # precipitation [mm 30min-1]
prec <- prec / 1000 *30*60  # precipitation [m s-1]

Rh <-                       # relative humidity [%]
Rh <- Rh / 100              # relative humidity

temp <-                     # air temperature [°C]
temp <- temp + 273          # air temperature [K]

  # required variables from other models
    #rn: net radiation (J m-2 s-1)
    #gs: ground heat flux (J m-2 s-1)


# parameter

param <- read.csv("Parameter.csv")

  # theta.sat:Volumetric water content at saturation [m3 m-3]; value taken from Bonan p. 120 (soil defined as clay based on grain size of climate data)
  # psi.sat:  Matric potential at saturation [m]; value taken from Bonan p. 120
  # k.sat:    Hydraulic conductivity at saturation [m s-1]; value taken from Bonan p. 120
  # BD:       bulk density [kg m-3]; from climate data
  # SOC:      soil organic carbon [kg m-2]; mean over layers from climate data
  # SD:       soil sampling depth/depth of soil layer [m]; from climate data
  # clay:     % of clay in the soil; mean over layers from climate data
  # PD:       particle density [kg m-3]; from literature [kg m-3]
  # B.om:     given by Lett et al. 2000 (in CLM4.5)
  # p:        air density (kg m-3)
  # psi.a:    water potential of air (Pa)
  # psi.fc:   soil water potential at field capacity (Pa)
  # b:        Exponent
  # cp:       specific heat of air (J kg-1)
  # lambda:   latent heat of vaporization (J kg-1)
  # MWrat:    ratio molecular weight of water vapor/dry air
  # ra:       aerodynamic resistance (s m-1)


# state variable

theta.in <-       # initial value for volumetric water content of soil [m3 m-3]


# calculations

ps <- 1 - (BD/PD)            # soil pore space [unitless]: bulk density = 1200 kg m-3 (climate data), particle density = 2650 kg m-3 (literature)
V <- 1 * 1 *SD               # volume of soil layer [m3]

  # for evaporation
gamma <- (cp * p) / (lambda * MWrat)  # psycrometric constant for potential evaporation [Pa K-1]

  # for drainage
B.min <- 2.91 + 0.159 * clay
f <- (SOC / (BD * SD)) * 1.72        # soil organic matter fraction; f = %C * 1.72
B <- (1 - f) * B.min + f * B.om
