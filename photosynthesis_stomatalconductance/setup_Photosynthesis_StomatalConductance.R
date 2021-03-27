# list for photosynthesis fluxes/state variables

flux = list()

#library("signal")
library("pracma") # for roots function

#load functions
source("photosynthesis_stomatalconductance/calc_fun_Photosynthesis_StomatalConductance.R")
source("photosynthesis_stomatalconductance/hybrid_root_ci.R")
source("satvap.R")
#source("photosynthesis_stomatalconductance/satvap.R")
# source("photosynthesis_stomatalconductance/CO2LeafBoundaryLayer.R") # get h2o from group 4
source("photosynthesis_stomatalconductance/LeafBoundaryLayer.R") # calculate h2o without group 4
#source("photosynthesis_stomatalconductance/PAR.R")
source("photosynthesis_stomatalconductance/brent_root_ci.R")
source("photosynthesis_stomatalconductance/CiFunc.R")

#adjusting parameters for sensitiviy analysis
#pars$vcmax25 = 60 #default: 60
#pars$g1 = 9 #default: 9
