### Photosynthesis testing loop

##
rm(list=ls())

# Setting some time unit variables in unit seconds
dt <- 3600 # delta time: model time step

## Load parameters and adjust units ----
source("setup_parameters.R")

## Load and prepare input ----
source("setup_sitedata.R")

## Load functions ----
source("fun_calc_radiative_transfer.R")
source("photosynthesis_stomatalconductance/calc_fun_Photosynthesis_StomatalConductance.R")

## Load initial state ----
## This should be a dataframe with all the state variables and one row with initial values

initial_state <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Create output dataframe. Copy of initial_state but more variables can be added.
out <- initial_state

# Source setup scripts for different model components
source("photosynthesis_stomatalconductance/setup_Photosynthesis_StomatalConductance.R")

# Setup progress bar
library(progress)
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = length(input$time),
                       clear = FALSE) # set to true to remove bar after finishing

## Model run (for loop) ----
for(n in 1:length(input$time)) {

  if(n==1) {state_last <- initial_state[1,]} else state_last <- out[(n-1),] # state variable values at previous time step
  met <- input[n,]

  # Calculate radiative transfer

  #This is a really temporary workaround, before we calculate the tsoil and tleaf from the other submodels
  radiation_state <- list(t_leaf = met$tair, t_soil = met$tair)
  radiation <- fun_calc_radiative_transfer(met, radiation_state, pars, dt)
  out[n, names(radiation)] <- radiation

  # calculate photosynthesis and stomatal conductance for sunlit and shaded leaves
  #adjusting parameters for sensitiviy analysis
  pars$vcmax25 = 60
  pars$g1 = 13.5

  state_last$tleaf <- met$tair # leaf temeperature placeholder
  an_gs_sun <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sun*4.6) # 1 W/m2 ≈ 4.6 μmole.m2/s
  an_gs_sha <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sha*4.6) # 1 W/m2 ≈ 4.6 μmole.m2/s
  an_gs <- an_gs_sun # leaf m-2 -> ground m-2 by x LAI
  an_gs$an <- an_gs_sun$an * out[n,]$LAI_sunlit + an_gs_sha$an * (out[n,]$LAI - out[n,]$LAI_sunlit)
  an_gs$gs <- an_gs_sun$gs * out[n,]$LAI_sunlit + an_gs_sha$gs * (out[n,]$LAI - out[n,]$LAI_sunlit)
  out[n, names(an_gs)] <- an_gs
  pb$tick() # update progress bar
}

# some display settings
options(scipen = 999) # turning off "e" notations
par(mfrow = c(1,2)) # 2 visible plots

# unit conversions
gpp_umol = fluxes$gpp /(12 / 1000000 / 1000 * 3600) # kg m-2 (ground?) dt-1 to µmol m-2 (ground?) s-1 reversion
an_kg = out$an * 12 / 1000000 / 1000 * 3600 #µmol m-2 ground s-1
an_leaf = out$an/5 # an µmol m-2 leaf s-1
gs_leaf = out$gs/5  # gs mol h2o m-2 leaf s-1, base is 0.01
plot(an_leaf)
plot(gs_leaf)

# comparing an and gpp

# in kg per day
plot(an_kg, ylab = "an kg CO2/m2 ground/d",ylim = c(0,0.003)) #photosyntheis in kg
plot(fluxes$gpp, ylab = "GPP kg CO2/m2 ground/d",ylim = c(0,0.003)) #gpp in kg
plot(an_kg,fluxes$gpp)

# in umol per second
plot(out$an, ylab = "an umol CO2/m2 ground/s")#,ylim = c(0,30)) #photosynthesis in umol
plot(gpp_umol,ylab = "GPP umol CO2/m2 ground/s") #gpp in umol
plot(out$an, gpp_umol)


#plot gs
plot(out$gs, ylab = "gs mol h2o m-2 ground s-1") # in mol h2o m-2 ground s-1, base is 0.01
plot(out$gs, out$an) # in mol h2o m-2 ground s-1, base is 0.01

out$an/out$gs

# calculating slope g1  by use of regression model
#g1 = (gs - g0) / (An hs / ca)
gs_g0 = out$gs - pars$g0
an_hs_ca = (out$an * out$hs) / out$cs
plot(gs_g0 ~ an_hs_ca)

g1_lm = lm(gs_g0 ~ an_hs_ca)
#plot(g1_lm)
coef(g1_lm)
summary(g1_lm) # returns a slope of 8.83 -> seems possible
# g1 [at time step in calibration] - coef(g1_lm)...hm could that work?

write.csv(out,file = "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/sensitivity_60_13-5")
#### Plotmaker ####

Filename <- "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/Photosynthesis Model Outputs_General.pdf"
Plottitle <- "Photosynthesis Model Output General"
Sitedata = data.frame(c(input,fluxes))
source("photosynthesis_stomatalconductance/plotmaker_ps_sc.R")
plotmaker_ps_sc(out)


