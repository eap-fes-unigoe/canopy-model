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
source("photosynthesis_stomatalconductance/fun_calc_an_gs.R")
#source("leafTemperature/fun_calc_LeafTemperature.R")

## Load initial state ----
## This should be a dataframe with all the state variables and one row with initial values

initial_state <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Create output dataframe. Copy of initial_state but more variables can be added.
out <- initial_state

# Source setup scripts for different model components
source("photosynthesis_stomatalconductance/setup_Photosynthesis_StomatalConductance.R")
#source("leafTemperature/setup_LeafTemperature.R")

#function of Latent heat of vaporization for leaf temperatre group
#source("latvap.R")

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
  an_gs <- fun_calc_an_gs(met,state_last,pars,out[n,])
  out[n, names(an_gs)] <- an_gs

  #Calculate leaf temperature and latent and sensible heat fluxes
  #Leafflux <- LeafTemperature(pars, state_last, vars_LeafTemperature)
  #for(i in 1:length(flux$Date.Time)){out[n,flux$Date.Time[i]] <- flux[i]}

  # Calculate plant C pools, soil decomposition and soil C pools
  #Cpools <- fun_calc_Cpools(pars, state_last, Cpools, vars_Cpools, fun_kmod_Ms, fun_kmod_Ts, site)
  #for(ipool in 1:length(names_Cpools)) {out[n, names_Cpools[ipool]] <- Cpools[ipool]}

  pb$tick() # update progress bar
}

# some display settings
options(scipen = 999) # turning off "e" notations
par(mfrow = c(1,2)) # 2 visible plots

#An = would optimally be compared with NPP
#fluxes$gpp  - fluxes$reco - fluxes$nee
# units are m-2 ground s-1
plot(out$ag)
plot(out$an)
plot(out$gs)
#plot(out$rd)

# unit conversions
gpp_umol = fluxes$gpp /(12 / 1000000 / 1000 * 3600) # kg m-2 ground? dt-1 to µmol m-2 ground s-1 reversion
an_kg = out$an * 12 / 1000000 / 1000 * 3600 #µmol m-2 ground h-1
an_leaf = out$an / 5 # an µmol m-2 leaf s-1
gs_leaf = out$gs * 5  # gs mol h2o m-2 leaf s-1, base is 0.01
plot(an_leaf)
plot(gs_leaf)

# comparing an and gpp

# in kg per day
plot(an_kg, ylab = "an kg CO2/m2 ground/h",ylim = c(0,0.003)) #photosynthesis in kg
plot(fluxes$gpp, ylab = "GPP kg CO2/m2 ground/h") #gpp in kg
plot(an_kg,fluxes$gpp)
#plot(an_kg,fluxes$nee)

# in umol per second
plot(out$an, ylab = "an umol CO2/m2 ground/s")#,ylim = c(0,30)) #photosynthesis in umol
plot(gpp_umol,ylab = "GPP umol CO2/m2 ground/s") #gpp in umol
plot(out$an, gpp_umol)
plot(out$ag, gpp_umol) #ag is gross photosynthesis rate
plot(out$an, out$ag)
#plot(input$p)

#comparing par from radiation gropu and from par function
#plot(out$apar, out$apar2)
#plot(out$apar, fluxes$r)

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

#testing for differences...of just the one parameter...can this be used for calibration in any way?
#why not?
#resid <- pars$g1 - coef(g1_lm)[2] # residual calculation after unit conversion

# g1 [at time step in calibration] - coef(g1_lm)...hm could that work?

write.csv(out,file = "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/hainich_july_60_9")
saveRDS(out, file = "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/photosynthesis_hainich_year_data_60_9.rds")
#### Plotmaker ####

Filename <- "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/Photosynthesis Model Outputs_General.pdf"
Plottitle <- "Photosynthesis Model Output General"
Sitedata = data.frame(c(input,fluxes))
source("photosynthesis_stomatalconductance/plotmaker_ps_sc.R")
plotmaker_ps_sc(out)


