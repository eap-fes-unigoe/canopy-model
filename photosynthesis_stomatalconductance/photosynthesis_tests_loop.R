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
source("fun_calc_Cpools.R")
source("fun_calc_radiative_transfer.R")
source("photosynthesis_stomatalconductance/calc_fun_Photosynthesis_StomatalConductance.R")

## Load initial state ----
## This should be a dataframe with all the state variables and one row with initial values

initial_state <- read.csv("initial_state.csv")

## Initial calculations and variables ----

# Create output dataframe. Copy of initial_state but more variables can be added.
out <- initial_state

# Source setup scripts for different model components
#source("setup_Cpools.R")
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
  #site <- fluxes[n,]

  # Calculate radiative transfer

  #This is a really temporary workaround, before we calculate the tsoil and tleaf from the other submodels
  radiation_state <- list(t_leaf = met$tair, t_soil = met$tair)
  radiation <- fun_calc_radiative_transfer(met, radiation_state, pars, dt)
  out[n, names(radiation)] <- radiation

  # calculate photosynthesis and stomatal conductance
  # state_last$gbw <- 0.702
  state_last$tleaf <- met$tair # leaftemeperature placeholder
  #calculating photosynthesis and stomatal conductance for sunlit leaves.
  an_gs_sun <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sun*4.6) # 1 W/m2 ≈ 4.6 μmole.m2/s ?
  #calculating photosynthesis and stomatal conductance for shaded leaves
  an_gs_sha <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sha*4.6) # 1 W/m2 ≈ 4.6 μmole.m2/s ?
  ##conversion from per leaf area to per ground area by use of LAI and the proportion of each
  an_gs <- an_gs_sun
  an_gs$an <- an_gs_sun$an * out[n,]$LAI_sunlit + an_gs_sha$an * (out[n,]$LAI - out[n,]$LAI_sunlit)
  an_gs$gs <- an_gs_sun$gs * out[n,]$LAI_sunlit + an_gs_sha$gs * (out[n,]$LAI - out[n,]$LAI_sunlit)
  out[n, names(an_gs)] <- an_gs

  pb$tick() # update progress bar
}
# Write out output
#write.csv(out, file="testoutput_calib_07_2018.csv")

#}

#turning off "e" notations
options(scipen = 999)

# comparing An and gpp
gpp_umol = fluxes$gpp /(12 / 1000000 / 1000 * 3600) # kg m-2 (ground?) dt-1 to µmol m-2 (ground?) s-1 reversion
an_kg = out$an * 12 / 1000000 / 1000 * 3600 #µmol m-2 (leaf?) s-1
gpp_An_comparison = data.frame(An_kg = an_kg , GPP_kg = fluxes$gpp)
# compare LAI factor with gpp an
gpp_An_comparison$LAI = out$LAI
gpp_An_comparison$LAI_sun = out$LAI_sunlit
gpp_An_comparison$LAI_sha = out$LAI - out$LAI_sunlit
gpp_An_comparison$div_an_gpp = gpp_An_comparison$GPP/gpp_An_comparison$An
gpp_An_comparison$div_an_gpp_10 = gpp_An_comparison$div_an_gpp * 10
gpp_An_comparison$an_umol = out$an
gpp_An_comparison$gpp_umol = gpp_umol
#out$an-gpp_umol
#an_kg-fluxes$gpp
#summary(gpp_An_comparison)
#plot(out$an * (12 / 1000000 / 1000 * 3600),fluxes$gpp, xlim = c(0,0.003),ylim = c(0,0.003))
par(mfrow = c(1,2))
plot(an_kg, ylab = "an kg CO2/m2 ground/d",ylim = c(0,0.003)) #photosyntheis in kg
plot(fluxes$gpp, ylab = "GPP kg CO2/m2 ground/d",ylim = c(0,0.003)) #gpp in kg
plot(out$an, ylab = "an umol CO2/m2 ground/s")#,ylim = c(0,30)) #photosynthesis in umol
plot(gpp_umol,ylab = "GPP umol CO2/m2 ground/s") #gpp in umol
plot(gpp_An_comparison$div_an_gpp, ylim = c(-1,1))
plot(gpp_An_comparison$div_an_gpp_10, ylim = c(-10,10))
mean(gpp_An_comparison$div_an_gpp)
mean(gpp_An_comparison$div_an_gpp_10)
plot(an_kg,fluxes$gpp)
plot(out$an, gpp_umol)


#comüparing radiation values
rad_compare = data.frame(sim_rad_sha = out$ic_sha, sim_rad_sun = out$ic_sun, apar = apar)
rad_compare$div = rad_compare$apar/rad_compare$sim_rad_sun
rad_compare$adj_sun = rad_compare$sim_rad_sun * 4.6
rad_compare$adj_sha = rad_compare$sim_rad_sha * 4.6


#plot gs
plot(out$gs/5, ylab = "gs mol h2o m-2 leaf s-1") # in mol h2o m-2 leaf s-1, base is 0.01
plot(out$gs, ylab = "gs mol h2o m-2 ground s-1") # in mol h2o m-2 ground s-1, base is 0.01
plot(out$gs/5, out $an) # in mol h2o m-2 leaf s-1, base is 0.01
plot(out$gs, out $an) # in mol h2o m-2 ground s-1, base is 0.01
mean(out$an/(5)/(out$gs/5)) # slope of -5.97 -> similar to simulation
gs_without_0 <- out$gs[!out$gs < 0.1]
plot(gs_without_0)
an_wihtout_0 = out$an[!out$an < 0]
plot(an_wihtout_0)
plot(gs_without_0,an_wihtout_0)
#outputs: with 2 as conversion factor: up to 60 an in umol Co2m-2, should be in the are of 10-12, comparison off by a lot
# 4.6, as found online and is found in PAR.r, the matlab bonan script
out$an/out$gs

write.csv(out,file = "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/test_output_20_03_with_LAI_60_9_bad_values")
#### Plotmaker ####

Filename <- "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/Photosynthesis Model Outputs_General.pdf"
Plottitle <- "Photosynthesis Model Output General"
Sitedata = data.frame(c(input,fluxes))
source("photosynthesis_stomatalconductance/plotmaker_ps_sc.R")
plotmaker_ps_sc(out)


