fun_photosynthesis_calib <- function(input,initial_state,pars,pars_calib){

#source("setup_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/calc_fun_Photosynthesis_StomatalConductance.R")

#print(c("input",input))
pars$vcmax25 = pars_calib[1]
pars$g1 = pars_calib[2]
## Model run (for loop) ----
for(n in 1:length(input$time)) {
#for(n in 1:168) {

  print(n)
  if(n==1) {state_last <- initial_state[1,]} else state_last <- out[(n-1),] # state variable values at previous time step
  met <- input[n,]
  #site <- fluxes[n,]

  # Calculate radiative transfer

  # This is a really temporary workaround, before we calculate the tsoil and tleaf from the other submodels
  radiation_state <- list(t_leaf = met$tair, t_soil = met$tair)
  radiation <- fun_calc_radiative_transfer(met, radiation_state, pars, dt)
  out[n, names(radiation)] <- radiation

  # calculate photosynthesis and stomatal conductance
  state_last$tleaf <- met$tair
  ps_sc_sun <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sun * 4.6) # 1 W/m2 ≈ 4.6 μmole.m2/s ?
  ps_sc_sha <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sha * 4.6) # 1 W/m2 ≈ 4.6 μmole.m2/s ?
  photosynthesis_stomatalconductance <- ps_sc_sun
  photosynthesis_stomatalconductance$an <- ps_sc_sun$an * out[n,]$LAI_sunlit + ps_sc_sha$an * out[n,]$LAI-out[n,]$LAI_sunlit
  photosynthesis_stomatalconductance$gs <- ps_sc_sun$gs * out[n,]$LAI_sunlit + ps_sc_sha$gs * out[n,]$LAI-out[n,]$LAI_sunlit
  out[n, names(photosynthesis_stomatalconductance)] <- photosynthesis_stomatalconductance

  # update progress bar
  # pb$tick()

}

rm(met, site, state_last, names_Cpools, ipool)
# Write out output
# write.csv()
 return(out)
 #return(an)
}
