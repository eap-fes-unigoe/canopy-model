fun_photosynthesis_calib <- function(input,initial_state,pars,pars_calib){

#source("setup_photosynthesis_calib.R")
source("photosynthesis_stomatalconductance/calc_fun_Photosynthesis_StomatalConductance.R")

#print(c("input",input))
pars$vcmax25 = pars_calib[1]
pars$g1 = pars_calib[2]
## Model run (for loop) ----
for(n in 1:length(input$time)) {
#for(n in 10:15) {

  if(n==1) {state_last <- initial_state[1,]} else state_last <- out[(n-1),] # state variable values at previous time step
  met <- input[n,]
  #site <- fluxes[n,]

  # Calculate radiative transfer

  # This is a really temporary workaround, before we calculate the tsoil and tleaf from the other submodels
  radiation_state <- list(t_leaf = met$tair, t_soil = met$tair)
  radiation <- fun_calc_radiative_transfer(met, radiation_state, pars, dt)
  out[n, names(radiation)] <- radiation

  # calculate photosynthesis and stomatal conductance

  # tleaf substitute
  state_last$tleaf <- met$tair
  state_last$gbw <- 0.702
  # put in out?
  #print(c("input",input))
  #print(c("met",met))
  #photosynthesis_stomatalconductance_sun <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sun)
  photosynthesis_stomatalconductance_sun <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sun*out[n,]$LAI_sunlit)
  #state_last$PAR <- radiation$ic_sun
  #photosynthesis_stomatalconductance_sha <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sha)
  photosynthesis_stomatalconductance_sha <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,out[n,]$ic_sha*out[n,]$LAI-out[n,]$LAI_sunlit)
  photosynthesis_stomatalconductance <- photosynthesis_stomatalconductance_sun + photosynthesis_stomatalconductance_sha
  #photosynthesis_stomatalconductance$an <- photosynthesis_stomatalconductance$an * #LAI
  out[n, names(photosynthesis_stomatalconductance)] <- photosynthesis_stomatalconductance

  # update progress bar
  # pb$tick()

}

rm(met, site, state_last, names_Cpools, ipool)
# Write out output
# write.csv()
 return(out)
}
