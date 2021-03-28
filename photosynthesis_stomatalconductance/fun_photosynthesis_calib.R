# photosynthesis calibration loop
fun_photosynthesis_calib <- function(input,initial_state,pars,pars_calib){

# setting the parameters to be calibrated to testing values
pars$vcmax25 = pars_calib[1]
pars$g1 = pars_calib[2]

## Calibration run (for loop) ----
for(n in 1:length(input$time)) {
#for(n in 1:24) {
  if(n==1) {state_last <- initial_state[1,]} else state_last <- out[(n-1),] # state variable values at previous time step
  met <- input[n,]

  # Calculate radiative transfer

  # This is a really temporary workaround, before we calculate the tsoil and tleaf from the other submodels
  radiation_state <- list(t_leaf = met$tair, t_soil = met$tair)
  radiation <- fun_calc_radiative_transfer(met, radiation_state, pars, dt)
  out[n, names(radiation)] <- radiation

  # calculate photosynthesis and stomatal conductance
  an_gs <- fun_calc_an_gs(met,state_last,pars,out[n,])
  out[n, names(an_gs)] <- an_gs
}

rm(met, state_last)
 return(out)
}
