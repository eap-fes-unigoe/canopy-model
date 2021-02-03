# fun_cost_soilmoisture.R

# initial_state,pars,ps_sc)
cost_photo <- function(pars_calib) {
  output <- fun_photosynthesis_calib(input = input, initial_state = initial_state, pars = pars, ps_sc = ps_sc)

  resid <- output$an - fluxes$gpp
  resid <- resid[!is.na(resid)]

 # return(resid)
}

# run function

cost_photo(pars_calib = pars_calib)
