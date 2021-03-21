cost_photo <- function(pars_calib) {
  output <- fun_photosynthesis_calib(input = input, initial_state = initial_state, pars = pars, pars_calib)
  resid <- output$an - fluxes$gpp/(12 / 1000000 / 1000 * 3600) # residual calculation after unit conversion
  resid <- resid[!is.na(resid)] # removal of NAs of the residuals

  # testing for g1 residuals?
  #resid <- output$an/output$gs - pars_calib$g1 does this make sense? no, the formula is more comples than that
  #resid <- resid[!is.na(resid)] # removal of NAs of the residuals

  print(Sys.time())
  return(resid)
}
