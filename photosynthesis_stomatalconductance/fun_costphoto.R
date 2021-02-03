# fun_cost_soilmoisture.R


cost_photo <- function(pars_calib) {
  output <- fun_photosynthesis_calib(input = input, param_mincalib = param_mincalib, pars_calib = pars_calib, theta.in = fluxes$swc)

  resid <- output$an - fluxes$gpp
  resid <- resid[!is.na(resid)]

 # return(resid)
}

# run function

costphoto(pars_calib = pars_calib)
