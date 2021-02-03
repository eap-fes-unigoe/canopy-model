# fun_cost_soilmoisture.R

cost_photosynthesis <- function(pars_calib) {
  output <- calc_fun_Photosynthesis_StomatalConductance(input = input, param_mincalib = param_mincalib, pars_calib = pars_calib, theta.in = fluxes$swc)

  resid <- output$theta - fluxes$swc
  resid <- resid[!is.na(resid)]
}

# run function

cost_soilmoisture(pars_calib = pars_calib)
