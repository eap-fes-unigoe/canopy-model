# fun_cost_soilmoisture.R

# initial_state,pars,ps_sc)
cost_photo <- function(pars_calib) {
  output <- fun_photosynthesis_calib(input = input, initial_state = initial_state, pars = pars, pars_calib)
  # print(c("an",output$an))
  # print(c("an",output))
  # multiple to make an comparabale with gpp
  # resid <- output * (12 / 1000000 / 1000 * 3600) - fluxes$gpp
  resid <- output$an * (12 / 1000000 / 1000 * 3600) - fluxes$gpp
  resid <- resid[!is.na(resid)]
  # print(c("resid",resid))
  o = o + 1
  print(c("another_round", o))
  return(resid)
}

# run function

#res = cost_photo(pars_calib = pars_calib)
#res_list = as.list(res)
# 17:03
# started roughly 13:00 08.02.2021
# still running at 17:50
