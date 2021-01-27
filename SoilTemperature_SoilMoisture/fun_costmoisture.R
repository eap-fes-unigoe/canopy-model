# fun_cost_soilmoisture.R

cost_soilmoisture <- function(pars_calib) {
  source("fun_soilmoisture.R")
  output <- get_theta_soil(input.test = input.test, stat.var = stat.var, param = param, pars_calib)

  resid <- output$theta - hainich_data$theta
  resid <- resid[!is.na(resid)]
}
