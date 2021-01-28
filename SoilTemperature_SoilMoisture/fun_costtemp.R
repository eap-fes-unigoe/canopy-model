# fun_cost_soiltemperature.R

cost_soiltemperature <- function(pars_calib) {
  source("fun_soiltemperature.R")
  output <- get_soiltemp(input.test = input.test, stat.var = stat.var, param = param)
  
}
