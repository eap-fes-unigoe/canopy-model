## Plotting ##

library(ggplot2)
library(gridExtra)
library(tidyr)

Tsubsoil <- 283
output_cal <- get_soiltemp(input = input, initial_state = initial_state, pars = pars)

# Compare measured data to modeled data and pre-calibrated data

out_temp <- data.frame(fluxes$time, fluxes$tsoil, output_cal$Tsoil)
names(out_temp) <- c("time","MeasuredTsoil", "SoilTemperature")

gg_temp <- gather(out_temp, key = "type", value = "temp", SoilTemperature, MeasuredTsoil, factor_key = T)

ggplot() +
  geom_line(aes(x=gg_temp$time,y=gg_temp$temp, color = gg_temp$type), size = 1) +
  labs(y = "Soil Temperature [K]", x = "time", color = "")

