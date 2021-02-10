## Plotting ##

library(ggplot2)
library(gridExtra)
library(tidyr)


# 1. Sensitivity analysis

# plot sensitivity functions of model output to parameters

date <- fluxes[2][1:743, ]
par_sens <- cbind(date, Sens[, 3:7])

gg_df <- gather(par_sens, key = "type", value = "theta", epmod, b.sm, psi.n1, ra, theta.sat, factor_key = T)

ggplot() +
  geom_line(aes(gg_df$date, gg_df$theta, color = gg_df$type), size = 1) +
  labs(y = "", x = "time", color = "")



# 2. Compare measured data to modeled data and pre-calibrated data

out_theta <- data.frame(fluxes$time, fluxes$swc, output_cal$theta, output$theta)
names(out_theta) <- c("time", "swc", "theta_cal", "theta")

gg_theta <- gather(out_theta, key = "type", value = "theta", swc, theta_cal, theta, factor_key = T)

ggplot() +
  geom_line(aes(gg_theta$time, gg_theta$theta, color = gg_theta$type), size = 1) +
  labs(y = "soil water content [m3 m-3]", x = "time", color = "")



# 3. Plotting output variables

out <- data.frame(fluxes$time, output_cal[ , -8], input$p)
names(out) <- c("time", "theta", "runoff", "hyd_conductivity", "evaporation", "drainage", "water_potential", "water_potential_sub", "precipitation")


# evaporation

gg_theta <- gather(out, key = "type", value = "theta", evaporation, factor_key = T)

ggplot() +
  geom_line(aes(gg_theta$time, gg_theta$theta, color = gg_theta$type), size = 1) +
  labs(y = "evaporation [m s-1]", x = "time", color = "")


# drainage against hydraulic conductivity

gg_theta <- gather(out, key = "type", value = "theta", drainage, factor_key = T)

ggplot() +
  geom_line(aes(gg_theta$hyd_conductivity, gg_theta$theta, color = gg_theta$type), size = 1) +
  labs(y = "drainage [m s-1]", x = "hydraulic conductivity [m s-1]", color = "")


# water potential and precipitation

gg_theta <- gather(out, key = "type", value = "theta", water_potential, factor_key = T)

ggplot() +
  geom_line(aes(gg_theta$time, gg_theta$theta, color = gg_theta$type), size = 1) +
  labs(y = "water potential [m]", x = "time", color = "")


gg_theta <- gather(out, key = "type", value = "theta", precipitation, factor_key = T)

ggplot() +
  geom_line(aes(gg_theta$time, gg_theta$theta, color = gg_theta$type), size = 1) +
  labs(y = "precipitation [m3 dt-1]", x = "time", color = "")


# multiple plots together for comparison

gg_thetaA <- gather(out, key = "type", value = "theta", theta, factor_key = T)

A <- ggplot() +
  geom_line(aes(gg_thetaA$time, gg_thetaA$theta, color = gg_thetaA$type), size = 1) +
  labs(y = "theta [m3 m-3]", x = "time", color = "")

gg_thetaB <- gather(out, key = "type", value = "theta", precipitation, factor_key = T)

B <- ggplot() +
  geom_line(aes(gg_thetaB$time, gg_thetaB$theta, color = gg_thetaB$type), size = 1) +
  labs(y = "precipitation [m3 dt-1]", x = "time", color = "")

gg_thetaC <- gather(out, key = "type", value = "theta", water_potential, factor_key = T)

C <- ggplot() +
  geom_line(aes(gg_thetaC$time, gg_thetaC$theta, color = gg_thetaC$type), size = 1) +
  labs(y = "water potential [m]", x = "time", color = "")

gg_thetaD <- gather(out, key = "type", value = "theta", drainage, factor_key = T)

D <- ggplot() +
  geom_line(aes(gg_thetaD$time, gg_thetaD$theta, color = gg_thetaD$type), size = 1) +
  labs(y = "drainage [m s-1]", x = "time", color = "")

gg_thetaE <- gather(out, key = "type", value = "theta", hyd_conductivity, factor_key = T)

E <- ggplot() +
  geom_line(aes(gg_thetaE$time, gg_thetaE$theta, color = gg_thetaE$type), size = 1) +
  labs(y = "hydraulic conductivity [m s-1]", x = "time", color = "")

grid.arrange(A, B, C, D, E, ncol = 2)
