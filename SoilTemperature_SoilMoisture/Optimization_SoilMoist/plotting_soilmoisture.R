## Plotting ##

library(ggplot2)
library(gridExtra)
library(tidyr)


# 1. Sensitivity analysis

# plot sensitivity functions of model output to parameters

date <- input[1][1:743, ]
par_sens <- cbind(date, Sens[, 3:7])
names(par_sens) <- c("date", "epmod", "b", "psi.n1", "ra", "theta.sat")

gg_df <- gather(par_sens, key = "type", value = "theta", epmod, b, psi.n1, ra, theta.sat, factor_key = T)

S <- ggplot() +
  geom_line(aes(gg_df$date, gg_df$theta, color = gg_df$type), size = 1) +
  labs(y = "sensitivity", x = "time [h]", color = "", title = "Sensitivity analysis") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        #panel.background = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black")
        )
ggsave("D:\\Sensitivity.png", plot = S, width = 12.5, height = 7.5, units = "in", dpi = 700)


# 2. Compare measured data to modeled data and pre-calibrated data

out_theta <- data.frame(input$datetime, fluxes$swc, output_cal$theta, output$theta)
names(out_theta) <- c("time", "theta_measured", "theta_calibrated", "theta_uncalibrated")

gg_theta <- gather(out_theta, key = "type", value = "theta", theta_measured, theta_calibrated, theta_uncalibrated, factor_key = T)

T <- ggplot() +
  geom_line(aes(gg_theta$time, gg_theta$theta, color = gg_theta$type), size = 1) +
  labs(y = "soil water content [m3 m-3]", x = "date", color = "", title = "Comparison of measured, uncalibrated and calibrated soil water content") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"))
T
ggsave("D:\\Theta.png", plot = T, width = 12.5, height = 7.5, units = "in", dpi = 700)


# 3. Plotting output variables

out <- data.frame(input$datetime, output_cal[ , -8], input$p)
names(out) <- c("time", "theta", "runoff", "hyd_conductivity", "evaporation", "drainage", "water_potential", "water_potential_sub", "precipitation")


# evaporation

gg_thetaF <- gather(out, key = "type", value = "theta", evaporation, factor_key = T)

F <- ggplot() +
  geom_line(aes(gg_theta$time, gg_theta$theta, color = gg_theta$type), size = 1) +
  labs(y = "evaporation [m s-1]", x = "date", color = "", title = "Evaporation") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"))
F

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
  labs(y = "theta [m3 m-3]", x = "date", color = "", title = "Soil water content") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"))

gg_thetaB <- gather(out, key = "type", value = "theta", precipitation, factor_key = T)

B <- ggplot() +
  geom_line(aes(gg_thetaB$time, gg_thetaB$theta, color = gg_thetaB$type), size = 1) +
  labs(y = "precipitation [m3 dt-1]", x = "date", color = "", title = "Precipitation") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"))

gg_thetaC <- gather(out, key = "type", value = "theta", water_potential, factor_key = T)

C <- ggplot() +
  geom_line(aes(gg_thetaC$time, gg_thetaC$theta, color = gg_thetaC$type), size = 1) +
  labs(y = "water potential [m]", x = "date", color = "", title = "Water potential") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"))

gg_thetaD <- gather(out, key = "type", value = "theta", drainage, factor_key = T)

D <- ggplot() +
  geom_line(aes(gg_thetaD$time, gg_thetaD$theta, color = gg_thetaD$type), size = 1) +
  labs(y = "drainage [m s-1]", x = "date", color = "", title = "Drainage") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"))

gg_thetaE <- gather(out, key = "type", value = "theta", hyd_conductivity, factor_key = T)

E <- ggplot() +
  geom_line(aes(gg_thetaE$time, gg_thetaE$theta, color = gg_thetaE$type), size = 1) +
  labs(y = "hydraulic conductivity [m s-1]", x = "date", color = "", title = "Hydraulic conductivity") +
  theme(title = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.line = element_line(colour = "black"))

grid.arrange(A, B, C, D, E, F, ncol = 2)
