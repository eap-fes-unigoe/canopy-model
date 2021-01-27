## Modelling Soil Moisture with one soil layer ##

library(dplyr)
library(ggplot2)

# input data

input <- read.csv("Climate_Data.csv", header = T, sep = ";")                                                 # file with all the input variables
input$Date.Time <- as.POSIXct(input$Date.Time, format = "%d.%m.%Y %H:%M")                                    # first column as Date with time
input.t <- filter(input, input$Date.Time > "2017-10-31 23:30:00" & input$Date.Time < "2017-12-01 00:00:00")  # only data from Nov 2017 (example data)
input.t[ , 2:82] <-lapply(input.t[ , 2:82], as.numeric)                                                      # change class to numeric
input.test <- input.t[ , -(1)]

param <- read.csv("Parameter.csv", header = T, sep = ';')

# create function

source("fun_soilmoisture.R")

output <- get_theta_soil(input.test = input.test, stat.var = stat.var, param = param)

#### create plots ####


A <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, theta)) +
  xlab("Date") +
  ylab("theta [m3 m-3]") +
  ggtitle("Volumetric water content")

B <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, prec)) +
  xlab("Date") +
  ylab("prec [m s-1]") +
  ggtitle("Precipitation")

C <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, runoff)) +
  xlab("Date") +
  ylab("runoff [m s-1]") +
  ggtitle("Runoff")

D <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, inf)) +
  ylab("inf [m s-1]") +
  xlab("Date") +
  ggtitle("Infiltration")

E <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, evap)) +
  xlab("Date") +
  ylab("evap [m s-1]") +
  ggtitle("Evaporation from soil")

F <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, Rh)) +
  xlab("Date") +
  ylab("Rh [%]") +
  ggtitle("Relative Humidity")

G <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, drain)) +
  xlab("Date") +
  ylab("drain [m s-1]") +
  ggtitle("Drainage")

H <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, temp)) +
  xlab("Date") +
  ylab("temp [K]") +
  ggtitle("Temperature")

multiplot(A, B, C, D, E, F, G, H, cols = 2)


I <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, psi, colour = "psi")) +
  geom_line(aes(input.t$Date.Time, psi.n1, colour = "psi.n1")) +
  xlab("Date") +
  ylab("matric potential [m]") +
  ggtitle("Matric potential") +
  theme(legend.position = c(0.97, 0.7))
I

J <- ggplot(output) +
  geom_line(aes(input.t$Date.Time, k)) +
  xlab("Date") +
  ylab("k [m s-1]") +
  ggtitle("Hydraulic conductivity")

multiplot(G, I, J, B)
