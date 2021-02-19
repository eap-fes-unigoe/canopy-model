## sensitivity analysis for soil temperature code parameters ##
library(dplyr)
library(FME)
pars_calib <- c(Cvsoil=1.9, Ko=3, Tsubsoil=290)


Sens <- sensFun(cost_soiltemperature1, pars_calib)
summary(Sens)
plot(Sens$Cvsoil, ylim = c(-0.4, 0.5),main='Sensitivity Analysis', sub='Change over time',
     xlab='Flux in time', ylab='Range Values Sensitivity Function', col="blue", type = "l" )
lines(Sens$Ko, col="red")
legend("bottomright", inset=0.01, legend=c("Cvsoil", "Ko"), 
       lwd=2, cex=1.2, col=c("blue", "red"), bg="lightyellow")
plot(Sens$Tsubsoil,main='Sensitivity Analysis', sub='Change over time',
     xlab='Flux in time', ylab='Range Values Sensitivity Function', type = "l", col="orange")
legend("bottomright", inset=0.01, legend=c("Tsubsoil"), 
       lwd=2, cex=1.2, col=c("orange"), bg="lightyellow")

  