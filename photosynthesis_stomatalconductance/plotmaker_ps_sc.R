plotmaker_ps_sc =function(out)

#####Plotmaker Function####


Sitedata = data.frame(c(input,fluxes))


pdf (file= Filename , title= Plottitle)

plot(out$an ~ c(1:length(out$Time)), xlab = "Timesteps", ylab = "Leaf net photosynthesis (umol CO2/m2 leaf/s)", main= "Net assimilation over time")
plot(out$gs ~ c(1:length(out$time)), xlab = "Timesteps", ylab = "Leaf stomatal conductance (mol H2O/m2 leaf/s)", main= "stomata conductance over time")
plot(out$an ~ Sitedata$tair, xlab = "Temperature (°C)", ylab = "Leaf net photosynthesis (umol CO2/m2 leaf/s)", main ="Net assimilation over temperature")
plot(out$gs ~ Sitedata$tair, xlab = "Temperature (°C)", ylab = "Leaf stomatal conductance (mol H2O/m2 leaf/s)", main ="stomata conductance over temperature")
plot(out$an ~ Sitedata$co2, xlab = "Ambient CO2 concentration (µmol mol-1)", ylab = "Leaf net photosynthesis (umol CO2/m2 leaf/s)", main ="Net assimilation over ambient CO2 concentration")
plot(out$gs ~ Sitedata$co2, xlab = "Ambient CO2 concentration (µmol mol-1)", ylab = "Leaf stomatal conductance (mol H2O/m2 leaf/s)" , main ="stomata conductance over ambient CO2 concentration")
plot(out$an ~ Sitedata$sw_in, xlab = "Incoming shortwave radiation (W/m2)", ylab ="Leaf net photosynthesis (umol CO2/m2 leaf/s)", main ="Net assimilation over shortwave radiation")

dev.off()
