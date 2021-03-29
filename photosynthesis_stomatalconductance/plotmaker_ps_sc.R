


#####Plotmaker Function####
setwd("C:/Users/Mogli/Desktop/Git Hub/canopy-model_new")
out = read.table("photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/photosynthesis_hainich_year_data_60_9", header = T, dec = ".", sep = ",")

source("setup_sitedata.R")
Sitedata =  input


#### Linear models ####
d.mod = data.frame(out, Sitedata)
d.mod.norm.an = subset(d.mod[which(d.mod$an > 0),])
d.mod.norm.an$tair2 = d.mod.norm.an$tair^2
qmtemp_an = lm(d.mod.norm.an$an ~ d.mod.norm.an$tair + d.mod.norm.an$tair2 )
qmtemp_an2 = lm(d.mod.norm.an$an~poly(d.mod.norm.an$tair,3,raw=TRUE))

d.mod.norm.gs = subset(d.mod[which(d.mod$gs > 0.1),])
d.mod.norm.gs$tair2 = d.mod.norm.gs$tair^2
qmtemp_gs = lm(d.mod.norm.gs$gs ~ d.mod.norm.gs$tair + d.mod.norm.gs$tair2 )

qmsw_an = nls(out$an~a*Sitedata$sw_in/(1+b*Sitedata$sw_in),data=c(out, Sitedata),start=list(a=1.8,b=0.04))


temp_an_predict = predict(qmtemp_an)
temp_an_predict2 = predict(qmtemp_an2)
temp_gs_predict = predict(qmtemp_gs)
sw_in_an_predict = predict(qmsw_an)

########

S = "Year long observed photosynthesis (GPP)"


#pdf (file= "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/Sensitivity analysis plots/hainich_july_60_9.pdf")

plot(out$an ~ c(1:length(out$an)/24), xlab = "Timesteps in days", ylab = "Leaf net photosynthesis (umol CO2/m2 leaf/s)", main= c("Net assimilation over time", S), type = "l", cex.main= 0.8, ylim = c(0,80))
plot(out$gs ~ c(1:length(out$an)/24), xlab = "Timesteps in days", ylab = "Leaf stomatal conductance (mol H2O/m2 leaf/s)", main= c("stomatal conductance over time", S), type = "l", cex.main= 0.8, ylim =c(0,1.8))

plot(out$an ~ Sitedata$tair, xlab = "Temperature (K)", ylab = "Leaf net photosynthesis (umol CO2/m2 leaf/s)", main = c("Net assimilation over temperature", S), sub = "Fitted model excludes baseline (night values) for better fit.", cex.main= 0.8, ylim = c(0,80))
lines(temp_an_predict2 ~ d.mod.norm.an$tair, col = "red")

plot(fluxes$GPP_NT_VUT_REF ~ c(1:length(fluxes$GPP_NT_VUT_REF)/24), xlab = "Timesteps in days", ylab = "Leaf net photosynthesis (umol CO2/m2 leaf/s)", main= c("Net assimilation over time", S), type = "l", cex.main= 0.8, ylim = c(0,80))


plot(out$gs ~ Sitedata$tair, xlab = "Temperature (K)", ylab = "Leaf stomatal conductance (mol H2O/m2 leaf/s)", main =c("stomata conductance over temperature", S), sub = "Fitted model excludes baseline (night values) for better fit.", cex.main= 0.8, ylim = c(0, 1.8))
lines(temp_gs_predict ~ d.mod.norm.gs$tair, col = "red")


plot(out$an ~ Sitedata$sw_in, xlab = "Incoming shortwave radiation (W/m2)", ylab ="Leaf net photosynthesis (umol CO2/m2 leaf/s)", main = c("Net assimilation over shortwave radiation", S), cex.main= 0.8, ylim = c(0,80))
lines(sw_in_an_predict ~ Sitedata$sw_in, col="red")
#dev.off()


