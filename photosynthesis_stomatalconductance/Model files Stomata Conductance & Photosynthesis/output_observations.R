
# some display settings
options(scipen = 999) # turning off "e" notations
par(mfrow = c(1,2)) # 2 visible plots

#An = would optimally be compared with NPP
#fluxes$gpp  - fluxes$reco - fluxes$nee
# units are m-2 ground s-1
plot(out$ag)
plot(out$an)
plot(out$gs)
#plot(out$rd)

# unit conversions
gpp_umol = fluxes$gpp /(12 / 1000000 / 1000 * 3600) # kg m-2 ground? dt-1 to µmol m-2 ground s-1 reversion
an_kg = out$an * 12 / 1000000 / 1000 * 3600 #µmol m-2 ground h-1
an_leaf = out$an / 5 # an µmol m-2 leaf s-1
gs_leaf = out$gs * 5  # gs mol h2o m-2 leaf s-1, base is 0.01
plot(an_leaf)
plot(gs_leaf)

# comparing an and gpp

# in kg per day
plot(an_kg, ylab = "an kg CO2/m2 ground/h") #photosynthesis in kg
plot(fluxes$gpp, ylab = "GPP kg CO2/m2 ground/h") #gpp in kg
plot(an_kg,fluxes$gpp)
#plot(an_kg,fluxes$nee)

# in umol per second
plot(out$an, ylab = "an umol CO2/m2 ground/s")#,ylim = c(0,30)) #photosynthesis in umol
plot(gpp_umol,ylab = "GPP umol CO2/m2 ground/s") #gpp in umol
plot(out$an, gpp_umol)
plot(out$ag, gpp_umol) #ag is gross photosynthesis rate
plot(out$an, out$ag)
#plot(input$p)

#comparing par from radiation gropu and from par function
#plot(out$apar, out$apar2)
#plot(out$apar, fluxes$r)

#plot gs
plot(out$gs, ylab = "gs mol h2o m-2 ground s-1") # in mol h2o m-2 ground s-1, base is 0.01
plot(out$gs, out$an) # in mol h2o m-2 ground s-1, base is 0.01

out$an/out$gs

# calculating slope g1  by use of regression model
#g1 = (gs - g0) / (An hs / ca)
gs_g0 = out$gs - pars$g0
an_hs_ca = (out$an * out$hs) / out$cs
plot(gs_g0 ~ an_hs_ca)

g1_lm = lm(gs_g0 ~ an_hs_ca)
#plot(g1_lm)
coef(g1_lm)
summary(g1_lm) # returns a slope of 8.83

write.csv(out,file = "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/hainich_july_60_9")
saveRDS(out, file = "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/photosynthesis_hainich_year_data_60_9.rds")
#### Plotmaker ####

Filename <- "photosynthesis_stomatalconductance/Model files Stomata Conductance & Photosynthesis/Outputs/Photosynthesis Model Outputs_General.pdf"
Plottitle <- "Photosynthesis Model Output General"
Sitedata = data.frame(c(input,fluxes))
source("photosynthesis_stomatalconductance/plotmaker_ps_sc.R")
plotmaker_ps_sc(out)

plot(input$sw_in)
points(out$ic_sh, col = "green")
points(out$ic_sun, col = "red")
points(out$ic_sha+out$ic_sun, col = "blue")
points(input$sw_in, col = "blue")
plot(out$ic)
