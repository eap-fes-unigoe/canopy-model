## sensitivity analysis for soil moisture ##

pars_calib <- c(b.sm = 11.4, epmod = 1, psi.n1 = -0.4, ra = 10, theta.sat = 0.48)

Sens <- sensFun(cost_soilmoisture, pars_calib)
summary(Sens)
