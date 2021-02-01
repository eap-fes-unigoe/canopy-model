PAR = function(pars,met,ps_sc){

# Solar radiation (W/m2)
fsds = met$sw_in

# --- Waveband indices for visible

ps_sc$rho = 0.057;
ps_sc$tau = 0.048;

# par to W m^-2 ?
# radiation replaced

swskyvis = 0.5 * fsds;   # short wave sky

# --- Ground variables

albsoi = 0.1;      # Soil albedo (visible waveband)

# --- Radiation absorbed by leaf (from gourp 3)

# vis Solar radiation incident on leaf

swincvis = swskyvis * (1 + albsoi);

# vis Solar radiation absorbed by leaf

swflxvis = swincvis * (1 - ps_sc$rho - ps_sc$tau);
apar = swflxvis * 4.6;
print(c("apar:", apar))

return(apar)

}
