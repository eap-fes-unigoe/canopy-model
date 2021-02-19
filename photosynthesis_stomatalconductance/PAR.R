PAR = function(pars,met){

# Solar radiation (W/m2)
fsds = met$sw_in

rho = 0.057;
tau = 0.048;

# par to W m^-2 ?
# radiation replaced

swskyvis = 0.5 * fsds;   # short wave sky

# --- Ground variables

albsoi = 0.1;      # Soil albedo (visible waveband)

# --- Radiation absorbed by leaf (from gourp 3)

# vis Solar radiation incident on leaf

swincvis = swskyvis * (1 + albsoi);

# vis Solar radiation absorbed by leaf

swflxvis = swincvis * (1 - rho - tau);
apar = swflxvis * 4.6;
print(c("apar:", apar))

return(apar)

}
