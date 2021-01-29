PAR = function(pars,met,ps_sc){

# --- Waveband indices for visible and near-infrared

ps_sc$vis = 1;

# Solar radiation (W/m2)
fsds = met$sw_in

# par to W m^-2 ?
# radiation replaced

swskyvis = 0.5 * fsds;   # short wave sky

# --- Ground variables

albsoi[ps_sc$vis] = 0.1;      # Soil albedo (visible waveband)

# --- Radiation absorbed by leaf (from gourp 3)

# vis Solar radiation incident on leaf

swincvis = swskyvis * (1 + albsoi[ps_sc$vis]);

# vis Solar radiation absorbed by leaf

swflxvis = swincvis * (1 - ps_sc$rho[ps_sc$vis] - ps_sc$tau[ps_sc$vis]);
apar = swflxvis * 4.6;

return(apar)

}
