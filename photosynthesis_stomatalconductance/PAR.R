#PAR = function(pars,input){
#PAR = function(pars,sw_in){
PAR = function(sw_in){

# Solar radiation (W/m2)
fsds = sw_in
#fsds = input$sw_in #for testing purposes input$sw_in = met$sw_in

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
# 4.6 conversion W m-2 to photon also confirmed by
#https://www.researchgate.net/post/Can-I-convert-PAR-photo-active-radiation-value-of-micro-mole-M2-S-to-Solar-radiation-in-Watt-m2
#print(c("apar:", apar))

return(apar)

}
