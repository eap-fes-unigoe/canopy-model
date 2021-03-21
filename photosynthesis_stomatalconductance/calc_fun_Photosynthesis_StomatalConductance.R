calc_fun_Photosynthesis_StomatalConductance = function(met,state_last,pars,PAR){
#calc_fun_Photosynthesis_StomatalConductance = function(met,state_last,pars,PAR_list){
# output_flux = list()
# for(PAR in par_list) {    just an idea to

source("photosynthesis_stomatalconductance/hybrid_root_ci.R")
source("photosynthesis_stomatalconductance/satvap.R")
# source("photosynthesis_stomatalconductance/CO2LeafBoundaryLayer.R") # get h2o from group 4
source("photosynthesis_stomatalconductance/LeafBoundaryLayer.R") # calculate h2o without group 4
#source("photosynthesis_stomatalconductance/PAR.R")

# Beginning

esat = satvap(met$tair-pars$tfrz); # Saturation vapor pressure at atmosphere (Pa)
flux$eair = esat * met$rh; # Vapor pressure (Pa)

# Boundary layer conductance for CO2

# taking h20 conductance from leaf temperature group
# flux$gbc = CO2LeafBoundaryLayer(state_last,met,pars)
# flux$gbw = state_last$etflx ??? -> where is it?

# calculating both required conductances without group 4

blfluxes = LeafBoundaryLayer(state_last,met,pars)
flux$gbw = blfluxes[1]
flux$gbc = blfluxes[2]

# Photosynthetically active radiation from radiation group
flux$apar = PAR # umol photon/m2 leaf/s

# entropy terms in dependence of air T

flux$vcmaxse = 668.39 - 1.07 * met$tair
flux$jmaxse = 659.7 - 0.75 * met$tair
flux$rdse = 490

# scaling factors for high temperature inhibition

fth25 = function(hd, se) {1 + exp((-hd + se*(pars$tfrz+25)) / (pars$R *(pars$tfrz+25)))};
flux$vcmaxc = fth25 (pars$vcmaxhd, flux$vcmaxse);
flux$jmaxc  = fth25 (pars$jmaxhd, flux$jmaxse);
flux$rdc    = fth25 (pars$rdhd, flux$rdse);

# --- Adjust photosynthetic parameters for temperature

# C3 temperature response - Michaelis-Menten-function

ft = function(tl, ha) {exp(ha/(pars$R*(pars$tfrz+25)) * (1-(pars$tfrz+25)/tl));}
fth = function(tl, hd, se, fc) {fc / (1 + exp((-hd+se*tl)/(pars$R*tl)));}

flux$kc = pars$kc25 * ft(state_last$tleaf, pars$kcha);
flux$ko = pars$ko25 * ft(state_last$tleaf, pars$koha);
flux$cp = pars$cp25 * ft(state_last$tleaf, pars$cpha);

t1 = ft(state_last$tleaf, pars$vcmaxha);
t2 = fth(state_last$tleaf, pars$vcmaxhd, flux$vcmaxse, flux$vcmaxc);
flux$vcmax = pars$vcmax25 * t1 * t2;

t1 = ft(state_last$tleaf, pars$jmaxha);
t2 = fth(state_last$tleaf, pars$jmaxhd, flux$jmaxse, flux$jmaxc);
flux$jmax = pars$jmax25 * t1 * t2;

t1 = ft(state_last$tleaf, pars$rdha);
t2 = fth(state_last$tleaf, pars$rdhd, flux$rdse, flux$rdc);
flux$rd = pars$rd25 * t1 * t2;

# --- Electron transport rate je for C3 plants

# Solve the polynomial: aquad*Je^2 + bquad*Je + cquad = 0
# for Je. Correct solution is the smallest of the two roots.

qabs = 0.5 * pars$phi_psii * flux$apar;
aquad = pars$theta_j;
bquad = -(qabs + flux$jmax);
cquad = qabs * flux$jmax;
pcoeff = c(aquad,bquad,cquad);
proots = roots(pcoeff);
proots[1] = as.integer(proots[1])
flux$je = min(Re(proots[[1]]), Re(proots[[2]]));

# --- Ci calculation

# Initial estimates for Ci

ci0 = 0.7 * met$co2;
ci1 = ci0 * 0.99;

# Solve for Ci: Use CiFunc to iterate photosynthesis calculations
# until the change in Ci is < tol. Ci has units umol/mol

tol = 0.1;                 # Accuracy tolerance for Ci (umol/mol)

flux$esat = satvap ((state_last$tleaf-pars$tfrz));

# --- calculation of an (umol CO2/m2 leaf/s) and gs (mol H2O/m2 leaf/s)

flux_dummy = hybrid_root_ci (met,state_last,pars,flux,ci0, ci1,tol);
flux = flux_dummy[[1]]
flux$ci = flux_dummy[[2]];

# "par loop idea"  output_flux = output_flux + flux (chagne flux felow this to output_flux)}
# afterwards: sum results of sunny and shaded. (take mean for gs? hmm)

#esat = satvap ((state_last$tleaf-pars$tfrz));
#flux$vpd = max(esat - flux$hs*esat, 0.1);
#flux$hs = (flux$gbw * flux$eair + flux$gs * flux$esat) / ((flux$gbw + flux$gs) * flux$esat);

# --- Make sure iterative solution is correct

if (flux$gs < 0) {
  stop ('LeafPhotosynthesis: negative stomatal conductance')
}
# returning desired values: an, gs, ci, more? flux$cs!! return whole flux for now!
  return(flux)
  #return(data.frame(an = flux$an, gs = flux$gs, gbc = flux$gbc, ci = flux$ci))
}

