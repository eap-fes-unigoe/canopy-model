
Photosynthesis_Stomatalconductance = function(met,state_last,pars,ps_sc){

#source("sp_12_02.R")
source("photoysnthesis_stomatalconductance/hybrid_root_ci.R")
source("photoysnthesis_stomatalconductance/satvap.R")
source("photoysnthesis_stomatalconductance/CiFunc.R")
library("signal")
library("pracma")


source("CO2LeafBoundaryLayer.R")


# Vapor pressure (Pa) and specific humidity (kg/kg)

esat = satvap ((met$tair-pars$tfrz));
eair = esat * (met$rh);
#qair = pars$mmh2o / pars$mmdry * eair / (met$pa - (1 - pars$mmh2o/pars$mmdry) * eair);


# --- Initial leaf temperature
#initial_state$tleaf = met$tair;

state_last$gbc = CO2LeafBoundaryLayer(state_last,met,pars)

# Boundary layer conductance and leaf tempereatur form group 4

#state_last$gbw = # boundary layer conductance for water from group 4
#state_last$tleaf = # leaf temperature from group 4

# entropy terms in dependence of air T

vcmaxse = 668.39 - 1.07 * met$tair
jmaxse = 659.7 - 0.75 * met$tair


# WE ARE HERE

# --- Adjust photosynthetic parameters for temperature

# C3 temperature response

ft = function(tl, ha) {exp(ha/(pars$R*(pars$tfrz+25)) * (1-(pars$tfrz+25)/tl));}
fth = function(tl, hd, se, fc) {fc / (1 + exp((-hd+se*tl)/(pars$R*tl)));}

flux$kc = leaf$kc25 * ft(flux$tleaf, leaf$kcha);
flux$ko = leaf$ko25 * ft(flux$tleaf, leaf$koha);
flux$cp = leaf$cp25 * ft(flux$tleaf, leaf$cpha);

t1 = ft(flux$tleaf, leaf$vcmaxha);
t2 = fth(flux$tleaf, leaf$vcmaxhd, vcmaxse, leaf$vcmaxc);
flux$vcmax = leaf$vcmax25 * t1 * t2;

t1 = ft(flux$tleaf, leaf$jmaxha);
t2 = fth(flux$tleaf, leaf$jmaxhd, jmaxse, leaf$jmaxc);
flux$jmax = leaf$jmax25 * t1 * t2;

t1 = ft(flux$tleaf, leaf$rdha);
t2 = fth(flux$tleaf, leaf$rdhd, leaf$rdse, leaf$rdc);
flux$rd = leaf$rd25 * t1 * t2;

print("rd:")
print(flux$rd)

# calculating apar from sw_in

apar = PAR(pars,met,ps_sc)

# --- Electron transport rate for C3 plants

# Solve the polynomial: aquad*Je^2 + bquad*Je + cquad = 0
# for Je. Correct solution is the smallest of the two roots.

qabs = 0.5 * leaf$phi_psii * apar;
aquad = leaf$theta_j;
bquad = -(qabs + flux$jmax);
cquad = qabs * flux$jmax;
pcoeff = c(aquad,bquad,cquad);
proots = roots(pcoeff);
flux$je = min(proots[1], proots[2]);

# --- Ci calculation

# Initial estimates for Ci

ci0 = 0.7 * atmos$co2air;
ci1 = ci0 * 0.99;

# Solve for Ci: Use CiFunc to iterate photosynthesis calculations
# until the change in Ci is < tol. Ci has units umol/mol

tol = 0.1;                 # Accuracy tolerance for Ci (umol/mol)

# --- calculation of an and gs

flux_dummy = hybrid_root_ci (physcon, atmos, leaf, flux, ci0, ci1, tol);
flux = flux_dummy[[1]]
flux$ci = flux_dummy[[2]];

# --- Relative humidity and vapor pressure at leaf surface

esat = satvap ((flux$tleaf-pars$tfrz));
flux$hs = (state_last$gbw * eair + flux$gs * esat) / ((state_last$gbw + flux$gs) * esat);
flux$vpd = max(esat - flux$hs*esat, 0.1);

# --- Make sure iterative solution is correct

if (flux$gs < 0) {
  stop ('LeafPhotosynthesis: negative stomatal conductance')
}

# Compare with Ball-Berry model. The solution blows up with low eair. In input
# data, eair should be > 0.05*esat to ensure that hs does not go to zero.

gs_err = leaf$g1 * max(flux$an, 0) * flux$hs / flux$cs + leaf$g0;
if (abs(flux$gs-gs_err)*1e06 > 1e-04) {
  fprintf('gs = #15.4f\n', flux$gs)
  fprintf('gs_err = #15.4f\n', gs_err)
  stop ('LeafPhotosynthesis: failed Ball-Berry error check')
}

# Compare with Medlyn model. The solutions blows up with vpd = 0. The
# quadratic calcuation of gsw in CiFunc constrains vpd > 50 Pa, so this
# comparison is only valid for those conditions.

# if (leaf$gstyp == 0)
#  if ((esat - eair) > 50)
#    gs_err = 1.6 * (1 + leaf$g1 / sqrt(flux$vpd*0.001)) * max(flux$an, 0) / flux$cs + leaf$g0;
#if (abs(flux$gs-gs_err)*1e06 > 1e-04)
#  fprintf('gs = #15.4f\n', flux$gs)
#fprintf('gs_err = #15.4f\n', gs_err)
#error ('LeafPhotosynthesis: failed Medlyn error check')
#end
#end
#end

# Compare with diffusion equation: An = (ca - ci) * gleaf

an_err = (atmos$co2air - flux$ci) / (1 / flux$gbc + 1.6 / flux$gs);
if (flux$an > 0 & abs(flux$an-an_err) > 0.01){
  fprintf('An = #15.4f\n', flux$an)
  fprintf('An_err = #15.4f\n', an_err)
  stop ('LeafPhotosynthesis: failed diffusion error check')
}



  # write state variables
  #state_last$ = ...

  return(flux)

}
