calc_fun_Photosynthesis_StomatalConductance = function(met,state_last,pars,ps_sc){

#source("sp_12_02.R")
# photosynthesis_stomatalconductance/
source("photosynthesis_stomatalconductance/hybrid_root_ci.R")
source("photosynthesis_stomatalconductance/satvap.R")
#source("photosynthesis_stomatalconductance/CiFunc.R")
source("photosynthesis_stomatalconductance/CO2LeafBoundaryLayer.R")
source("photosynthesis_stomatalconductance/PAR.R")
library("signal")
library("pracma")


# Vapor pressure (Pa) and specific humidity (kg/kg)
# where does esat come from? state_last ?

esat = satvap ((met$tair-pars$tfrz));
flux$eair = esat * (met$rh);
#qair = pars$mmh2o / pars$mmdry * eair / (met$pa - (1 - pars$mmh2o/pars$mmdry) * eair);

# Boundary layer conductance for CO2

flux$gbc = CO2LeafBoundaryLayer(state_last,met,pars)

# entropy terms in dependence of air T

flux$vcmaxse = 668.39 - 1.07 * met$tair
flux$jmaxse = 659.7 - 0.75 * met$tair

#from LeafPhydiologyParams.R
# done with LeafPhysiologyParams.R

fth25 = function(hd, se) {1 + exp((-hd + se*(pars$tfrz+25)) / (pars$R *(pars$tfrz+25)))};
flux$vcmaxc = fth25 (pars$vcmaxhd, flux$vcmaxse);
flux$jmaxc  = fth25 (pars$jmaxhd, flux$jmaxse);
flux$rdc    = fth25 (pars$rdhd, ps_sc$rdse);

# --- Adjust photosynthetic parameters for temperature

# C3 temperature response

ft = function(tl, ha) {exp(ha/(pars$R*(pars$tfrz+25)) * (1-(pars$tfrz+25)/tl));}
fth = function(tl, hd, se, fc) {fc / (1 + exp((-hd+se*tl)/(pars$R*tl)));}

# add flux$kc,flux$ko,flux$cp,flux$vcmax,flux$jmax,flux$rd,flux$je to:
# state variables state_last? or
# give them to cifunc individually? or
# use ps_sc? or
# use different list?

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
t2 = fth(state_last$tleaf, pars$rdhd, ps_sc$rdse, flux$rdc);
flux$rd = pars$rd25 * t1 * t2;

#print("rd:")
#print(flux$rd)

# calculating apar from sw_in

flux$apar = PAR(pars,met,ps_sc)

# --- Electron transport rate for C3 plants

# Solve the polynomial: aquad*Je^2 + bquad*Je + cquad = 0
# for Je. Correct solution is the smallest of the two roots.

qabs = 0.5 * pars$phi_psii * flux$apar;
aquad = pars$theta_j;
bquad = -(qabs + flux$jmax);
cquad = qabs * flux$jmax;
pcoeff = c(aquad,bquad,cquad);
proots = roots(pcoeff);
flux$je = min(proots[1], proots[2]);

print(c("flux$je:", flux$je))
# --- Ci calculation

# Initial estimates for Ci

ci0 = 0.7 * met$co2;
ci1 = ci0 * 0.99;

# Solve for Ci: Use CiFunc to iterate photosynthesis calculations
# until the change in Ci is < tol. Ci has units umol/mol

tol = 0.1;                 # Accuracy tolerance for Ci (umol/mol)

# --- calculation of an and gs

flux_dummy = hybrid_root_ci (met,state_last,pars,flux,ci0, ci1,tol);
flux = flux_dummy[[1]]
flux$ci = flux_dummy[[2]];

# --- Relative humidity and vapor pressure at leaf surface

esat = satvap ((state_last$tleaf-pars$tfrz));
flux$hs = (state_last$gbw * flux$eair + flux$gs * esat) / ((state_last$gbw + flux$gs) * esat);
flux$vpd = max(esat - flux$hs*esat, 0.1);

# --- Make sure iterative solution is correct

if (flux$gs < 0) {
  stop ('LeafPhotosynthesis: negative stomatal conductance')
}

# Compare with Ball-Berry model. The solution blows up with low eair. In input
# data, eair should be > 0.05*esat to ensure that hs does not go to zero.

#gs_err = pars$g1 * max(flux$an, 0) * flux$hs / flux$cs + pars$g0;
#if (abs(flux$gs-gs_err)*1e06 > 1e-04) {
#  fprintf('gs = #15.4f\n', flux$gs)
#  fprintf('gs_err = #15.4f\n', gs_err)
#  stop ('LeafPhotosynthesis: failed Ball-Berry error check')
#}

# Compare with diffusion equation: An = (ca - ci) * gleaf

#an_err = (met$co2 - flux$ci) / (1 / flux$gbc + 1.6 / flux$gs);
#if (flux$an > 0 & abs(flux$an-an_err) > 0.01){
#  fprintf('An = #15.4f\n', flux$an)
#  fprintf('An_err = #15.4f\n', an_err)
#  stop ('LeafPhotosynthesis: failed diffusion error check')
#}



  # write state variables
  #state_last$ = ...
#write.csv(out,file = "photosynthesis_testrun_01")

#plot(out$an ~ out$gs)
#plot(out$an ~ atmos$tair_i)
#plot(out$an ~ atmos$eair)
#plot(out$an ~ Hainich5Days$NIGHT)
#plot(out$an[1:24] ~ c(1:24))

#plot(out$gs ~ out$gs)
#plot(out$gs ~ atmos$tair_i)
#plot(out$gs ~ atmos$eair)
#plot(out$gs ~ Hainich5Days$NIGHT)
#plot(out$gs ~ Hainich5Days$TIMESTAMP_END, type = "l")

  return(data.frame(an = flux$an, gs = flux$gs, gbc = flux$gbc,ci = flux$ci))
}

