CiFunc = function(met,state_last,pars,flux, ci_val){

# --- Metabolic (demand-based) photosynthetic rate

# C3: Rubisco-limited photosynthesis
flux$ac = flux$vcmax * max(ci_val - flux$cp, 0) / (ci_val + flux$kc * (1 + pars$o2air / flux$ko));

# C3: RuBP regeneration-limited photosynthesis
flux$aj = flux$je * max(ci_val - flux$cp, 0) / (4 * ci_val + 8 * flux$cp);

# --- Net photosynthesis as the minimum or co-limited rate

# First co-limit flux$ac and Aj. flux$ai is the intermediate co-limited photosynthesis
# rate found by solving the polynomial: aquad*flux$ai^2 + bquad*flux$ai + cquad = 0 for flux$ai.
# Correct solution is the smallest of the two roots.

aquad = pars$colim_c3;
bquad = -(flux$ac + flux$aj);
cquad = flux$ac * flux$aj;
pcoeff = c(aquad,bquad,cquad);
proots = roots(pcoeff);
flux$ai = min(Re(proots[[1]]), Re(proots[[2]]));
flux$ag = flux$ai;

# Prevent growth photosynthesis from ever being negative

flux$ac = max(flux$ac, 0);
flux$aj = max(flux$aj, 0);
flux$ap = max(flux$ap, 0);
flux$ag = max(flux$ag, 0);

# Net CO2 uptake

flux$an = flux$ag - flux$rd;

# --- CO2 at leaf surface

flux$cs = met$co2 - flux$an / flux$gbc;
flux$cs = max(flux$cs, 1);

# --- Stomatal constraint function

# Saturation vapor pressure at leaf temperature

esat = satvap(state_last$tleaf-pars$tfrz);

# Ball-Berry stomatal conductance is a quadratic equation
# for gs given An: aquad*gs^2 + bquad*gs + cquad = 0. Correct
# solution is the larger of the two roots. This solution is
# valid for An >= 0. With An <= 0, gs = g0.

term = flux$an / flux$cs;
if (flux$an > 0){
  aquad = 1;
  bquad = flux$gbw - pars$g0 - pars$g1 * term;
  cquad = -1 * flux$gbw * (pars$g0 + pars$g1 * term * flux$eair / flux$esat);
  pcoeff = c(aquad,bquad,cquad);
  proots = roots(pcoeff);
  flux$gs = max(Re(proots[[1]]), Re(proots[[2]]));
} else {
  flux$gs = pars$g0;
}
# --- Diffusion (supply-based) photosynthetic rate

# Leaf CO2 conductance (mol CO2/m2/s)

gleaf = 1 / (1 / flux$gbc + 1.6 / flux$gs);

# Calculate Ci from the diffusion rate

cinew = met$co2 - flux$an / gleaf;

# --- Return the difference between the current Ci and the new Ci

if (flux$an >= 0){
  ci_dif = cinew - ci_val;
} else {
  ci_dif = 0;
}

CiFunc_output = list(flux,ci_dif)
return(CiFunc_output)

}
