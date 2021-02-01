CiFunc = function(met,state_last,pars,flux, ci_val){

library("signal")
library("pracma")

# --- Metabolic (demand-based) photosynthetic rate

# C3: Rubisco-limited photosynthesis
flux$ac = flux$vcmax * max(ci_val - flux$cp, 0) / (ci_val + flux$kc * (1 + pars$o2air / flux$ko));

print(c("flux$ac:",flux$ac))

# C3: RuBP regeneration-limited photosynthesis
flux$aj = flux$je * max(ci_val - flux$cp, 0) / (4 * ci_val + 8 * flux$cp);

print(c("aj:",flux$aj))

# --- Net photosynthesis as the minimum or co-limited rate

#if (leaf$colim == 1) { #Use co-limitation

# First co-limit flux$ac and Aj. flux$ai is the intermediate co-limited photosynthesis
# rate found by solving the polynomial: aquad*flux$ai^2 + bquad*flux$ai + cquad = 0 for flux$ai.
# Correct solution is the smallest of the two roots.

aquad = pars$colim_c3;
bquad = -(flux$ac + flux$aj);
cquad = flux$ac * flux$aj;
pcoeff = c(aquad,bquad,cquad);
proots = roots(pcoeff);


print(c("pcoeff:",pcoeff))

flux$ai = min(proots[1], proots[2]);

print(c("flux$ai:",flux$ai))

flux$ag = flux$ai;
#}

# Prevent photosynthesis from ever being negative

flux$ac = max(flux$ac, 0);
flux$aj = max(flux$aj, 0);
flux$ap = max(flux$ap, 0);
flux$ag = max(flux$ag, 0);

# Net CO2 uptake

flux$an = flux$ag - flux$rd;

# flux$an = max(flux$an, 0)

# --- CO2 at leaf surfflux$ace

flux$cs = met$co2 - flux$an / flux$gbc;
flux$cs = max(flux$cs, 1);

# --- Stomatal constrflux$aint function

# Saturation vapor pressure at leaf temperature

#esat = satvap ((flux$tleaf-physcon$tfrz));

# Ball-Berry stomatal conductance is a quadratic equation
# for gs given An: aquad*gs^2 + bquad*gs + cquad = 0. Correct
# solution is the larger of the two roots. This solution is
# valid for An >= 0. With An <= 0, gs = g0.
print(c("an:",flux$an))
print(c("cs:",flux$cs))

term = flux$an / flux$cs;
#print(c("flux$esat",flux$esat))
#state_last$gbw
if (flux$an > 0){
  aquad = 1;
  bquad = state_last$gbw - pars$g0 - pars$g1 * term;
  cquad = -1 * state_last$gbw * (pars$g0 + pars$g1 * term * flux$eair / flux$esat);
  pcoeff = c(aquad,bquad,cquad);
  proots = roots(pcoeff);
  flux$gs = max(proots[1], proots[2]);
} else {
  flux$gs = pars$g0;
}

print(c("flux$gs",flux$gs))

# --- Diffusion (supply-based) photosynthetic rate

# Leaf CO2 conductance (mol CO2/m2/s)

gleaf = 1 / (1 / flux$gbc + 1.6 / flux$gs);

# Calculate Ci from the diffusion rate

cinew = met$co2 - flux$an / gleaf;

print(c("cinew",cinew))
# --- Return the difference between the current Ci and the new Ci

if (flux$an >= 0){
  ci_dif = cinew - ci_val;
} else {
  ci_dif = 0;
}

CiFunc_output = list(flux,ci_dif)
return(CiFunc_output)

}
