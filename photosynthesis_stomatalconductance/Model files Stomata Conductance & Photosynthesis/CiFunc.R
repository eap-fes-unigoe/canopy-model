CiFunc = function(physcon, atmos, leaf, flux, ci_val){

library("signal")
library("pracma")

# --- Metabolic (demand-based) photosynthetic rate

# C3: Rubisco-limited photosynthesis
ac = flux$vcmax * max(ci_val - flux$cp, 0) / (ci_val + flux$kc * (1 + atmos$o2air / flux$ko));

print("ac:")
print(ac)

# C3: RuBP regeneration-limited photosynthesis
flux$aj = flux$je * max(ci_val - flux$cp, 0) / (4 * ci_val + 8 * flux$cp);

print("aj:")
print(flux$aj)

# --- Net photosynthesis as the minimum or co-limited rate

#if (leaf$colim == 1) { #Use co-limitation

# First co-limit Ac and Aj. Ai is the intermediate co-limited photosynthesis
# rate found by solving the polynomial: aquad*Ai^2 + bquad*Ai + cquad = 0 for Ai.
# Correct solution is the smallest of the two roots.

aquad = leaf$colim_c3;
bquad = -(ac + flux$aj);
cquad = ac * flux$aj;
pcoeff = c(aquad,bquad,cquad);
proots = roots(pcoeff);


print("pcoeff:")
print(pcoeff)

ai = min(proots[1], proots[2]);

print("ai:")
print(ai)

flux$ag = ai;
#}

# Prevent photosynthesis from ever being negative

ac = max(ac, 0);
flux$aj = max(flux$aj, 0);
flux$ap = max(flux$ap, 0);
flux$ag = max(flux$ag, 0);

# Net CO2 uptake

flux$an = flux$ag - flux$rd;

# flux$an = max(flux$an, 0)

# --- CO2 at leaf surface

flux$cs = atmos$co2air - flux$an / flux$gbc;
flux$cs = max(flux$cs, 1);

# --- Stomatal constraint function

# Saturation vapor pressure at leaf temperature

esat = satvap ((flux$tleaf-physcon$tfrz));

# Ball-Berry stomatal conductance is a quadratic equation
# for gs given An: aquad*gs^2 + bquad*gs + cquad = 0. Correct
# solution is the larger of the two roots. This solution is
# valid for An >= 0. With An <= 0, gs = g0.
print("an:")
print(flux$an)
print("cs:")
print(flux$cs)

term = flux$an / flux$cs;
if (flux$an > 0){
  aquad = 1;
  bquad = flux$gbv - leaf$g0 - leaf$g1 * term;
  cquad = -1 * flux$gbv * (leaf$g0 + leaf$g1 * term * atmos$eair / esat);
  pcoeff = c(aquad,bquad,cquad);
  proots = roots(pcoeff);
  flux$gs = max(proots[1], proots[2]);
} else {
  flux$gs = leaf$g0;
}


# --- Diffusion (supply-based) photosynthetic rate

# Leaf CO2 conductance (mol CO2/m2/s)

gleaf = 1 / (1 / flux$gbc + 1.6 / flux$gs);

# Calculate Ci from the diffusion rate

cinew = atmos$co2air - flux$an / gleaf;

# --- Return the difference between the current Ci and the new Ci

if (flux$an >= 0){
  ci_dif = cinew - ci_val;
} else {
  ci_dif = 0;
}

CiFunc_output = list(flux,ci_dif)
return(CiFunc_output)

}
