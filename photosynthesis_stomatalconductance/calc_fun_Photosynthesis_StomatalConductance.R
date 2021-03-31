calc_fun_Photosynthesis_StomatalConductance = function(met,state_last,pars,apar){

  #Saturation vapor pressure at atmosphere at air temperature (Pa)
  esat = satvap(met$tair-pars$tfrz)[[1]];
  flux$eair = esat * met$rh; # Vapor pressure (Pa)

  # Boundary layer conductance for H20 and CO2, as passed on from fun_calc_an_gs
  flux$gbw = state_last$gbw #mol H2O/m2 leaf/s
  flux$gbc = state_last$gbc #mol CO2/m2 leaf/s

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
  qabs = 0.5 * pars$phi_psii * apar;
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

  # Accuracy tolerance for Ci (umol/mol)
  tol = 0.1;

  # --- calculation of an (umol CO2/m2 leaf/s) and gs (mol H2O/m2 leaf/s)
  flux_dummy = hybrid_root_ci (met,state_last,pars,flux,ci0, ci1,tol);
  flux = flux_dummy[[1]]
  flux$ci = flux_dummy[[2]];

  # --- Make sure iterative solution is correct
  if (flux$gs < 0) {
    stop ('LeafPhotosynthesis: negative stomatal conductance')
  }
  return(flux)
  #return(data.frame(an = flux$an, gs = flux$gs, gbc = flux$gbc, ci = flux$ci))
}

