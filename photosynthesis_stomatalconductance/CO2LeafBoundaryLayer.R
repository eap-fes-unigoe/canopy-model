CO2LeafBoundaryLayer = function(state_last,met,pars) {

  # --- Adjust diffusivity for temperature and pressure

  fac = 101325 / met$pa * (met$tair / pars$tfrz)^1.81;

  visc = pars$visc0 * fac; # Kinematic viscosity (m2/s)
  Dh = pars$Dh0 * fac;     # Molecular diffusivity, heat (m2/s)
  Dv = pars$Dv0 * fac;     # Molecular diffusivity, H2O (m2/s)
  Dc = pars$Dc0 * fac;     # Molecular diffusivity, CO2 (m2/s)

  # --- Dimensionless numbers

  Re = met$ws * pars$dleaf / visc;     # Reynolds number
  Pr = visc / Dh;                      # Prandtl number
  Scc = visc / Dc;                     # Schmidt number for CO2

  # Grashof number

  Gr = pars$grav * pars$dleaf^3 * max(state_last$tleaf-met$tair, 0) / (met$tair * visc * visc);

  # --- Empirical correction factor for Nu and Sh

  b1 = 1.5;

  # --- Nusselt number (Nu) and Sherwood numbers (H2O: Shv, CO2: Shc)

  # Forced convection - laminar flow

  Shc_lam = b1 * 0.66 * Scc^0.33 * Re^0.5;     # Sherwood number, CO2

  # Forced convection - turbulent flow

  Shc_turb = b1 * 0.036 * Scc^0.33 * Re^0.8;   # Sherwood number, CO2

  # Choose correct flow regime for forced convection

  Shc_forced = max(Shc_lam, Shc_turb);

  # Free convection

  Shc_free = 0.54 * Scc^0.25 * Gr^0.25;        # Sherwood number, CO2

  # Both forced and free convection regimes occur together

  Shc = Shc_forced + Shc_free;

  # --- Boundary layer conductances (m/s)

  gbc = Dc * Shc / pars$dleaf;

  # Molar density (mol/m3)

  rhomol = met$pa / (pars$R * met$tair);

  # --- Convert conductance (m/s) to (mol/m2/s)

  gbc = gbc * rhomol;

  return(gbc)

}
