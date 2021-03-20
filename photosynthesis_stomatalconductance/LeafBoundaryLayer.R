LeafBoundaryLayer = function(state_last,met,pars) {

  # --- Adjust diffusivity for temperature and pressure

  fac = 101325 / met$pa * (met$tair / pars$tfrz)^1.81;

  visc = pars$visc0 * fac; # Kinematic viscosity (m2/s)
  Dh = pars$Dh0 * fac;     # Molecular diffusivity, heat (m2/s)
  Dv = pars$Dv0 * fac;     # Molecular diffusivity, H2O (m2/s)
  Dc = pars$Dc0 * fac;     # Molecular diffusivity, CO2 (m2/s)

  # --- Dimensionless numbers

  Re = met$ws * pars$dleaf / visc; # Reynolds number
  Pr = visc / Dh;                      # Prandtl number
  Scv = visc / Dv;                     # Schmidt number for H2O
  Scc = visc / Dc;                     # Schmidt number for CO2

  # Grashof number

  Gr = pars$grav * pars$dleaf^3 * max(state_last$tleaf-met$tair, 0) / (met$tair * visc * visc);

  # --- Empirical correction factor for Nu and Sh

  b1 = 1.5;

  # --- Nusselt number (Nu) and Sherwood numbers (H2O: Shv, CO2: Shc)

  # Forced convection - laminar flow

  Nu_lam  = b1 * 0.66 *  Pr^0.33 * Re^0.5;     # Nusselt number
  Shv_lam = b1 * 0.66 * Scv^0.33 * Re^0.5;     # Sherwood number, H2O
  Shc_lam = b1 * 0.66 * Scc^0.33 * Re^0.5;     # Sherwood number, CO2

  # Forced convection - turbulent flow

  Nu_turb  = b1 * 0.036 *  Pr^0.33 * Re^0.8;   # Nusselt number
  Shv_turb = b1 * 0.036 * Scv^0.33 * Re^0.8;   # Sherwood number, H2O
  Shc_turb = b1 * 0.036 * Scc^0.33 * Re^0.8;   # Sherwood number, CO2

  # Choose correct flow regime for forced convection

  Nu_forced = max(Nu_lam, Nu_turb);
  Shv_forced = max(Shv_lam, Shv_turb);
  Shc_forced = max(Shc_lam, Shc_turb);

  # Free convection

  Nu_free  = 0.54 *  Pr^0.25 * Gr^0.25;        # Nusselt number
  Shv_free = 0.54 * Scv^0.25 * Gr^0.25;        # Sherwood number, H2O
  Shc_free = 0.54 * Scc^0.25 * Gr^0.25;        # Sherwood number, CO2

  # Both forced and free convection regimes occur together

  Nu = Nu_forced + Nu_free;
  Shv = Shv_forced + Shv_free;
  Shc = Shc_forced + Shc_free;

  # --- Boundary layer conductances (m/s)

  gbh = Dh *  Nu / pars$dleaf;
  gbw = Dv * Shv / pars$dleaf;
  gbc = Dc * Shc / pars$dleaf;

  # Molar density (mol/m3)

  rhomol = met$pa / (pars$R * met$tair);

  # --- Convert conductance (m/s) to (mol/m2/s)

  # Output
  #   gbh          ! Leaf boundary layer conductance, heat (mol/m2 leaf/s)
  #   gbw          ! Leaf boundary layer conductance, H2O (mol H2O/m2 leaf/s)
  #   gbc          ! Leaf boundary layer conductance, CO2 (mol CO2/m2 leaf/s)

  gbh = gbh * rhomol;
  gbw = gbw * rhomol;
  gbc = gbc * rhomol;

  return(c(gbw,gbc))

}
