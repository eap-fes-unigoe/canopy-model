# list for photosynthesis fluxes/state variables

flux = list()

# list for photosynthresis parameters

ps_sc = list()


# --- Waveband indices for ps_sc$visible and near-infrared

ps_sc$vis = 1;
ps_sc$rho[ps_sc$vis] = 0.057;
ps_sc$tau[ps_sc$vis] = 0.048;

# netropy term for respiration

ps_sc$rdse = 490


