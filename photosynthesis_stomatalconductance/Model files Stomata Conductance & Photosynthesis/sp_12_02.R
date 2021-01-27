sp_12_02 = function(flux,leaf,physcon,atmos){

# -------------------------------------------------------------------------
#Create physical constants, inital atmos values, leafphysiology params, inital fluxes
# -------------------------------------------------------------------------

# call these:

source("satvap.R")
source("LeafPhysiologyParams.R")
source("LeafBoundaryLayer.R")

# will get these from elsewhere later, or by iterating. Where startpioint?
params = list()
ground = list()

# --- Waveband indices for visible and near-infrared

params$vis = 1; params$nir = 2

# --- Physical constants

physcon$grav = 9.80665;               # Gravitational acceleration (m/s2)     # needed for Leeafboundarylayer
physcon$tfrz = 273.15;                # Freezing point of water (K)           # needed for Photosynthesis
physcon$sigma = 5.67e-08;             # Stefan-Boltzmann constant (W/m2/K4)   # needed for ground radiation
physcon$mmdry = 28.97 / 1000;         # Molecular mass of dry air (kg/mol)    # needed for specific humidity
physcon$mmh2o = 18.02 / 1000;         # Molecular mass of water (kg/mol)      # needed for specific humidity
physcon$cpd = 1005;                   # Specific heat of dry air at constant pressure (J/kg/K)
physcon$cpw = 1846;                   # Specific heat of water vapor at constant pressure (J/kg/K)
physcon$rgas = 8.31446;               # Universal gas constant (J/K/mol)      # needed for Photosynthesis
physcon$visc0 = 13.3e-06;             # Kinematic viscosity at 0C and 1013.25 hPa (m2/s)  # needed for Leeafboundarylayer
physcon$Dh0 = 18.9e-06;               # Molecular diffusivity (heat) at 0C and 1013.25 hPa (m2/s) # needed for Leeafboundarylayer
physcon$Dv0 = 21.8e-06;               # Molecular diffusivity (H2O) at 0C and 1013.25 hPa (m2/s)  # needed for Leeafboundarylayer
physcon$Dc0 = 13.8e-06;               # Molecular diffusivity (CO2) at 0C and 1013.25 hPa (m2/s)  # needed for Leeafboundarylayer

# Leaf physiological parameters

leaf = LeafPhysiologyParams(params,physcon,leaf);

# --- Atmospheric forcing
# for Photosynthesis/Stomatal Conductance atmos$eair and flux$apar are needed
# these are calcualet in the lines below

# Process sunlit or shaded leaf

# leaftype = 'sun';
# leaftype = 'shade';

# Atmospheric CO2 (umol/mol) and O2 (mmol/mol)

atmos$co2air = 450;
atmos$o2air = 0.209 * 1000;

# Air temperature (K) and relative humidity (#)

atmos$tair = physcon$tfrz + 30;
atmos$relhum = 60;

# Wind (m/s)
# needed for leafpboundarylayerconductance
# u = 0.01_r8  ! Still air
# u = 0.1_r8   ! Calm - smoke rises vertically
# u = 1.0_r8   ! Light air - smoke drift indicates wind direction
# u = 2.5_r8   ! Light breeze - wind felt on skin, leaves rustle
# u = 5.0_r8   ! Gentle breeze - leaves constantly moving and light flag extended
# u = 10.0_r8  ! Moderate breeze

atmos$wind = 1;

# Atmospheric pressure (Pa)

atmos$patm = 101325;

# Vapor pressure (Pa) and specific humidity (kg/kg)

esat = satvap ((atmos$tair-physcon$tfrz));
atmos$eair = esat * (atmos$relhum / 100);
atmos$qair = physcon$mmh2o / physcon$mmdry * atmos$eair / (atmos$patm - (1 - physcon$mmh2o/physcon$mmdry) * atmos$eair);

# Molar density (mol/m3)

atmos$rhomol = atmos$patm / (physcon$rgas * atmos$tair);

# Air density (kg/m3)

#atmos$rhoair = atmos$rhomol * physcon$mmdry * (1 - (1 - physcon$mmh2o/physcon$mmdry) * atmos$eair / atmos$patm);

# Molecular mass of air (kg/mol)

#atmos$mmair = atmos$rhoair / atmos$rhomol;

# Specific heat of air at constant pressure (J/mol/K)

#atmos$cpair = physcon$cpd * (1 + (physcon$cpw/physcon$cpd - 1) * atmos$qair) * atmos$mmair;

# Atmospheric longwave radiation (W/m2)

#atmos$irsky = 400;

# Solar radiation (W/m2)

# switch leaftype
# case 'sun'
fsds = 800;   # Sun leaf

#fsds = Hainich5Days$SW_IN_F

# case 'shade'
# fsds = 300;   # Shade leaf
# end

# par to W m^-2 ?
# radiation replaced

#atmos$swskyvis = 0.5 * fsds;   # short wave sky

atmos$swsky[params$vis] = 0.5 * fsds;   # short wave sky
atmos$swsky[params$nir] = 0.5 * fsds;   # short wave sky

# --- Ground variables

ground$albsoi[params$vis] = 0.1;      # Soil albedo (visible waveband)
ground$albsoi[params$nir] = 0.2;      # Soil albedo (near-infrared waveband)


tg = atmos$tair;
ground$irgrd = physcon$sigma * tg^4;

# --- Radiation absorbed by leaf (from gourp 3)

# Solar radiation incident on leaf

#flux$swincvis = atmos$swskyvis * (1 + ground$albsoi[params$vis]);

flux$swinc[params$vis] = atmos$swsky[params$vis] * (1 + ground$albsoi[params$vis]);
flux$swinc[params$nir] = atmos$swsky[params$nir] * (1 + ground$albsoi[params$nir]);

# Solar radiation absorbed by leaf

#flux$swflxvis = flux$swincvis * (1 - leaf$rho[params$vis] - leaf$tau[params$vis]);


flux$swflx[params$vis] = flux$swinc[params$vis] * (1 - leaf$rho[params$vis] - leaf$tau[params$vis]);
flux$swflx[params$nir] = flux$swinc[params$nir] * (1 - leaf$rho[params$nir] - leaf$tau[params$nir]);
flux$apar = flux$swflx[params$vis] * 4.6;


#flux$apar = flux$swflxvis * 4.6;


# Radiative forcing for leaf temperature calculation (not needed for an/gs)

#flux$qa = flux$swflx[params$vis] + flux$swflx[params$nir] + leaf$emiss * (atmos$irsky + ground$irgrd);

# --- Initial leaf temperature

flux$tleaf = atmos$tair;

flux = LeafBoundaryLayer(physcon, atmos, leaf, flux)

fluxleafphysconatmos = list(flux,leaf,physcon,atmos)

return(fluxleafphysconatmos)

}
