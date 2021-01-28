# Load the input data

library(dplyr)

input <- read.csv("data/Hainich_2018_input.csv")

# Initial variable selection, renaming and conversion
input <- input %>% mutate(
  time = 1:nrow(input),
  tair = TA_F + 273.15,  # Celsius to Kelvin
  p = P_F,               # mm 30min-1 = l m-3
  sw_in = SW_IN_F,       # W m-2
  lw_in = LW_IN_F,       # W m-2
  ppfd_in = PPFD_IN,     # µmol m-2 s-1
  vpd = VPD_F * 100,     # hPa to Pa
  pa = PA_F * 1000,      # kPa to Pa
  ws = WS_F,             # m s-1
  rh = RH / 100,         # percent to fraction
  sw_dif = SW_DIF,       # W m-2
  co2 = CO2_F_MDS,       # µmol mol-1
  NIGHT = NULL,
  .keep = "unused"
)


fluxes <- read.csv("data/Hainich_2018_fluxes.csv")

# Initial variable selection, renaming and conversion
# tsoil and swc means across soil depths don't take into account layer thickness or properties.
# We ignore this for the purpose of this excersice.
fluxes <- fluxes %>% mutate(
  time = 1:nrow(fluxes),
  sw_out = SW_OUT,
  tsoil = ((TS_F_MDS_1 + TS_F_MDS_2 + TS_F_MDS_3 + TS_F_MDS_4) / 4) + 273.15,
  swc = (SWC_F_MDS_1 + SWC_F_MDS_2 + SWC_F_MDS_3) / 3,
  g = G_F_MDS,
  le = LE_F_MDS,
  h = H_F_MDS,
  nee = NEE_VUT_REF,
  reco = RECO_NT_VUT_REF,
  gpp = GPP_NT_VUT_REF,
  TIMESTAMP_START = NULL,
  TIMESTAMP_END = NULL,
  NIGHT = NULL,
  LW_OUT = NULL,
  TS_F_MDS_5 = NULL,
  LE_RANDUNC = NULL,
  H_RANDUNC = NULL,
  NEE_VUT_REF_JOINTUNC = NULL,
  .keep = "unused"
)

