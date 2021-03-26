# Load the input data

library(dplyr)
library(lubridate)

input <- read.csv(file.path("data", "Hainich_2018-07_input.csv"))
#input <- read.csv(file.path("data", "Hainich_2018_input.csv"))
fluxes <- read.csv(file.path("data", "Hainich_2018-07_fluxes.csv"))

# Initial variable selection, renaming and conversion
input <- input %>% mutate(
  time = 1:nrow(input),
  datetime = force_tz(as_datetime(Date.Time), "Etc/GMT+1"),
  tair = TA_F + 273.15,  # Celsius to Kelvin
  p = P_F,               # mm / time_step = l m-2 / time_step
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


# Initial variable selection, renaming and conversion
# tsoil and swc means across soil depths don't take into account layer thickness or properties.
# We ignore this for the purpose of this excersice.
fluxes <- fluxes %>% mutate(
  time = 1:nrow(fluxes),
  sw_out = SW_OUT,          # W m-2
  lw_out = LW_OUT,          # W m-2
  tsoil = ((TS_F_MDS_1 + TS_F_MDS_2 + TS_F_MDS_3 + TS_F_MDS_4) / 4) + 273.15, # 30cm depth mean. Celsius to Kelvin
  swc = ((SWC_F_MDS_1 + SWC_F_MDS_2 + SWC_F_MDS_3) / 3) / 100, # 30cm depth mean. Percent to fraction
  g = G_F_MDS,              # W m-2
  le = LE_F_MDS,            # W m-2
  h = H_F_MDS,              # W m-2
  nee = NEE_VUT_REF * 12 / 1000000 / 1000 * dt,        # µmol m-2 s-1 to kg m-2 dt-1
  reco = RECO_NT_VUT_REF * 12 / 1000000 / 1000 * dt,   # µmol m-2 s-1 to kg m-2 dt-1
  gpp = GPP_NT_VUT_REF * 12 / 1000000 / 1000 * dt,     # µmol m-2 s-1 to kg m-2 dt-1
  TIMESTAMP_START = NULL,
  TIMESTAMP_END = NULL,
  NIGHT = NULL,
  TS_F_MDS_5 = NULL,
  LE_RANDUNC = NULL,
  H_RANDUNC = NULL,
  NEE_VUT_REF_JOINTUNC = NULL,
  .keep = "unused"
)

