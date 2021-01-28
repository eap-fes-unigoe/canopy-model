# Load the input data

library(dplyr)

input <- read.csv("data/Hainich_2018_input.csv")

# Initial variable selection, renaming and conversion
input <- input %>% mutate(
  time = 1:nrow(input),
  tair = TA_F + 273.15,  # Celsius to Kelvin
  p = P_F,
  sw_in = SW_IN_F,
  lw_in = LW_IN_F,
  vpd = VPD_F * 100,     # hPa to Pa
  pa = PA_F * 1000,      # kPa to Pa
  ws = WS_F,
  rh = RH / 100,         # percent to fraction
  sw_dif = SW_DIF,
  co2 = CO2_F_MDS,
  TIMESTAMP_START = NULL,
  TIMESTAMP_END = NULL,
  NIGHT = NULL,
  .keep = "unused"
)
