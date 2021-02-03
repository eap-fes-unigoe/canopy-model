# setup input and fluxes for 2016 (because in 2018 lw is NA)
library(tidyverse)
library(readxl)
dt <- 3600
source("setup_parameters.R")
source("fun_calc_radiative_transfer.R")

path <- "../data/FLX_DE-Hai_FLUXNET2015_FULLSET_HH_2016-2018_beta-3_for_class.xlsx"
read_fluxnet_data <- function(path) {
  read_excel(path, na = "-9999") %>%
    select("Date/Time", SW_IN_F, SW_DIF, SW_OUT, LW_IN_F, LW_OUT, TS_F_MDS_1, TA_F, NIGHT) %>% # Columns interesting for radiative model
    rename(datetime = "Date/Time") %>%
    transmute(datetime = force_tz(datetime, "Etc/GMT+1" ),
              sw_dif = SW_DIF,
              sw_in = SW_IN_F,
              lw_in = LW_IN_F,
              t_leaf = 273.15 + TA_F,       # Temperature of air aproximation for leaf T for now
              t_soil = TS_F_MDS_1 + 273.15,
              sw_out = SW_OUT,
              lw_out = LW_OUT
    )
}
data <- read_fluxnet_data(path) %>%
    filter(datetime < as_date("2017-01-01"))

input <- tibble()
state <- select(data, t_leaf = t_leaf, t_soil = t_soil)

