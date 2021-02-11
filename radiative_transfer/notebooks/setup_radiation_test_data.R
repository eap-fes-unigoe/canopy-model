# setup input and fluxes for 2016 (because in 2018 lw is NA)
library(tidyverse)
library(progress)
dt <- 3600
source("setup_parameters.R")

source("fun_calc_radiative_transfer.R")

input <- read.csv(file.path('data','Hainich_2018_input.csv'))
fluxes <- read.csv(file.path('data','Hainich_2018_fluxes.csv'))

# Initial variable selection, renaming and conversion
input <- input %>% mutate(
  time = 1:nrow(input),
  datetime = force_tz(as_datetime(Date.Time), "Etc/GMT+1"),
  tair = TA_F + 273.15,  # Celsius to Kelvin
  p = P_F/1000,          # mm time_step-1 to m3 m-2 time_step-1
  sw_in = SW_IN_F,       # W m-2
  lw_in = LW_IN_F,       # W m-2
  ppfd_in = PPFD_IN,     # µmol m-2 s-1
  vpd = VPD_F * 100,     # hPa to Pa
  pa = PA_F * 1000,      # kPa to Pa
  ws = WS_F,             # m s-1
  rh = RH / 100,         # percent to fraction
  sw_dif = SW_DIF,       # W m-2
  co2 = CO2_F_MDS,       # µmol mol-1
  night = as_factor(as.integer(NIGHT)),
  .keep = "unused"
)


# Initial variable selection, renaming and conversion
# tsoil and swc means across soil depths don't take into account layer thickness or properties.
# We ignore this for the purpose of this excersice.
fluxes <- fluxes %>% mutate(
  time = 1:nrow(fluxes),
  sw_out = SW_OUT,          # W m-2
  tsoil = ((TS_F_MDS_1 + TS_F_MDS_2 + TS_F_MDS_3 + TS_F_MDS_4 + TS_F_MDS_5) / 5) + 273.15, # 30cm depth mean. Celsius to Kelvin
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
  lw_out = LW_OUT,
  TS_F_MDS_5 = NULL,
  LE_RANDUNC = NULL,
  H_RANDUNC = NULL,
  NEE_VUT_REF_JOINTUNC = NULL,
  .keep = "unused"
)

state <- tibble(t_leaf = input$tair, t_soil = fluxes$tsoil)

# Utility funcs

#' Full radiative transfer model with potentially new paramameters that overwrite the defaults
rad_transf_new_p <- function (new_p=list()){
  pars <- merge_lists(pars, new_p)
  rad_transf(pars = pars)
}

#' Full radiative transfer model

d_input <- input
d_state <- state
d_pars <-  pars
d_dt <- dt
rad_transf <- function(input = d_input, state = d_state, pars = d_pars, dt = d_dt){
  out <- data.frame()
  pb <- setup_pb()
  for (i in seq_along(input$datetime)){
    rad<- radiative_transfer_step_debug(input[i,], state[i,], pars, dt)
    out[i, names(rad)] <- rad
    pb$tick()
  }
  return(out)
}

setup_pb <- function(){
  progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed: :elapsedfull | ETA: :eta]",
                       total = length(input$datetime),
                       clear = FALSE)
}

#' merges two list, in case of conflicts uses the second list value
merge_lists <- function (list1, list2){
  l <- list1
  for (name in names(list2)){
    l[name] <- list2[name]
  }
  return(l)
}

#' transpose an aggreagated sensisivity dataframe
invert_sens <- function(df){
    vars <- df$var
    df <- select(df, -var)
    pars <- names(df)
    df <- as_tibble(t(df))
    df <- cbind(pars, df)
    names(df) <- c("var", vars)
    return(df)
}

filter_sens <- function(sens_model, vars = NULL, pars = NULL){
  sens_model <- select(sens_model, -x)
  if (!is.null(vars)) {sens_model <- filter(sens_model, var %in% vars)}
  if (!is.null(pars)) {sens_model <- select(sens_model, all_of(c("var", pars)))}
  return(sens_model)
}

detailed_sens <- function(sens_model, func){
  sens_model %>%
        group_by(var) %>%
        summarize_all(func) %>%
        invert_sens
}



Kd_2stream <- get_two_stream_Kd() # This is a costant value that depends only on the leaf angle distribution
#' This is the rad_transf function that can take custom parameters from the input
radiative_transfer_step_debug <- function(input, state, pars, dt){
    # Calc all the intermediate parameters
    # Possible optimization here as not all the paramaters changes every step
    LAI <- ifelse("LAI" %in% names(input), input$LAI, get_day_LAI(input$datetime, pars$max_LAI, pars$leaf_out, pars$leaf_full, pars$leaf_fall, pars$leaf_fall_complete))
    radiation_PAI <- max(LAI, pars$min_radiation_PAI) # During winter the are no leaves but there are still branches that interact with light
    avg_datetime <- input$datetime - duration(dt/2) # calculating the zenith at the mid of the interval
    zenith <- ifelse("zenith" %in% names(input), input$zenith, get_zenith(avg_datetime, pars$lat, pars$lon))
    Kb <- ifelse("Kb" %in% names(input), input$Kd, get_Kb(zenith, max_Kb = 1000)) # 1000 is an arbitraty high number
    Kd <- ifelse("kd" %in% names(input), input$Kb, get_Kd(LAI))
    omega_leaf <- pars$rho_leaf + pars$tau_leaf
    beta <- get_beta(pars$rho_leaf, pars$tau_leaf)
    beta0 <- get_beta0(zenith, Kb, Kd_2stream, omega_leaf)

    # the incoming shortwave is the total diffure + direct. Due to sensor errors the difference can be negative so the min possible value is set to 0
    sw_sky_b <- max(input$sw_in - input$sw_dif, 0)
    shortwave <- shortwave_radiation(sw_sky_b, input$sw_dif, radiation_PAI, Kb, Kd_2stream, beta, beta0 , omega_leaf,
                                     pars$clump_OMEGA, pars$alb_soil_b, pars$alb_soil_d)
    longwave <- longwave_radiation(input$lw_in, radiation_PAI, state$t_leaf, state$t_soil, Kb, Kd, pars$em_leaf, pars$em_soil)

    LAI_sunlit <- get_LAI_sunlit(LAI, Kb, pars$clump_OMEGA)
    vars <- c(LAI=LAI, LAI_sunlit=LAI_sunlit, radiation_PAI = radiation_PAI, Kb=Kb, Kd=Kd, beta=beta, beta0= beta0, zenith=zenith)

    return(data.frame(c(shortwave, longwave, vars)))
}


breaks_12hours <- function (limits){
  seq(limits[1], limits[2], by="12 hours")
}

labels_12hours <- function (limits){
  format(breaks_12hours(limits), "%d %b %Ih")
}
