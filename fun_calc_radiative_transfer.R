# Radiative transfer model

source("radiative_transfer/shortwave.R")
source("radiative_transfer/longwave.R")
source("radiative_transfer/calc_parameters.R")


#' Radiative transfer model step
#'
#' This the core routine of the radiative trasfer model. It calls all the models function
#'
#' @param input A data frame row (or list) containing at least the following elements
#' - datetime
#' - sw_in total incoming shortwave radiation
#' - sw_dif diffuse shortwave radiation incoming
#' - lw_in longwave radiation incoming
#'
#' @param state A data frame row (or list) containing at least the following elements
#' - t_soil temperature of soil (in Kelvin)
#' - t_leaf temperature of leaves (in Kelvin)
#'
#' @param pars a list of the model parameters containing at least the following elements
#' - max LAI value in the summer
#' - min_LAI min value of LAI during winter, it is an aproximation that consider the total Plant Area Index as LAI
#' - leaf_out day leaves start in spring
#' - leaf_full day leaves reach max LAI
#' - leaf_fall day leaves start to fall
#' - leaf_fall_complete day all leaves are fallen
#'
#' - lat latidude
#' - lon longitude
#'
#' - rho_leaf Reflencance of leaf
#' - tau_leaf trasmissivity of leaf
#' - omega_leaf scattering coefficient of leaf
#' - clump_OMEGA canopy clumping coefficient
#' - alb_soil_b soil albedo direct beam
#' - alb_soil_d soil albedo diffuse
#'
#' - em_leaf emittivity of leaves
#' - em_soil emittivity of soil
#'
#' @return One row data Dataframe with
#' - ic Absorbed shortwave radiation from the canopy
#' - ic_sun Absorbed shortwave radiation from sunlit canopy
#' - ic_sha Absorbed shortwave radiation from shaded canopy
#' - ig Absorbed shortwave radiation from soil
#' - i_up Reflected shortwave radiation above the canopy
#' - i_down Transmitted shortwave radiation below the canopy
#' - lc Absorbed longwave radiation from the canopy
#' - lc_sun  Absorbed longwave radiation from sunlit canopy
#' - lc_sha  Absorbed longwave radiation from shaded canopy
#' - lg Absorbed longwave radiation from the soil
#' - l_up Emitted longwave radiation above the canopy
#' - l_down Transmitted longwave radiation below the canopy
#' - LAI Leaf Area Index
#' - LAI_sunlit LAI of sunlit canopy

# The Kd in the Two Stream model has a different value
Kd_2stream <- get_two_stream_Kd() # This is a costant value that depends only on the leaf angle distribution
fun_calc_radiative_transfer <- function(input, state, pars, dt){
    # Calc all the intermediate parameters
    # Possible optimization here as not all the paramaters changes every step
    LAI <- get_day_LAI(input$datetime, pars$max_LAI, pars$leaf_out, pars$leaf_full, pars$leaf_fall, pars$leaf_fall_complete)
    radiation_PAI <- max(LAI, pars$min_radiation_PAI) # During winter the are no leaves but there are still branches that interact with light
    avg_datetime <- input$datetime - duration(dt/2) # calculating the zenith at the mid of the interval
    zenith <- get_zenith(avg_datetime, pars$lat, pars$lon)
    Kb <- get_Kb(zenith, max_Kb = 1000) # 1000 is an arbitrary high number
    Kd <- get_Kd(LAI)
    omega_leaf <- pars$rho_leaf + pars$tau_leaf
    beta <- get_beta(pars$rho_leaf, pars$tau_leaf)
    beta0 <- get_beta0(zenith, Kb, Kd_2stream, omega_leaf)

    # the incoming shortwave is the total diffure + direct. Due to sensor errors teh difference can be negative so the min possible value is set to 0
    sw_sky_b <- max(input$sw_in - input$sw_dif, 0)
    shortwave <- shortwave_radiation(sw_sky_b, input$sw_dif, radiation_PAI, Kb, Kd_2stream, beta, beta0 , omega_leaf,
                                     pars$clump_OMEGA, pars$alb_soil_b, pars$alb_soil_d)
    longwave <- longwave_radiation(input$lw_in, radiation_PAI, state$t_leaf, state$t_soil, Kb, Kd, pars$em_leaf, pars$em_soil)

    LAI_sunlit <- get_LAI_sunlit(LAI, Kb, pars$clump_OMEGA)
    LAIs <- c(LAI=LAI, LAI_sunlit=LAI_sunlit)

    return(data.frame(c(shortwave, longwave, LAIs)))
}
