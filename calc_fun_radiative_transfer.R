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
#' - sw_sky_b direct beam shortwave radiation incoming
#' - sw_sky_d diffuse shortwave radiation incoming
#' - lw_sky longwave radiation incoming
#'
#' @param params a list of the model parameters containing at least the following elements
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
calc_fun_radiative_transfer <- function(input, params){

    # Calc all the intermediate parameters
    LAI <- get_day_LAI(input$datetime, params$max_LAI, params$min_LAI, params$leaf_out, params$leaf_full, params$leaf_fall, params$leaf_fall_complete)
    zenith <- get_zenith(input$datetime, params$lat, params$lon) # should be 15 mins earlier because is a better average value of the half an hour interval
    Kb <- get_Kb(zenith)
    Kd <- get_Kd(LAI)
    beta <- get_beta(params$rho_leaf, params$tau_leaf)
    beta0 <- get_beta0(zenith, Kb, Kd_2stream, params$omega_leaf)


    shortwave <- shortwave_radiation(input$sw_sky_b, input$sw_sky_d, LAI, Kb, Kd_2stream, beta, beta0 , params$omega_leaf,
                                     params$clump_OMEGA, params$alb_soil_b, params$alb_soil_d)
    longwave <- longwave_radiation(input$lw_sky, LAI, input$t_leaf, input$t_soil, Kb, Kd, params$em_leaf, params$em_soil)

    LAI_sunlit <- get_LAI_sunlit(LAI, Kb, params$clump_OMEGA)
    LAIs <- c(LAI=LAI, LAI_sunlit=LAI_sunlit)

    return(data.frame(c(shortwave, longwave, LAIs)))

}
