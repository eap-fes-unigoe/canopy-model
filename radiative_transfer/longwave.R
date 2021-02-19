## simplified longwave model (assumes there is no upward scattering)

#' Calculate the longwave radiation absorbed by the canopy with sunlit and shaded components
#' @param lw_sky_d longwave radiation above the canopy in W m-2
#' @param t_leaf temperature leaves (in Kelvin)
#' @param t_soil temperature of soil (in Kelvin)
#' @param LAI Leaf Area Index
#' @param Kb Direct beam extiction coefficient
#' @param Kd Diffuse exiction coefficient
#' @param em_leaf Longwave emissivity of leaf
#' @param em_soil Longwave emissivity of soil
#'
#' @return list of lc, lg, lc_sun, lc_sha, l_up, l_down
longwave_radiation <- function(lw_sky, LAI, t_leaf, t_soil, Kb, Kd, em_leaf, em_soil){
  ## commonly used terms--
  sigma <- 5.67e-08   # Stefan-Boltzmann constant W m−2 K−4
  
  lw_soil_emit <- em_soil * sigma * t_soil^4
  lw_leaf_emit <- em_leaf * sigma  * t_leaf^4

  ## Equation 14.134
  lw_down_trans <- function(x)
    lw_sky * (1 - em_leaf * (1-exp(-Kd * x)))
  lw_down_emit <- function(x)
    lw_leaf_emit *(1-exp(-Kd * x))

  lw_down <- function(LAI) lw_down_emit(LAI) + lw_down_trans(LAI)


  ## Equation 14.135

  lw_up_trans <- function(x) {
    lw_soil_emit * (1 - em_leaf * (1-exp(-Kd * (LAI - x))))
  }

  lw_up_emit <- function(x)
    lw_leaf_emit * (1-exp(-Kd * (LAI - x)))

  lw_up <- function (x) lw_up_trans(x) + lw_up_emit(x)
  ## Equation 14.137
  perc_abs <-  1-exp(-Kd * LAI) # amount absorbed
  lc <- perc_abs * (em_leaf * (lw_sky + lw_soil_emit)
         - 2 * lw_leaf_emit)

  ## Equation 14.138
  lg<- lw_down(LAI) - lw_soil_emit # Lw adboserbed by the soil


  ### --- Sunlit and shaded leaves ---

  # Sunlit Eq. 14.140

  lc_sun <-
  (
      (
        (em_leaf * (lw_sky - sigma * t_leaf ^ 4 ) * Kd )
        / (Kd + Kb)
        * (1 - exp(-(Kd + Kb) * LAI))
      )
      +
      (
        (em_leaf *(lw_soil_emit - sigma * t_leaf ^ 4 ) * Kd)
        / (Kd - Kb)
        * (exp(-Kb * LAI)  - exp(-Kd * LAI))
      )
  )

  # Shaded Eq. 14.141

  lc_sha <- lc - lc_sun

  return(list(lc = lc,                        # Lw radiation absorbed by the canopy
              lg = lg,                        # Lw radiation absorbed by the soil
              lc_sun = lc_sun,                # Lw radiation absorbed by the sunlit canopy
              lc_sha = lc_sha,                # Lw radiation absorbed by the shaded canopy
              l_up = lw_up(0),             # Lw emitted into the sky
              l_down = lw_down(LAI)           # Lw reaching the soil
  ))
}
