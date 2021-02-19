## solving for direct/diffuse shortwave using the two stream approximation

#' Calculate the direct shortwave radiation absorbed by the canopy with sunlit and shaded components
#' @param sw_sky_b direct beam radiation above the canopy in W m-2
#' @param LAI Leaf Area Index
#' @param Kb Direct beam extiction coefficient
#' @param Kd Diffuse exiction coefficient
#' @param beta Fraction of diffuse radiation upward scattered
#' @param beta0 Fraction of direct beam radiation upward scattered
#' @param omega_leaf Leaf scattering coefficient (reflectace + trasmittance)
#' @param clump_OMEGA Canopy clumping coefficient
#' @param alb_soil_b Albedo soil for direct beam radiation
#'
#' @return list of ic, ic_sun, ic_sha
direct_beam_radiation <- function(sw_sky_b, LAI, Kb, Kd, beta, beta0, omega_leaf, clump_OMEGA, alb_soil_b, alb_soil_d){
  
  # defining common terms between direct/diffuse
  # --- Common terms: Eqs. (14.87) - (14.91)
  
  b <- (1 - (1 - beta) * omega_leaf) * Kd
  c <- beta * omega_leaf * Kd
  h <- sqrt(b*b - c*c)
  u <- (h - b - c) / (2 * h)
  v <- (h + b + c) / (2 * h)
  #d = omega(p,ib) * Kb(p) * atmos.swskyb(p,ib) / (h*h - Kb(p)*Kb(p));
  g1 <- ((beta0 * Kb - b * beta0 - c * (1 - beta0)) *
         omega_leaf * Kb * sw_sky_b / (h^2 - Kb^2))
  g2 <- ((1 - beta0) * Kb + c * beta0 + b * (1 - beta0)) * omega_leaf * Kb * sw_sky_b / (h*h - Kb^2)
  
  # --- Exponential functions of leaf area
  
  s1 <- function(x) exp(-h * clump_OMEGA * x);
  s2 <- function(x) exp(- Kb * clump_OMEGA * x)
  
  
  
  # --- Direct beam solution
  # n1 (Eq. 14.92) and n2 (14.93)
  
  num1 <- v * (g1 + g2 * alb_soil_b + alb_soil_b * sw_sky_b) * s2(LAI)
  num2 <- g2 * (u + v * alb_soil_b) * s1(LAI)
  den1 <- v * (v + u * alb_soil_b) / s1(LAI)
  den2 <- u * (u + v * alb_soil_b) * s1(LAI)
  n2b <- (num1 - num2) / (den1 - den2)
  n1b <- (g2 - n2b * u) / v
  
  # Scattered direct beam fluxes:
  # iupwb - direct beam flux scattered upward above cumulative LAI (W/m2); Eq. (14.94)
  # idwnb - direct beam flux scattered downward below cumulative LAI (W/m2); Eq. (14.95)
  
  i_upw_b <-  function(x) -g1 * s2(x) + n1b * u * s1(x) + n2b * v / s1(x)
  i_dwn_b <-  function(x)  g2 * s2(x) - n1b * v * s1(x) - n2b * u / s1(x)
  
  # icb - direct beam flux absorbed by canopy (W/m2); Eq. (14.97)
  
  ic_b <- sw_sky_b * (1 - s2(LAI)) - i_upw_b(0) + i_upw_b(LAI) - i_dwn_b(LAI)

  # ig_b - direct beam flux absorbed by the soil; Eq 14.98

  ig_b <- ((1- alb_soil_d) * i_dwn_b(LAI)) + ((1 - alb_soil_b) * sw_sky_b * s2(LAI))
  
  # icsunb - direct beam flux absorbed by sunlit canopy (W/m2); Eq. (14.114)
  # icshab - direct beam flux absorbed by shaded canopy (W/m2); Eq. (14.115)
  
  a1b <- -g1 *      (1 - s2(LAI)*s2(LAI)) / (2 * Kb) + 
    n1b * u * (1 - s2(LAI)*s1(LAI)) / (Kb + h) + n2b * v * (1 - s2(LAI)/s1(LAI)) / (Kb - h)
  a2b <-  g2 *      (1 - s2(LAI)*s2(LAI)) / (2 * Kb) -
    n1b * v * (1 - s2(LAI)*s1(LAI)) / (Kb + h) - n2b * u * (1 - s2(LAI)/s1(LAI)) / (Kb - h)
  
  ic_sun_b <- (1 - omega_leaf) * ((1 - s2(LAI)) * sw_sky_b + Kd * (a1b + a2b) * clump_OMEGA)
  ic_sha_b <- ic_b - ic_sun_b

  i_up_b <- i_upw_b(0)
  i_down_b <- i_dwn_b(LAI)
  
  return(list(ic_b = ic_b, ic_sun_b=ic_sun_b, ic_sha_b=ic_sha_b, ig_b = ig_b, i_up_b = i_up_b, i_down_b = i_down_b))
}

#' Calculate the diffuse shortwave radiation absorbed by the canopy with sunlit and shaded components
#' @param sw_sky_d diffuse radiation above the canopy in W m-2
#' @param LAI Leaf Area Index
#' @param Kb Direct beam extiction coefficient
#' @param Kd Diffuse exiction coefficient
#' @param beta Fraction of diffuse radiation upward scattered
#' @param beta0 Fraction of direct beam radiation upward scattered
#' @param omega_leaf Leaf scattering coefficient (reflectace + trasmittance)
#' @param clump_OMEGA Canopy clumping coefficient
#' @param alb_soil_d Albedo soil for diffuse radiation
#'
#' @return list of ic, ic_sun, ic_sha, ig, i_up_d, i_down_d
diffuse_radiation <- function(sw_sky_d, LAI, Kb, Kd, beta, beta0, omega_leaf, clump_OMEGA, alb_soil_d){
  
  # defining common terms between direct/diffuse
  # --- Common terms: Eqs. (14.87) - (14.91)
  
  b <- (1 - (1 - beta) * omega_leaf) * Kd
  c <- beta * omega_leaf * Kd
  h <- sqrt(b*b - c*c)
  u <- (h - b - c) / (2 * h)
  v <- (h + b + c) / (2 * h) 
  #g1 <- ((beta0 * Kb - b * beta0 - c * (1 - beta0))
  #       * omega_leaf * Kb * sw_sky_d / (h^2 - Kb^2))
  #g2 <- ((1 - beta0) * Kb + c * beta0 + b * (1 - beta0)) * omega_leaf * Kb * sw_sky_d / (h*h - Kb^2)
  
  # --- Exponential functions of leaf area
  
  s1 <- function(x) exp(-h * clump_OMEGA * x);
  s2 <- function(x) exp(-Kb * clump_OMEGA * x)

  # --- Diffuse solution
  
  # n1d and n2d (Eq. 14.99) and n2 (14.100)
  num <- sw_sky_d * (u + v * alb_soil_d) * s1(LAI)
  den1 <- v * (v + u * alb_soil_d) / s1(LAI)
  den2 <- u * (u + v * alb_soil_d) * s1(LAI)
  n2d <- num / (den1 - den2)
  n1d <- -(sw_sky_d + n2d * u) / v
  
  # Scattered diffuse fluxes:
  # iupwd - diffuse flux scattered upward above cumulative LAI (W/m2); Eq. (14.101)
  # idwnd - diffuse flux scattered downward below cumulative LAI (W/m2); Eq. (14.102)

  i_upw_d <- function(x)  n1d * u * s1(x) + n2d * v / s1(x)
  i_dwn_d <- function(x) -n1d * v * s1(x) - n2d * u / s1(x)
  
  
  #' icd - diffuse flux absorbed by canopy (W/m2); Eq. (14.104)
  ic_d <- sw_sky_d - i_upw_d(0) + i_upw_d(LAI) - i_dwn_d(LAI)

  #' ig_b - diffuse flux absorbed by the soil; Eq 14.105
  ig_d <- (1 - alb_soil_d) * i_dwn_d(LAI)

  # icsund - diffuse flux absorbed by sunlit canopy (W/m2); Eq. (14.120)
  # icshad - diffuse flux absorbed by shaded canopy (W/m2); Eq. (14.121)
  
  a1d <-  n1d * u * (1 - s2(LAI)*s1(LAI)) / (Kb + h) + n2d * v * (1 - s2(LAI)/s1(LAI)) / (Kb - h)
  a2d <- -n1d * v * (1 - s2(LAI)*s1(LAI)) / (Kb + h) - n2d * u * (1 - s2(LAI)/s1(LAI)) / (Kb - h)
  
  ic_sun_d <- (1 - omega_leaf) * Kd * (a1d + a2d) * clump_OMEGA
  ic_sha_d <- ic_d - ic_sun_d

  i_up_d <-  i_upw_d(0)
  i_down_d <-  i_dwn_d(LAI)
  
  return(list(ic_d = ic_d, ic_sun_d = ic_sun_d, ic_sha_d = ic_sha_d, ig_d = ig_d , i_up_d = i_up_d, i_down_d = i_down_d))
}

#' Calculate the shortwave radiation absorbed by the canopy with sunlit and shaded components
#'
#' @param sw_sky_b direct beam radiation above the canopy in W m-2
#' @param sw_sky_d diffuse radiation above the canopy in W m-2
#' @param LAI Leaf Area Index
#' @param Kb Direct beam extiction coefficient
#' @param Kd Diffuse exiction coefficient
#' @param beta Fraction of diffuse radiation upward scattered
#' @param beta0 Fraction of direct beam radiation upward scattered
#' @param omega_leaf Leaf scattering coefficient (reflectace + trasmittance)
#' @param clump_OMEGA Canopy clumping coefficient
#' @param alb_soil_b Albedo soil for direct beam radiation
#' @param alb_soil_d Albedo soil for diffuse radiation
#'
#' @return list of ic, ic_sun, ic_sha
shortwave_radiation <- function(sw_sky_b, sw_sky_d, LAI, Kb, Kd, beta, beta0, omega_leaf, clump_OMEGA, alb_soil_b, alb_soil_d){
 
  ib <- direct_beam_radiation(sw_sky_b, LAI, Kb, Kd, beta, beta0, omega_leaf, clump_OMEGA, alb_soil_b, alb_soil_d)
  id <- diffuse_radiation(sw_sky_d, LAI, Kb, Kd, beta, beta0, omega_leaf, clump_OMEGA, alb_soil_d)
  
  ic <- ib$ic_b + id$ic_d
  ic_sun <- ib$ic_sun_b + id$ic_sun_d
  ic_sha <- ib$ic_sha_b + id$ic_sha_d
  ig <- ib$ig_b + id$ig_d
  i_up <- ib$i_up_b + id$i_up_d
  i_down <- ib$i_down_b + id$i_down_d
  
  return(list(ic = ic, ic_sun = ic_sun, ic_sha = ic_sha, ig=ig, i_up = i_up, i_down = i_down))
  
}
