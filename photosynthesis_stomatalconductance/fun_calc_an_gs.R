#fun_calc_an_gs <- function(met,state_last,pars,par_sun,par_sha,LAI,LAI_sun) {
fun_calc_an_gs <- function(met,state_last,pars,out) {

  # leaf temeperature placeholder if leaf tempearatre is nor avaliable
  if(is.na(state_last$tleaf)){
    state_last$tleaf <- met$tair
    warning("No leaf temperature value available")
    }

  #calculation of LAI_sha
  LAI_sun <- out$LAI_sunlit
  LAI_sha <- out$LAI - out$LAI_sunlit

  # transforming W/m2 to μmol photon.m2 leaf/s by multiplication with 4.6
  # if radiation script not used, default on PAR.R
  if(is.na(out$ic_sun)){
    source("photosynthesis_stomatalconductance/PAR.R")
    par_sun <- PAR(met$sw_in * LAI_sun/out$LAI)
    par_sha <- PAR(met$sw_in * LAI_sha/out$LAI)
  } else {
    par_sun <- out$ic_sun * 4.6
    par_sha <- out$ic_sha * 4.6
  }

  # calculate photosynthesis and stomatal conductance for sunlit and shaded leaves
  an_gs_sun <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,par_sun)
  an_gs_sha <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,par_sha)

  # leaf m-2 -> ground m-2 by x LAI
  an_gs <- an_gs_sun
  for (flu in 1:length(an_gs_sun)) {
    an_gs[flu] <- as.numeric(an_gs_sun[flu]) + as.numeric(an_gs_sha[flu])
    #an_gs[flu] <- as.numeric(an_gs_sun[flu]) * LAI_sun + as.numeric(an_gs_sha[flu]) * LAI_sha
  }
  return(an_gs)
}
