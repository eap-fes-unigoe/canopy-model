#fun_calc_an_gs <- function(met,state_last,pars,par_sun,par_sha,LAI,LAI_sun) {
fun_calc_an_gs <- function(met,state_last,pars,out) {

  # leaf temperature placeholder if leaf temperature is not available
  if(is.na(state_last$tleaf)){
    state_last$tleaf <- met$tair
    #warning("No leaf temperature value available")
  }

  ## Boundary layer conductance for CO2 and H20, workaround in case its not available
  if(is.na(state_last$tleaf)){
    source("photosynthesis_stomatalconductance/LeafBoundaryLayer.R") # calculate h2o without group 4
    blfluxes = LeafBoundaryLayer(state_last,met,pars)
    state_last$gbw = blfluxes[1]
    state_last$gbc = blfluxes[2]
  } else {
    state_last$gbc = CO2LeafBoundaryLayer(state_last,met,pars)
  }

  # transforming W/m2 to μmol photon.m2 leaf/s by multiplication with 2
  # if radiation script not used, default on PAR.R for testing purposes
  #calculation of LAI_sha
  if(is.na(out$ic_sun)){
    source("photosynthesis_stomatalconductance/PAR.R")
    par_sun <- PAR(met$sw_in)
    par_sha <- PAR(met$sw_in)
  } else {
    #par_sun <- out$ic_sun * 4.6
    #par_sha <- out$ic_sha * 4.6
    par_sun <- out$ic_sun * 2
    par_sha <- out$ic_sha * 2
    #LAI_sun <- out$LAI_sunlit
    #LAI_sha <- out$LAI - out$LAI_sunlit
  }

  # calculate photosynthesis and stomatal conductance for sunlit and shaded leaves
  an_gs_sun <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,par_sun)
  an_gs_sha <- calc_fun_Photosynthesis_StomatalConductance(met,state_last,pars,par_sha)
  #the output here for an, ag, aj, ap, ac is umol CO2/m2 ground/s and for gs is mol H2O/m2 ground/s
  # LAI has already been included by using the sunlit and shaded leafs radiation radiation

  # leaf m-2 -> ground m-2 by x LAI
  #here, all values for sun and shaded are added, to get total values per ground area.
  # There are some variables in flux, for which adding doesn't make sense!?
  an_gs <- an_gs_sun
  for (flu in 1:length(an_gs_sun)) {
    an_gs[flu] <- as.numeric(an_gs_sun[flu]) + as.numeric(an_gs_sha[flu])
    #an_gs[flu] <- as.numeric(an_gs_sun[flu]) * LAI_sun + as.numeric(an_gs_sha[flu]) * LAI_sha
  }
  return(an_gs)
}
