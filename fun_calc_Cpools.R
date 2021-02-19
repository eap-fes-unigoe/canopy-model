## fun_calc_Cpools.R
## Function that calculates changes in C pool at every time step

fun_calc_Cpools <- function(pars, state_last, Cpools, vars_Cpools, fun_kmod_Ms, fun_kmod_Ts, site) {

  list2env(vars_Cpools, envir = environment())

  NPP <- site$gpp - site$reco/2; if(NPP<0) NPP <- 0 # state_last$NPP # A rough estimation of NPP from site data
  Ts <- site$tsoil # state_last$tsoil
  Ms_vol <- site$swc # state_last$swc
  Ms_ps  <- Ms_vol / pars$ps
  kmod_Ts <- fun_kmod_Ts(Ts)
  kmod_Ms <- fun_kmod_Ms(S_e = Ms_ps, a = pars$kmod_a, b = pars$kmod_b, c = pars$kmod_c, d = pars$kmod_d)
  kmod <- kmod_Ts * kmod_Ms
  for(x in 4:9) {xi[x,x] <- kmod}

  # This calculates changes of different pools
  for (i in 1:npool) {
    dC[i,1] <- NPP * B[i,1] - xi[i,i] * K[i,i] * Cpools[i,1];
    for (j in 1:npool) {
      if (j != i)
        dC[i,1] <- dC[i,1] + A[i,j] * xi[j,j] * K[j,j] * Cpools[j,1];
    }
  }

  # Heterotrophic respiration
  RH = xi[9,9] * K[9,9] * Cpools[9,1];
  for (j in 4:8) {
    suma <- 0;
    for (i in 4:9) {
      if (i != j)
        suma <- suma + A[i,j];
    }
    RH <- RH + (1 - suma) * xi[j,j] * K[j,j] * Cpools[j,1];
  }

  # update pools
  for (i in 1:npool) {
    Cpools[i,1] <- Cpools[i,1] + dC[i,1]
  }

  return(Cpools)
}
