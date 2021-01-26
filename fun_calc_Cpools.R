## fun_calc_Cpools.R
## Function that calculates changes in C pool at every time step

fun_calc_Cpools <- function(pars, state_last, vars_Cpools, fun_kmod_Ms, fun_kmod_Ts) {

  Ts <- state_last$Ts
  Ms_vol <- state_last$Ms_vol
  Ms_ps  <- Ms_vol / ps
  kmod_Ts <- fun_kmod_Ts(Ts)
  kmod_Ms <- fun_kmod_Ms(Ms_ps)
  kmod <- kmod_Ts * kmod_Ms
  for(x in 4:9) {xi[x,x] <- kmod}

  U <- NA


}
