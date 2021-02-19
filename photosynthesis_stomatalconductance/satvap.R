satvap = function(tc){

# Compute saturation vapor pressure and change in saturation vapor pressure
# with respect to temperature. Polynomial approximations are from:
# Flatau et al. (1992) Polynomial fits to saturation vapor pressure.
# Journal of Applied Meteorology 31:1507-1513. Input temperature is Celsius.

# --- For water vapor (temperature range is 0C to 100C)

a0 =  6.11213476;        b0 =  0.444017302;
a1 =  0.444007856;       b1 =  0.286064092e-01;
a2 =  0.143064234e-01;   b2 =  0.794683137e-03;
a3 =  0.264461437e-03;   b3 =  0.121211669e-04;
a4 =  0.305903558e-05;   b4 =  0.103354611e-06;
a5 =  0.196237241e-07;   b5 =  0.404125005e-09;
a6 =  0.892344772e-10;   b6 = -0.788037859e-12;
a7 = -0.373208410e-12;   b7 = -0.114596802e-13;
a8 =  0.209339997e-15;   b8 =  0.381294516e-16;


tc = min(tc, 100);

# --- Saturation vapor pressure (esat, mb) and derivative (desat, mb)

esat  = a0 + tc*(a1 + tc*(a2 + tc*(a3 + tc*(a4 + tc*(a5 + tc*(a6 + tc*(a7 + tc*a8)))))));

# --- Convert from mb to Pa

esat  = esat  * 100;

return(esat)

}
