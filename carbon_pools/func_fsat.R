# Here goes a title 
# Some documentation

a <- 0.6 #0.55 
b <- 1.27 #1.7
c <- 0.0012 # -0.007
d <- 2.84 # 3.22
e <- d * (b - a) / (a - c)
fsat <- seq(0,1,0.01)
func_fsat <- function(fsat) {((fsat - b)/(a - b))^e * ((fsat - c) / (a - c))^d}
eff_fsat <- func_fsat(fsat) 
plot(eff_fsat~fsat)
