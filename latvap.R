#function of Latent heat of vaporization -- latvap()
latvap<-function (tc, mmh2o){   ### same question here...
  # Latent heat of vaporization (J/mol) at temperature tc (degC)  
  val<- 2501.6 - 2.3773 * tc # Latent heat of vaporization (J/g) 
  val<- val * 1000           # Convert from J/g to J/kg 
  val<- val * mmh2o          # Convert from J/kg to J/mol  
  return(val) 
}