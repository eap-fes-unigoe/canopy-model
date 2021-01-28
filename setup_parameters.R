# Setup parameters

pars_temp <- read.csv("parameters.csv")
# Parameters are extracted and then converted into a list for easy handling
pars <- pars_temp$value
names(pars) <- pars_temp$name
pars <- as.list(pars)
rm(pars_temp)
