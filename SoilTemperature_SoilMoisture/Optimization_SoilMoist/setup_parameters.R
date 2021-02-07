# Setup parameters

pars_full <- read.csv(parsfile)
# Parameters are extracted and then converted into a list for easy handling
pars <- pars_full$value
names(pars) <- pars_full$name
pars <- as.list(pars)
