
# create paramater list from pars table
#pars_values <- pars$value
#names(pars_values) = pars$name
#parslist = as.list(pars_values)

#parsconst = pars$value
#names(parsconst) = pars$name
#parsconst = pars$value[which(pars$type == "physical constant")]
#names(parsconst) = pars$name[which(pars$type == "physical constant")]
#physcon = as.list(parsconst)
#physcon$rgas = physcon$R


#parsleaf = pars$value[which(pars$type == "photosynthesis_leaf")]
#names(parsleaf) = pars$name[which(pars$type == "photosynthesis_leaf")]
#leaf = as.list(parsleaf)


#parsflux = pars$value[which(pars$type == "photosynthesis_leaf")]
#names(parsleaf) = pars$name[which(pars$type == "photosynthesis_leaf")]
#leaf = as.list(parsleaf)

#fill lists
#physcon
#flux
#leaf = parslist$
#etc


# --- Waveband indices for ps_sc$visible and near-infrared

ps_sc$vis = 1;
ps_sc$rho[ps_sc$vis] = 0.057;
ps_sc$tau[ps_sc$vis] = 0.048;


#from LeafPhydiologyParams.R
# done with LeafPhysiologyParams.R

leaf$jmax25 = 1.67 * leaf$vcmax25;
leaf$rd25 = 0.015 * leaf$vcmax25;

fth25 = function(hd, se) {1 + exp((-hd + se*(physcon$tfrz+25)) / (physcon$rgas*(physcon$tfrz+25)))};
leaf$vcmaxc = fth25 (leaf$vcmaxhd, leaf$vcmaxse);
leaf$jmaxc  = fth25 (leaf$jmaxhd, leaf$jmaxse);
leaf$rdc    = fth25 (leaf$rdhd, leaf$rdse);






