
# Boundary layer conductance and leaf tempereatur form group 4

flux$gbv = # boundary layer conductance for water from group 4
flux$gbc = # boundary layer conductance for CO2 from group 4
flux$tleaf = # leaf temperature from group 4


# entropy terms in dependance of air T

leaf$vcmaxse = 668.39 - 1.07 * atmos$tair
leaf$jmaxse = 659.7 - 0.75 * atmos$tair
