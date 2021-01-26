## setup_Cpools.R
## C pool simulation setup

# Soil temperature effect on decomposition function
# Source: Bonan Ecosystem Modeling book, page 327
# Ts: soil temprature (K), k: modifier (unitless)
fun_kmod_Ts <- function (Ts) {
  kmod_Ts <- 0.56 + 1.46 / pi * atan ( 0.031 * pi * (Ts - 288.85) )
}

# Soil moisture
# Source: Bonan Ecosystem Modeling book, page 328
# S_e: volume soil water / volume soil pore space (uniteless)
fun_kmod_Ms <- function (S_e, a, b, c, d) {
  e <- d * (b-a) / (a-c)
  kmod_Ms <- ( (S_e - b) / (a - b) ) ^ e * ( (S_e - c) / (a - c) ) ^ d
}

# --- number of pools
npool <- 9

# --- NPP partitioning: B[i,1] = NPP partitioning to pool i

B <- matrix(0, npool, 1)

B[1,1] <- 0.25            # leaf
B[2,1] <- 0.55            # fine root
B[3,1] <- 0.20            # wood
B[4,1] <- 0               # metabolic litter
B[5,1] <- 0               # structural litter
B[6,1] <- 0               # coarse woody debris
B[7,1] <- 0               # fast SOM
B[8,1] <- 0               # slow SOM
B[9,1] <- 0               # passive SOM

K <- matrix(0, npool, npool)   # zero array elements
# units in / year
K[1,1] <- 1.12      # leaf
K[2,2] <- 0.10      # fine root
K[3,3] <- 0.025     # wood
K[4,4] <- 10.0      # metabolic litter
K[5,5] <- 0.95      # structural litter
K[6,6] <- 0.49      # coarse woody debris
K[7,7] <- 1.97      # fast SOM
K[8,8] <- 0.108     # slow SOM
K[9,9] <- 0.0024    # passive SOM

K <- K / syear * dt

# --- carbon transfer matrix: A(i,j) = fractional carbon flow from pool j that enters pool i

A <- matrix(0, npool, npool)

A[1,1] <- -1
A[2,2] <- -1
A[3,3] <- -1
A[4,4] <- -1
A[5,5] <- -1

A[6,6] <- -1
A[7,7] <- -1
A[8,8] <- -1
A[9,9] <- -1

A[4,1] <- 0.67
A[5,1] <- 0.33
A[4,2] <- 0.58
A[5,2] <- 0.42
A[6,3] <- 1.00
A[7,4] <- 0.45
A[7,5] <- 0.36
A[8,5] <- 0.14
A[7,6] <- 0.24
A[8,6] <- 0.28
A[8,7] <- 0.39
A[9,7] <- 0.006
A[9,8] <- 0.003

# --- environmental scalar: xi(i,i) <- environmental scalar for pool i
xi <- matrix(0, npool, npool)

xi[1,1] <- 1.01
xi[2,2] <- 1.0
xi[3,3] <- 1.0
xi[4,4] <- 1.0
xi[5,5] <- 1.0
xi[6,6] <- 1.0
xi[7,7] <- 1.0
xi[8,8] <- 1.0
xi[9,9] <- 1.0

vars_Cpools <- list(B, K, A, xi)
