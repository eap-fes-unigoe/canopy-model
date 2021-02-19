# Supplemental program 17.1




# 
# I think there are 2 main things you need to do now:
# - Clean the code. There is a lot of code commented out, so it is not used. 
  #It is there to show that you can make the calculations in different ways. 
  #You can calculate the change in each pool line by line, or you can use the matrix calculations. 
  #They should all give the same result. Choose one and delete the other lines.

# - You should add the temperature and moisture base rate modifier functions. 
# You can base this on the equations 18.11, 18.12 and 18.13 (or different ones from literature if you want). 
# All the turnover base rates of the litter and soil pools should be thus modified by soil T and moisture. 
# The value f(T)*f(M) should be your scaling factor, so should replace your xi values in the code. 
# This scaling is also described in the book after equations 17.6 and 17.9. 
# Note that the scaling factors for all your litter and soil pools will be the same value, 
# because we are working with a 1-layer soil, so there is only 1 value for soil T and M at each time step.
# # Soil T and M you will later get from the other group. 
# For now just use a fixed value or check in the climate data file for Hainich forest.
# # Hope that helps. In all it is not much work you still need to do.



# -----------------------------
# BGC model with temperature and moisture base rate modifier functions
# -----------------------------

# rm(list=ls())
# --------------
# Read Input
# --------------

# fernando: it is generally recommended to save your data as a .csv and then read it with read.csv()
# Also, easier if T (now Ts) is a vector
library(readxl) 
soil_temp_day_avg <- read_excel("Tagesmittelwerte soil temp.xlsx")
Ts <- soil_temp_day_avg$day_avg

# --------------
# Functions
# --------------

# Figure 18.12, page 327
# fernando: T_row is unnecessary here. Just pass the value you want as T when calling the function
# Also, avoid using T for a variable name because R could confuse it with meaning TRUE. Changed to Ts (soil temp)
# f_T <- function ( T, T_row ) { 
#   T_kelvin <- T[T_row, ] + 273.15
f_T <- function (Ts) { 
  Ts_kelvin <- Ts + 273.15
  k <- 0.56 + 1.46 / pi * atan ( 0.031 * pi * (Ts_kelvin - 288.85) )
  return( k )
}


# Figure 18.13, page 328
# ge?ndert: a-b zu b-a, damit der Bruch nicht negativ ist!
# fernando: the problem was not the function but your Se values (see below).
f_Se <- function ( S_e ) {
  # Se <- ( (S_e - b) / (b - a) ) ^ e * ( (S_e - c) / (a - c) ) ^ d
  Se <- ( (S_e - b) / (a - b) ) ^ e * ( (S_e - c) / (a - c) ) ^ d
  return( Se )
}


# --------------
# Initialization
# --------------


# --- number of pools

npool <- 9


# --- NPP (gC/m2/day)

U <- 1000 / 365


# --- NPP partitioning: B[i,1] = NPP partitioning to pool i

B <- matrix(0, 9, 1)

B[1,1] <- 0.25            # leaf
B[2,1] <- 0.55            # fine root
B[3,1] <- 0.20            # wood
B[4,1] <- 0               # metabolic litter
B[5,1] <- 0               # structural litter
B[6,1] <- 0               # coarse woody debris
B[7,1] <- 0               # fast SOM
B[8,1] <- 0               # slow SOM
B[9,1] <- 0               # passive SOM


# --- base turnover rate: K[i,i] = base turnover rate for pool i [/day]
# fernando: here the rates have time units. You can adjust the time unit here, or after calculating the fluxes
# The time step is defined below as dt, and used after the flux calculations. I made some changes below and 
# made all in units seconds.
# It is useful to define the time quantities in seconds, e.g:
sec <- 1
shalfhour <- 1800
shour <- 3600
sday <- shour * 24
smonth <- sday * 30
syear <- sday * 365

# fernando: removed / 365 (you should then convert units later using dt)
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

#--- Adding soil temperature and soil moisture effect on K 

# soil moisture Kelyy et al. 2000 
# f(0) = f(Se) = Se = theta/theta_sat with theta = volumetric water content and theta_sat = porosity 

# fernando: here is one problem. you are using percent for theta but fraction for theta sat. 
# This is why your fraction in the function was negative and the value of sol_f_Se is enormous.
theta <- 0.335 #33.5    # Mittelwert aus 52000 Messungen, 5cm Tiefe 
theta_sat <- 0.48 # QUELLE Martinez 2004

S_e <- theta / theta_sat


# f(Se)= (Se-b/a-b)^e(Se-c/a-c)^d with a=0.6, b=1.27, c=0.0012, d=2.84 for fine-texture soils
a <- 0.6
b <- 1.27
c <- 0.0012
d <- 2.94
e <- d * (b-a) / (a-c)

# sol_f_Se <- f_Se( S_e ) # fernando: this should be inside the for loop, because theta will be a time series also


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

# fernando: this xi matrix is a substitute for sol_f_Se * sol_f_T
# See changes in the for loop

# --- environmental scalar: xi(i,i) <- environmental scalar for pool i
xi <- matrix(0, npool, npool)

xi[1,1] <- 1.01
xi[2,2] <- 1.00
xi[3,3] <- 1.00
xi[4,4] <- 0.40
xi[5,5] <- 0.40
xi[6,6] <- 0.40
xi[7,7] <- 0.40
xi[8,8] <- 0.40
xi[9,9] <- 0.40

xi_m <- xi # keep a matrix that is not changed to get equilibrium values

# --- initial pool size: C(i,1) = carbon for pool i (g C/m2)

C <- matrix(0, npool, 1)


#-------------------
# Time stepping loop
# ------------------

# fernando: dt is the time step now defined in number of seconds.
# --- length of time step (seconds)

dt <- sday

# --- convert rates and fluxes to dt time
K <- K / syear * dt
U <- U / sday * dt

# --- number of years to simulate 

nyears <- 1

# --- days per month

ndays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

# --- initialize step counter

nstep <- 0

# --- advance time

dC <- matrix(0, 9, 1)

# 
x1 <- c(0);
y1 <- c(0);
y2 <- c(0);
y3 <- c(0);
y4 <- c(0);
y5 <- c(0);

counter_days <- 0


# Hier haben wir in der inneren Schleife f?r Tag 1:26 Werte und ab Tag 27 Inf Werte,
# die sich dann nat?rlich durch alle weiteren Berechnungen ziehen, sodass keine 
# Grafiken erzeugt werden k?nnen.

# fernando: could make just one loop and save values at each time step. At the moment you are saving values
# only once every year
for (year in 1:nyears) {
    for (month in 1:12) {
        for (day in 1:ndays[month]) { # 1:26

          # durchg?ngige Nummerierung der Tage f?r Kompatibilit?t mit den Mittelwerten pro Tag in Excel
          # fernando: counter_days seems useful for saving the output, but what happens if
          # counter_days is larger than the length of you temperature vector?
          # Also, counter_days and nstep have the same value. One can be removed.
          
          counter_days <- counter_days + 1 # fernando: you don't need to end a line with ; in R

          # fernando: this can be simplified
          # Multipling  baserate K with  f(t) and f(0) Del Grosso et al. (2005a)
          sol_f_T <- f_T( Ts[counter_days] ) 
          sol_f_Se <- f_Se( S_e )
          sol_f <- sol_f_T * sol_f_Se
          for(x in 4:9) {xi[x,x] <- sol_f}
          
          nstep <- nstep + 1;       # step counter
          cumyear <- nstep / 365;   # cumulative year
    
          for (i in 1:npool) {
            dC[i,1] <- U * B[i,1] - xi[i,i] * K[i,i] * C[i,1]; 
            for (j in 1:npool) {
              if (j != i)
                dC[i,1] <- dC[i,1] + A[i,j] * xi[j,j] * K[j,j] * C[j,1];
            }
          }
          
          
          # heterotrophic respiration
          
          RH = xi[9,9] * K[9,9] * C[9,1];
          for (j in 4:8) {
            suma <- 0;
            for (i in 4:9) {
              if (i != j)
                suma <- suma + A[i,j];
            }
            RH <- RH + (1 - suma) * xi[j,j] * K[j,j] * C[j,1];
          }
          
          
          # update pools
          # fernando: dt is used here to adjust the fluxes. 
          # I think is is clearer to adjust the rates above so dt can be removed here.
          for (i in 1:npool) {
            C[i,1] <- C[i,1] + dC[i,1] # * dt;
          }
        
          vegc <- C[1,1] + C[2,1] + C[3,1];    # vegetation: leaf + root + wood
          litc <- C[4,1] + C[5,1];             # litter: metabolic + structural
          cwdc <- C[6,1];                      # coarse woody debris
          somc <- C[7,1] + C[8,1] + C[9,1];    # soil organic matter: fast + slow + passive
          totc <- vegc + litc + cwdc + somc;   # total carbon
          
          
          # balance check
          
          dCtot <- 0;
          for (i in 1:npool) {
           dCtot = dCtot + dC[i,1];
          }
        
          #err <- U - (RH + dCtot);
          #if ( abs(err) > 1e-12 ) {
          #  #print ( 'err <- #15.5f\n', err );
          #  stop ( 'BALANCE CHECK ERROR' );
          #}
        }
    }
    
    # save annual output for graphing
    
    x1[year] <- cumyear;
    y1[year] <- vegc;
    y2[year] <- litc;
    y3[year] <- cwdc;
    y4[year] <- somc;
    y5[year] <- totc;
    
   # fprintf('year = #8.1f\n',cumyear)
    
   
}


# ----------------------
# write final pools
# ---------------------

# ----------------------
# equilibrium pool sizes
# ----------------------

# fernando: there was an error here
C_eq <- -U * solve((A %*% xi_m %*% K), B)

# This is overwriting your model results!
vegc <- C_eq[1,1] + C_eq[2,1] + C_eq[3,1]
litc <- C_eq[4,1] + C_eq[5,1];
cwdc <- C_eq[6,1]
somc <- C_eq[7,1] + C_eq[8,1] + C_eq[9,1]
totc <- vegc + litc + cwdc + somc;


# ----------------------
# graph data
# ----------------------

# par(mfrow =c(3,2))
# 
# plot( x1, y1, xlab = "Year", ylab = "Carbon (g m-?)", main = "vegetation", na.rm=TRUE )
# plot( x1, y2, xlab = "Year", ylab = "Carbon (g m-?)", main = "litter" )
# plot( x1, y3, xlab = "Year", ylab = "Carbon (g m-?)", main = "cwd" )
# plot( x1, y4, xlab = "Year", ylab = "Carbon (g m-?)", main = "som" )
# plot( x1, y5, xlab = "Year", ylab = "Carbon (g m-?)", main = "total" )


# ----------------------------------
# Write formated output to text file
# ----------------------------------

#data = [x1; y1; y2; y3; y4; y5];
#fileID = fopen('data.txt','w');
#fprintf(fileID,'#8s #15s #15s #15s #15s #15s\n','year','vegc','litc','cwdc','somc','totc');
#fprintf(fileID,'#8.1f #15.5f #15.5f #15.5f #15.5f #15.5f\n',data);
#fclose(fileID);

