# Process input data

Input <- read.csv("FLX_DE-Hai.csv")
Input <- subset(Input, select = -c(USTAR, PPFD_IN, PPFD_OUT, SW_IN_POT, WD))
Input[Input == -9999] <- NA

max <- nrow(Input)
for(i in 1:max) {
  if(i ==1) {A <- NA; j <- 1; m <- 0}
  B <- sum(Input[i,3:ncol(Input)])
  if(is.na(A) & !is.na(B)) {
    print(paste("Start no NA at ", i))
    j <- i
  }
  if(!is.na(A) & is.na(B)) {
    k <- i-j
    print(paste("Stop no NA at ", i-1, " - Length:", k))
    if(k > m) {m <- k; l <- j}
  }
  if(!is.na(A) & !is.na(B) & i == max) {
    k <- i-j+1
    print(paste("Stop no NA at ", i, " - Length:", k))
    if(k > m) {m <- k; l <- j}
  }
  A <- B
}

Input_nogap <- Input[l:(l+m-1),]
write_csv(Input_nogap, "FLX_DE-Hai-nogap.csv")

