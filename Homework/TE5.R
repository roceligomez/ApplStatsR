#E1
#a
#rows at least 6 out of 12 are >0
X <- matrix(rnorm(2500*12,sd = sqrt(3)),nrow=2500,ncol=12)

#i

cont = 0 
for( i in 1:nrow(X)){
a = i - cont
    if (rowSums(X>0)[a] < 6 & i <= nrow(X)) {
    X <- X[(-a),]
    cont = cont + 1
  }
}



X <- X[rowSums(X>0)>=6,]

#ii

for( i in 1:nrow(X)){
  ifelse(rowSums(X>0)[i] < 6 & i >= nrow(X), X <- X[-i], X)
}

#iv
system.time(X <- X[rowSums(X>0)>=6,])

#E2
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/tree/master/Exercises/linear.RData", destfile = "linear.RData")



  
