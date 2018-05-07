
X <- matrix(rnorm(2500*12,sd=sqrt(3)),nrow=2500,ncol=12)



#a)


c=0
for (i in 1:nrow(X)) {
  a=i-c
  if (rowSums(X>0)[a]>6 & i<=nrow(X)) {
    X <- X[-a,]
    c=c+1
  }
}

