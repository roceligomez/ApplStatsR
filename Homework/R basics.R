setwd("D:/info eco/R/ejercicios")
#ejercicio 1

#a)

list1 <- list(
  x <- c(35:5,6:35),
  y <- c(rep(letters[1:4],each=12),rep(letters[5],each=13)),
  z <- matrix(rexp(16,rate=5),nrow = 4,ncol = 4),
  set.seed(z),
  list2 <- list(
    t <- c(35),
    d <- data.frame(
       "gender" = rep(c("male", "female") ,each=3, times=2),
        "age" = c(23,48,37, 37, 19, 54, 21, 20, 41, 26, 35, 32))))

#b)

# a. 
    x[[4]]
    x[[7]]
#b.
    z[[3,4]]
#c.
    d[1:6,1]
#d.
    d[d$gender=="female",2]
    
#c)
#a.
    factor(y,levels = c("c","d","b","a","e"))
#b.
    z <- z[-1,-3]
#c. 
   d$age2= ifelse(d$age>t,"old","young")
#d.
   d<-d[!(d$age>=50 & d$age<21),]
   
# ejercicio 2 (loops and data manipulation)

# a)

   X <- matrix(rnorm(2500*12,sd=sqrt(3)),nrow=2500,ncol=12)
   
#b)
   
   Y <- matrix(nrow=nrow(X),ncol=ncol(X))
   
   for (j in 1:ncol(Y)) {
   for (i in 1:nrow(Y)) {
     Y[[i,j]]= (X[[i,j]]-mean(X[,j])/sd(X[,j]))
   }
   }
   


#c) 

    for (i in 1:nrow(X)) {
      q <- numeric(length = 12)
        for (g in 1:ncol(X) ) {
         q[g] <- ifelse( X[[i,g]]>0 ,1,0) }
      if (sum(q)<6) {
        X <- X[-i,]}
    }
   
#d)
    t <- c(range(z[,3])<=range(c(z[,2],z[,1]))) 
#e)
    
    for (o in 1:nrow(X)){
      for (k in 1:ncol(X)) {
    X[[o,k]] <- ifelse(X[[o,k]]==max(X[o,]),min(X[o,]),X[[o,k]])
    } }

#f)
    for (i in 1:nrow(X)) {
      if (X[[i,1]]>0 & i!=2500){
      X[[i+1,1]]=X[[i+1,1]]+runif(1,min=-1,max=1)
  }else{
    if (X[[i,1]]<0 & i!=2500) {
    X[[i+1,1]]=X[[i+1,1]] + runif(1,min=-2,max=2)}}}
    
#g) 
    set.seed(2015) 
    w <- runif(10) 
    x <- runif(10) 
    y <- runif(10) 
    z <- runif(10) 
    dat <- data.frame(a = w, b = x, c = y, d = z)

l <- c(min(y,z)>min(w,x) & min(y,z)<max(w,x) | max(y,z)<max(w,x) &  max(y,z)>min(w,x) |min(y,z)>min(w,x) & max(y,z)<max(w,x)
      |min(w,x)>min(y,z) & min(w,x)<max(y,z)|max(w,x)<max(y,z) &  max(w,x)>min(y,z)|min(w,x)>min(y,z) & max(w,x)<max(y,z))    

#3 (basic graphical tools)

hist(X[,1],prob=TRUE,col="gray")
lines(density(X[,1]),col="red",lty=2)

qqplot(X[,1],X[,2],col="red",pch=17)
abline(a=0,b=1,col="blue")

opar <- par (mfrow=c(1,1))
boxplot(X[1,],X[4,],X[7,],names= c("blue","green","yelow"))
par (opar)