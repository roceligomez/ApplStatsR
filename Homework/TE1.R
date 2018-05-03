#E1
#parte a
set.seed(100)

list1=list(x = c(35:6, 6:35),
           y = factor(c(rep(c(LETTERS[1:5]),each = 12),"E"), levels=c(LETTERS[1:5])),
           #y=c(rep(LETTERS[1:5],each=12),"E"),
           mat = matrix(rexp(16, rate = 5),nrow = 4, ncol = 4),
           
           list2=list(t=25,
                      d=data.frame(gender = c(rep(c("Male", "Female"), 2, each=3)),
                                   age = c(23, 48, 37, 37, 19, 54, 21, 20, 41, 26, 35, 32)))
                                   )

#parteb

list1$x[c(4, 7)]

list1$mat[3,4]

data = list1$list2$d
data[1:6,1]
data[data$gender=="Female", 2]

#Parte c

levels(list1$y) <- c('C', 'D', 'B', 'A', 'E')

list1$mat <- list1$mat[-1,-3]

threshold = list1$list2$t
list1$list2$d$age2 <- factor(ifelse(list1$list2$d$age > threshold, "old", "young"))

list1$list2$d <- list1$list2$d[21 < list1$list2$d$age & list1$list2$d$age < 50,, drop = FALSE]


#Excercise 2

#a
X <- matrix(rnorm(2500*12,sd = sqrt(3)),nrow=2500,ncol=12)

#b
Y <- X
for( i in 1:ncol(X)){
  Y[,i] <- (Y[,i]-colMeans(X)[i])/sd(X[,i])
}

Yb <- t((t(X)-apply(X,2,mean))/apply(X,2,sd))


#c

J <- X>0
J <- J*1

for( i in 1:nrow(X)){
  if (rowSums(J)[i]<6) {
    X <- X[-i,]
  }
}

X <- X[rowSums(X>0)>=6,]

#d
VecD = c(0,0,0)
for(i in 1:3){
  maxi=max(list1$mat[i,-3])
  mini=min(list1$mat[i,-3])
  VecD[i] = list1$mat[i,3]<=maxi & list1$mat[i,3]>=mini
}
VecD = VecD==1

#e

for (i in 1:dim(X)[1]){
  a=max(X[i,])
  b=min(X[i,])
  X[i,which(X[1,]==a)] <- X[i,which(X[1,]==b)]
}

#f
for (j in 1:2500){
  if (X[j,1]>0 & j!=2500){
    X[j+1,1] = X[j+1,1] + runif(1,min=-1)
  } else {
    if(X[j,1]<0 & j!=2500){
    X[j+1,1] = X[j+1,1] + runif(1,min=-2,max=2)
    }
  }
}

#g

set.seed(2015)
w <- runif(10)
x <- runif(10)
y <- runif(10)
z <- runif(10)
dat <- data.frame(a=w,b=x,c=y,d=z)


for (i in 1:10){
dat$e[i] = (min(dat[i,-1-2])>=min(dat[i,-3-4]) & min(dat[i,-1-2]<=max(dat[i,-3-4])) |
        max(dat[i,-1-2])>=min(dat[i,-3-4]) & max(dat[i,-1-2]<=max(dat[i,-3-4]))  )
}


#Excercise 3

hist(X[,1],prob=T)
lines(density(X[,1]),col="red", lty="dashed")
dev.off()

qqplot(X[,1],X[,2],col="blue",pch=4)
abline(a=0,b=1,col="red")

boxplot(X[,1],X[,4],X[,7],names=c("blue","green","yellow"))
