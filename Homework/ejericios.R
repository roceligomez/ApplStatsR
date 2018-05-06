#EJERCICIO 1A
set.seed(123)
list1=list(x=c(35:5,6:35),y=c(rep(letters[1:5],each=12),"e"),mat=matrix(rexp(16,5),4,4),list2=list(t=c(35),d=data.frame(gender=rep(c(rep("male",3),rep("female",3)),times=2),age=c(23,48,37,37,19,54,21,20,41,26,35,32))))
#EJERCICIO 1B
list1$x[4]
list1$x[7]
list1$mat[3,4]
list1$list2$d$gender[1:6]
list1$list2$d[list1$list2$d$gender=="female",2]
#EJERCICIO 1C
factor(list1$y,levels=c("a","b","c","d","e"),labels=c("c","d","b","a","e"),ordered=TRUE)
list1$mat[-1,-3]
list1$list2$d$age2=ifelse(list1$list2$d$age>list1$list2$t,"old","young")
list1$list2$d=list1$list2$d[list1$list2$d$age<50 & list1$list2$d$age>=21,] #esta ultima coma te dice:incluye todas las columnas

#EJERCICIO 2A
X=matrix(nrow=2500,ncol=12,rnorm(2500*12,0,sqrt(3)))
#EJERCICIO 2B 
y=matrix(0,2500,12)
for (i in 1:12){
  y[,i]=((X[,i]-mean(X[,i]))/sd(X[,i]))
}
#EJERCICIO 2C
z=matrix(2500,12)
z=X[]>0
X=X[rowSums(z,)>=6,]
 
#EJERCICIO 2D
a=min(list1$mat[,1:2])
b=max(list1$mat[,1:2])
A=c(list1$mat[,3]<a & list1$mat[,3]>b)

#EJERICIO 2E
for (i in 1:dim(X)[1]){
  X[i,which(X[i,]==max(X[i,]))]=X[i,which(X[i,]==min(X[i,]))]
}

#EJERCICIO 2F
X=matrix(nrow=2500,ncol=12,rnorm(2500*12,0,sqrt(3)))
for (k in 1:2499){
  if (X[k,1]>0){
    X[k+1,1]=X[k+1,1]+runif(1,min=-1,max=1)
  } else if (X[k,1]<0){
    X[k+1,1]=X[k+1,1]+runif(1,min=-2,max=2)
  }
}

#EJERCICIO 2G
set.seed(2015)
w=runif(10)
x=runif(10)
y=runif(10)
z=runif(10)
dat=data.frame(a=w,b=x,c=y,d=z)
f=c(dat[,2]-dat[,1])
e=c(dat[,4]-dat[,3])
g=c(f<0)
h=c(e<0)
dat=data.frame(a=x,b=x,x=y,d=z,e=g,f=h)

#EJERCICIO 3
X=matrix(nrow=2500,ncol=12,rnorm(2500*12,0,sqrt(3)))
graf=par(lwd=2)
hist(X[,1],main="grafico1")
lines(density(X[,1]),col="red")
par(graf)

gra2=par(col.main="blue",bg="gray",pch=8)
qqplot(X[,1],X[,2],main="graf2")
abline(a=0,b=1,col="yellow")
par(gra2)

graf3=boxplot(X[,1],X[,4],X[,7],col=c("blue","green","yellow"))

