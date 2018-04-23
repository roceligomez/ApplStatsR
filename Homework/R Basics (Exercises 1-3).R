set.seed(1)
#Exercise 1 (Data structures and subsetting)
#Part (a)
x=c(35:5,6:35)
y=c(rep(letters[1:5],each=12),"e")
mat=matrix(data=rexp(16,rate=5),nrow=4,ncol=4)
t=35
gender=c(rep(c(rep(c("male","female"),each=3)),times=2))
age=c(23,48,37,37,19,54,21,20,41,26,35,32)
d=data.frame(gender,age)
list2=list(t,d)
list1=list(x,y,mat,list2)

#Part (b)
x[c(4,7)]
mat[3,4]
d[1:6,1]
d[d$gender=="female",2]

#Part (c)
factor(y,levels=c("c","d","b","a","e"))
mat<-mat[-1,-3]
d$age2<-ifelse(d$age>=t,"old","young")
d<-d[!(d$age>=50 & d$age<21),]


#Exercise 2 (Loops and data manipulation)
#Part (a)
X=matrix(data=rnorm(30000,mean=0,sd=sqrt(3)),nrow=2500,ncol=12)

#Part (b)
X=matrix(data=rnorm(30000,mean=0,sd=sqrt(3)),nrow=2500,ncol=12)
Xmean=colSums(X,na.rm=FALSE)
Xsd=apply(X,2,sd)
Y=matrix(data=((X-Xmean)/Xsd),nrow=2500,ncol=12)

#Part (c)
X=matrix(data=rnorm(30000,mean=0,sd=sqrt(3)),nrow=2500,ncol=12)
X<-X[rowSums(X>0)>=6,]

#Part (d)
maximum=c(max(mat[1,1],mat[2,1],mat[3,1]),max(mat[1,2],mat[2,2],mat[3,2]))
minimum=c(min(mat[1,1],mat[2,1],mat[3,1]),min(mat[1,2],mat[2,2],mat[3,2]))
ma=max(maximum)
mi=min(minimum)
range=matrix(nrow=1,ncol=3)
for(i in 1:3){
  if(ma>mat[i,3]&mi<mat[i,3]){
    range[1,i]=TRUE
  }
  else
  {
    range[1,i]=FALSE
  }
}

#Part (e)
X=matrix(data=rnorm(30000,mean=0,sd=sqrt(3)),nrow=2500,ncol=12)
for(i in 1:250){
  X[i,match(min(X[i,]),X[i,])]=max(X[i,])
}


#Part (f)
X=matrix(data=rnorm(30000,mean=0,sd=sqrt(3)),nrow=2500,ncol=12)
for(i in 1:2499){
  if(X[i,1]>0){
    X[i+1,1]=X[i+1,1]+runif(1,min=-1,max=1)
  }
  else{
    X[i+1,1]=X[i+1,1]+runif(1,min=-2,max=2)
  }
}

#Part (g)
set.seed(2015)
w <- runif(10)
x <- runif(10)
y <- runif(10)
z <- runif(10)
dat <- data.frame(a = w, b = x, c= y, d = z)
v=matrix(nrow=10,ncol=1)
for(i in 1:10){
  v[i]=as.logical(
    (min(dat$c[i],dat$d[i])<max(dat$a[i],dat$b[i]))&(max(dat$a[i],dat$b[i])<max(dat$c[i],dat$d[i]))|
    (min(dat$c[i],dat$d[i])<min(dat$a[i],dat$b[i]))&(min(dat$a[i],dat$b[i])<max(dat$c[i],dat$d[i]))|
    (min(dat$a[i],dat$b[i])<max(dat$c[i],dat$d[i]))&(max(dat$c[i],dat$d[i])<max(dat$a[i],dat$b[i]))|
    (min(dat$a[i],dat$b[i])<min(dat$c[i],dat$d[i]))&(min(dat$c[i],dat$d[i])<max(dat$a[i],dat$b[i]))
  )
}
dat <- data.frame(a = w, b = x, c= y, d = z, e = v)

#Exercise 3 (Basic graphical tools)
set.seed(1)

#Histogram
X=matrix(data=rnorm(30000,mean=0,sd=sqrt(3)),nrow=2500,ncol=12)
hist(X[,1],prob=TRUE)
lines(density(X[1,]),col="red",ylim="dashed")
dev.off()

#Quantile-quantile plot
qqplot(X[,1],X[,2])
abline(a=0,b=1,col="red",lwd=2)
dev.off()

#Boxplots
par(mfcol=c(1,3))
boxplot(X[1,],xlab="blue",col="blue")
boxplot(X[4,],xlab="green",col="green")
boxplot(X[7,],xlab="yellow",col="yellow")
dev.off()