#pregunta1
#a
list1=list(x=c(35:5,6:35),
           y=c(rep(letters[1:5],each=12),"e"),
           mat=matrix(rexp(16,5),nrow=4, ncol=4),
           list2=list(t=35,
                      d=data.frame(gender=c("male", "male", "male", "female", "female", "female", "male", "male", "male", "female", "female", "female"),
                                   age=c(23, 48, 37, 37, 19, 54, 21, 20, 41, 26, 35, 32))))
#b
  #a
list1$x[c(4,7)]
  #b
list1$mat[3,4]
  #c
data=list1$list2$d
data[c(1:6),1]
  #d
data[data$gender=="female",2]
#Subsetting lists with ???[???-operator returns always a list, while [[, and $ pull out elements of the list:
#c
  #a
factor(list1$y,levels=c("c","d","b","a","e"))
  #b
list1$mat<-list1$mat[-1,-3]
  #c
age2<-c(1:12)
data$age2<-ifelse(data$age>list1$list2$t,"old","young")
  #d
data<-data[50>data$age & data$age>=21,]
#pregunta2
#a
x<-matrix(data=rnorm(30000,mean=0,sd=sqrt(3)),nrow=2500, ncol=12)
#b
xm<-mean(x)
xsd<-sd(x,na.rm=TRUE)
x1<-((x-xm)/xsd)
#c
x<-x[rowSums(x>0)>=6,]
#d
matr<-list1$mat
maximo<-max(matr[,-3])
minimo<-min(matr[,-3])
range<-matrix(nrow=1,ncol=3)
for(i in 1:3){
  range[1,i]<-ifelse(matr[i,3]<maximo&matr[i,3]>minimo,TRUE,FALSE)
}
#e
for(i in 1:dim(x)[1]) {
  x[i,which(x[i,]==max(x[i,]))]<-x[i,which(x[i,]==min(x[i,]))]
}
#f
x=matrix(data=rnorm(30000,mean=0,sd=sqrt(3)), nrow=2500, ncol=12)
for(i in 1:2499){
  ifelse(x[i,1]>0,x[i+1,1]<-(x[i+1,1]+runif(1,min=-1,max=1)),x[i+1,1]<-(x[i+1,1]+runif(1, min=-2,max=2)))
}
#g
set.seed(2015)
w<-runif(10)
x<-runif(10)
y<-runif(10)
z<-runif(10)
dat<-data.frame(a=w, b=x, c=y, d=z)
dat
for(i in 1:10) {
  dat$e[i] <- as.logical(
    (max(dat$a[i],dat$b[i]) < min(dat$c[i],dat$d[i]))|(max(dat$c[i],dat$d[i]) < min(dat$a[i],dat$b[i]))
  )
  ifelse(dat$e[i]==FALSE,dat$e[i]<-TRUE,dat$e[i]<-FALSE)
}

#pregunta3
x<-matrix(data=rnorm(30000,mean=0,sd=sqrt(3)), nrow=2500, ncol=12)
#a
pdf("atl.graph1.pdf")
hist(x[,1],prob=TRUE)
lines(density(x[,1]), col="red", lty=2)
dev.off()
#b
pdf("atl.graph2.pdf")
qqplot(x[,1],x[,2])
abline(a=0,b=1,col="blue",lty=3)
dev.off()
#c
pdf("atl.graph3.pdf")
par(mfcol=c(1,3))
boxplot(x[1,],xlab="blue",col="blue")
boxplot(x[4,],xlab="green",col="green")
boxplot(x[7,],xlab="yellow",col="yellow")
dev.off()
