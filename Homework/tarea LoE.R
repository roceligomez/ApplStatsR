
#1a

list1=list(x=c(35:6,5:35),y=c(rep(letters[1:4],each=12),rep(letters[5],times=13)),mat=matrix(rexp(16,rate=5),nrow=4,ncol=4))
list1

list2=list(t=35,d=data.frame(gender=rep(c("male","female"),each=3,times=2),age=c(23,48,37,7,19,54,21,20,41,26,35,32)))
list2

#1b

list1$x[c(4,7)]
list1$mat[3,4]
list2$d$gender[1:6]
list2$d[c(list2$d$gender=="female"),2]


#1_c

list1$y=factor(list1$y,levels(list1$y))[c(3,4,2,1,5)]
list1$y

list1$mat[-3,-1]

t=40

list2$d$age2=ifelse (list2$d$age>t,"old","young")
list2$d$age2

q=50
w=21

list2$d$d1= ifelse(list2$d$age<=q & list2$d$age >= 21,1,0)

list2$d[c(list2$d$d1==1),]
list2$d <- list2$d[,1:3] #este ulitmo paso es casi innecesario pues lo unico que hace es eleiminar la columna llena de 1
list2$d

#2
#2a
n=12*2500
x= matrix(rnorm(n,mean=0,sd=3^(0.5)),2500,12)
#b
y = matrix(0,2500,12)
for(i in 1:12){
  
  y[,i]=(x[,i]-mean(x[,i])/sd(x[,i]))
}

#2c
d1=0

for (i in 1:12){
d1[i]= ifelse(sum(x[,i]>6),1,0)
}
 d1
 
 x=x[c(d1==1),]
 

 
#2d
list1$mat
b=list1$mat[1]


d2=0
for(i in 1:4){
  

  d2[i]= ifelse(b<list1$mat[i,3] & list1$mat[i,3]<list1$mat[2],1,0)
}
 d2
 

#2e
 
for(i in dim(x)[1]) {  
mn= min(x[i,])
mx=max(x[i,])
x[i,which(x[i,]==mx)]=x[i,which(x[i,]==mn)]
}

 #2F
x_2= matrix(rnorm(2500*12,0,sqrt(3)),2500,12)



for(i in 1:2500){
if(x_2[1,1]>0){
  if(i==2500){} else{
  x_2(i+1,1)=ifelse(i==2500,x_2[i,1],x_2[i+1,1]+runif(1,-1,1))
} else {
  if(i==2500){} else{
    x_2(i+1,1)=ifelse(i==2500,x_2[i,1],x_2[1+1,1]+runif(1,-2,2)
  }}}}
  

#2g

w=runif(10)
x=runif(10)
y=runif(10)
z=runif(10)
dat=data.frame(a=w,b=x,c=y,d=z)

dat
dat$e <- 0

for(i in 1:10) {
  dat$e[i] <- as.logical(
    (max(dat$a[i],dat$b[i]) < min(dat$c[i],dat$d[i]))
  )
}; dat$e <- as.logical(dat$e)
dat$e


#3
#a
x= matrix(rnorm(1,mean=0,sd=3^(0.5)),2500,12)

hist(x[,1],plot = FALSE)
lines(density(x[,1]),col="red")


pdf("hola_.pdf")
par(mfcol=c(1,3))

boxplot(x[1,],xlab="blue")
boxplot(x[4,],xlab="green")
boxplot(x[7,],xlab="yellow")  

dev.off()

