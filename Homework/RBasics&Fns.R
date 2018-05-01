#TAREA

# 1. R BASICS

#EJERCICIO 1: DATA STRUCTURES AND SUBSETTING

#PARTE A.
x=c(35:5,6:35)
as.vector(x)
y=c(rep(letters[1:5],times=1, each=12),"e")

set.seed(40)  
mat=matrix(rexp(16,5),ncol=4,nrow=4)  
print(mat)  

#componentes list2...
t=35
gender=c(rep(rep(c("male","female"),each=3),times=2))
as.factor(gender)
age=c(23,48,37,37,19,54,21,20,41,26,35,32)
as.vector(age)
d=data.frame(gender,age)
print(d)

list2=list(t,d)

#todo en la primera list1...
list1=list(x,y,mat,list2)

#PARTE B.

#(a) llamar al cuarto y septimo num de x:
list1[[1]][c(4,7)]

#(b) posicion (3,4) de mat:
list1[[3]][3,4] 

#(c) primeros seis elementos de la primera columna de del data frame:
list1[[4]][[2]][[1]][1:6]

#(d) edad de las female individuals
list1[[4]][[2]][[2]][4:6]
list1[[4]][[2]][[2]][10:12]

#PARTE C

#(a) redefinir niveles de y from lowest to highest:
y=c(rep(letters[5:1],times=1, each=12),"e")

#(b) eliminar primera fila y tercera colum de matriz:
#a=list1[[3]][-1,-3] 
mat=mat[-1,-3]

#(c) ...
t=35
age2=rep(T,12)
for(i in 1:12) {
  age2[i]=ifelse(age[i]>t,"OLD","YOUNG")
}
d=data.frame(gender,age,age2)
list2=list(t,d)

#(d) ...
new_age=rep(T,12)
for(i in 1:12) {
  new_age[i]=ifelse(age[i]>=50 | age[i]<21,NA,age[i])
}
d=data.frame(gender,new_age,age2)
d=na.exclude(d)
list2=list(t,d)

#EJERCICIO 2: LOOPS AND DATA MANIPULATION
rm(list=ls())

#PARTE A.
set.seed(40)
mat=matrix(rnorm(2500*12,mean=0,sd=sqrt(3)),nrow=2500,ncol=12) 

#PARTE B.
set.seed(40)
new_mat=matrix(0,nrow=2500,ncol=12)  

for (j in 1:12){
  new_mat[,j]=(mat[,j]-mean(mat[,j]))/sd(mat[,j])  
}

#otra forma: la facil...
#new_mat=scale(mat)  

#PARTE C.
#identificar las filas
ind=rep(TRUE,2500)
for (i in 1:2500){  
  ind[i]=ifelse(sum(mat[i,]>0)>=6,T,F)
}
#extraer las q si cumplen
mat_gd=mat[ind,]  

#PARTE D. 
rm(list=ls())
#mat del Ejercicio1
set.seed(40)  
mat=matrix(rexp(16,5),ncol=4,nrow=4) 

prueb=rep(T,4)  
for(i in 1:4) {
  prueb[i]=ifelse(mat[i,1]<mat[i,3] & mat[i,3]<mat[i,2],TRUE,FALSE)
}

#PARTE E.
rm(list=ls())
#mat de este ejercicio
set.seed(40)
mat=matrix(rnorm(2500*12,mean=0,sd=sqrt(3)),nrow=2500,ncol=12)

maxi=vector(mode="numeric",length = 2500)
for(i in 1:2500) {
  maxi[i]=max(mat[i,])
}

mini=vector(mode="numeric",length = 2500)
for(i in 1:2500) {
  mini[i]=min(mat[i,])
}

if (mat[1,1]==maxi[1]){
  mat[1,1]=mini[1]
}

#PARTE F.
for(i in 1:2500) {
  if (mat[i,1]>0) {
    mat[i+1,1]+runif(1,-1,1)
  }else{
    mat[i+1,1]+runif(1,-2,2)
  }
}

#PARTE G.
rm(list=ls())
set.seed(2015)
w <- runif(10)
x <- runif(10)
y <- runif(10)
z <- runif(10)
dat <- data.frame(a = w, b = x, c = y, d = z)

che=rep(T,10)
for (i in 1:10) {
  che[i]=ifelse(max(dat$a[i],dat$b[i])<min(dat$c[i],dat$d[i]),T,F)
}

#EJERCICIO 3: BASIC GRAPHICAL TOOLS
rm(list=ls())

#PARTE A - Histograma.
set.seed(40)
mat=matrix(rnorm(2500*12,mean=0,sd=sqrt(3)),nrow=2500,ncol=12) 
hist(mat[,1], main ="Histogram of te 1st Column of X",probability=TRUE)   
lines(density(mat[1,]),col="red", ylim="dashed")
dev.off()  

#PARTE B - Quantile-Quantile Plot (qqplot)
qqplot(mat[,1],mat[,2], pch="*", col="pink")
abline(a=0,b=1,col="lightblue")
dev.off()

#PARTE C - Boxplots
par(mfcol=c(1,3))
boxplot(mat[1,],col="lightblue")
boxplot(mat[4,],col="lightgreen")
boxplot(mat[7,],col="lightyellow")
dev.off()

# 4. FUNCTIONS, DEBUGGING & CONDITION HANDLING

#EJERCICIO 1: SIMPLE FUNCTIONS AND RESTRICTIONS
rm(list=ls())

#PARTE B.
my_factorial=function(n) {
  if(n==0){
    output=1
  } else if ((n-round(n))==0 & n>0) {
    output=prod(1:n)
  } else if ((n-round(n))==0 & n<0) {
    print("No es posible hacer la descomposicion factorial de un numero negativo")
  }
  return(output)
}


#PARTE C.
my_factorial=function(n) {
  if(n==0){
    output=1
  } else if ((n-round(n))!= 0) {
    print("ERROR: Fn solo apta para intergers, si no lo eres, abstente")
  } else if (n>0) {
    output=prod(1:n)
  } else if (n<0) {
    print("No es posible hacer la descomposicion factorial de un numero negativo")
  }
  return(output)
}

print(my_factorial(0))
print(my_factorial(-5))
print(my_factorial(4.5))

#PARTE D.
my_binomial=function(n,k){
  output=my_factorial(n)/(my_factorial(k)*(my_factorial(n-k))) #Error cuando k>n
  return(output)
}

#PARTE E.
my_binomial=function(n,k){
  if (k>n) {
    output=0
  } else {
    output=my_factorial(n)/(my_factorial(k)*(my_factorial(n-k)))
  }
  return(output)
}

#EJERCICIO 2: SCOPE AND ENVIRONMENTS
rm(list=ls())

running_mean = function(k) {
  1 / k * sum(x[1:k])
}

#PARTE A: fresh
#PARTE B: por el remove dsp de definir x no lo va a encontrar

#PARTE C.
running_mean = function(k, x = 1:5) {
  1 / k * sum(x[1:k])
}

#PARTE D: ?

#EJERCICIO 3: CLOSURES AND ENVIRONMENTS   
rm(list=ls())

n.root=function(n){
  ad=function(x){     #inner
    output=x^(1/n)
    return(output)
  }
  return(ad)          #outer
}

print(n.root(1:10)(500))

#las otras...
n.root_square=function(x){     
  output=x^(1/2)
  return(output)
}

n.root_cubic=function(x){     
  output=x^(1/3)
  return(output)
}