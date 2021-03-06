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

# 2. ADVANCED GRAPHICS
rm(list=ls())
#EJERCICIO 1: FROM BASIC PLOTS TO COMPLEX GRAPHICS
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/school_math.raw", destfile = "school_math.raw")
d=read.table("school_math.raw",header=TRUE)

#PARTE A.
summary(d)
head(d)

#PARTE B.

par(mfrow=c(2,2), col.lab="red", pch=15, lty="dashed")  

plot(d$mathach ~ d$ses, xlab="Achivement", ylab="Socio-economic status")
abline(lm(d$mathach ~ d$ses),d) #relacion lineal

hist(d$mathach, main="Achivement",xlab="Achivement")

hist(d$ses,main="Socio-economic status", xlab="Socio-economic status", probability = TRUE) #scaled
lines(density(d$ses))

boxplot(d$mathach ~ d$school)

#PARTE C.
dev.off()  #?

#EJERCICIO 2: THE LAYOUT FUNCTION
m=matrix(c(1, 3, 2, 4), byrow = TRUE,nrow=2,ncol=2)
layout(m)
plot(d$mathach ~ d$ses, xlab="Achivement", ylab="Socio-economic status")
boxplot(d$mathach ~ d$ses)
boxplot(d$mat)
boxplot(d$ses)

layout.show()

# 3. DATA MANAGEMENT 

#EJERCICIO 1: SPREADSHEET-LIKE DATA   
rm(list=ls())

#PARTE A.
getwd()
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt",destfile="knee.txt")
read.table("knee.txt")
#?

#PARTE B.
read.table("knee.txt",colClasses = c("factor",NA ,"factor","factor"))

#PARTE C.
read.table("knee.txt",colClasses = c("factor",NA ,"factor","factor"),col.names = c("treatment","age","sex","agony"))

#PARTE D.
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt",destfile="knee2.txt")
knee2=data.frame(read.table("knee2.txt"))
#nop, esta escrita la palabra nomas

#PARTE E. 
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee3.txt",destfile="knee3.txt")
read.table("knee3.txt", sep=";")
#aqui si; y como estaban separados por ";" cambio el default que son espacios para read.table

#PARTE F.
new=rep(T,length(knee2$age))
for(i in 1:length(knee2$age)) {
  new[i]=ifelse(knee2$age[i]>40,"OLD","YOUNG")
}
knee2=data.frame(knee2,new)

save(knee2,file="knee2g.Rdata")
save(knee2,file="knee2g.csv")  
write.table(knee2, file = "knee2.raw", quote = FALSE, sep = ":")    
write.table(knee2, file = "knee2.dat", col.names = FALSE, row.names = FALSE)

#EJERCICIO 2: HANDLING DATE OBJECTS AND ORDERING DATA IN R
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/movies.csv",destfile="movies.csv")
movies=read.csv("movies.csv")

#PARTE A. 
class(movies$end)  #factor
class(movies$release)  #factor

movies$release=as.Date(movies$release,format="%d.%m.%Y") #date
movies$end=as.Date(movies$end,format="%d.%m.%Y") #date

#PARTE B.
tot_dias=difftime(movies$end,movies$release, units=c("days"))
tot_sem=round(difftime(movies$end,movies$release, units=c("weeks")),digits=0)
movies=data.frame(movies,tot_dias,tot_sem)

#PARTE C.
year=format(movies$release,"%Y")
movies=data.frame(movies,year)

#PARTE D.
ranking=movies[order(movies$tot_sem,decreasing = TRUE),]

#EJERCICIO 3: CONVERTING OBJECTS AND *RData-OBJECTS 
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/data.RData?raw=true",destfile="data.RData")
load("data.RData")
ls(knee)  
ls(list1)

new_knee=data.matrix(knee)
new_vector=c(unlist(list1))  

#EJERCICIO 4: FOREIGN DATA STRUCTURES
rm(list=ls())

#PARTE A.
install.packages("foreign")
library(foreign)
require(foreign)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/blutdruck.sav?raw=true",destfile="blutdruck.sav")
blud=read.spss("blutdruck.sav")

#PARTE B.
install.packages("sas7bdat")
library(sas7bdat)
require(sas7bdat)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/s05_01.sas7bdat?raw=true",destfile="sas.sas7bdat")
read.sas7bdat("sas.sas7bdat")

#PARTE C.
read.dta("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/golf.dta?raw=true")

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

