#EXERCISE 1

#1a
set.seed(4)
x = c(35:5,6:35)
y = c(rep(letters[1:5],1,each=12),"e")
mat = matrix(rexp(16,5), ncol = 4, nrow = 4)
t = 35
d = data.frame("gender" = c(rep(c("Male", "Female"), each = 3, times = 2)), "age" = c(23,48,37,37,19,54,21,20,41,26,35,32))
list2 = list("t"=t,"d"=d)                         
list1 = list("x"=x, "y"=y, "mat"=mat, "list2"=list2)

#1b
list1$x[c(4,7)]
list1$mat[3,4]
list1$list2$d[1:6,1]
d[d$gender=="Female",]

#1c
factor(y, levels = c("c", "d", "b", "a", "e"))
mat[-1,-3]
d$age2 = ifelse(d$age>=t, "old", "young")
d[!((d$age>=50) | (d$age<21)),]


#2a
x = matrix(rnorm(12*2500, 0, sqrt(3)), ncol = 12, nrow = 2500)

#2b
y = matrix(ncol = 12, nrow = 2500)
for (i in 1:12){
  y[, i]=(x[, i]-mean(x[,i]))/sd(x[,i])
}

#2c
x[rowSums(x>0)>=6,]

#2d
Verify = vector(mode = "logical", length = 4L)
for (i in 1:4){
  if((max(mat[1,i],mat[2,i])>mat[3,i])&(min(mat[1,i], mat[2,i])<mat[3,i])){
   Verify[i] = TRUE
  }else{
    Verify[i] = FALSE
  }
}


#2e
for (i in 1:2500){
  x[i, which.min(x[i,])] = max(x[i,])
}

#2f
for (i in 1:2499){
  if (x[i,1]>=0){
    x[i+1,1]=x[i+1,1]+runif(1,min = -1, max = 1)
  }else{
    x[i+1,1]=x[i+1,1]+runif(1,min = -2,max = 2)
  }
}

#2g
set.seed(2015)
w = runif(10)
x = runif(10)
y = runif(10)
z = runif(10)
dat = data.frame(a = w, b = x, c = y, d = z)
final = vector(mode = "logical", length = 10L)
for (i in 1:10){
  final[i] = (
    (min(dat$c[i],dat$d[i])<max(dat$a[i],dat$b[i]))&(max(dat$a[i],dat$b[i])<max(dat$c[i],dat$d[i]))|
    (min(dat$c[i],dat$d[i])<min(dat$a[i],dat$b[i]))&(min(dat$a[i],dat$b[i])<max(dat$c[i],dat$d[i]))|
    (min(dat$a[i],dat$b[i])<max(dat$c[i],dat$d[i]))&(max(dat$c[i],dat$d[i])<max(dat$a[i],dat$b[i]))|
    (min(dat$a[i],dat$b[i])<min(dat$c[i],dat$d[i]))&(min(dat$c[i],dat$d[i])<max(dat$a[i],dat$b[i]))
  )
}


#3a
x = matrix(rnorm(12*2500, 0, sqrt(3)), ncol = 12, nrow = 2500)
hist(x[,1], prob = TRUE)
lines(density(x[,1]), col = "red", ylim = "dashed")

#3b
qqplot(x[,1], x[,2])
abline(a=0, b=1, col="green")
dev.off()

#3c
boxplot.matrix(x[,c(1,4,7)], use.cols = TRUE)
#or
par(mfrow = c(1,3))
boxplot(x[,1], xlab = "Blue", col = "Blue")
boxplot(x[,4], xlab = "Red", col = "Red")
boxplot(x[,7], xlab = "Yellow", col = "Yellow")
dev.off()


#EXERCISE 2

#1a
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/school_math.raw",destfile = "School_math.raw")
sm = read.table(file = "School_math.raw", header = T)
summary(sm)
head(sm, 8L)
tail(sm, 9L)

#important to restore default settings
defaultpar = par(no.readonly = T)
#1b
par(mfcol = c(2,2))
plot(ses ~ mathach, data = sm)
sm.lm = lm(ses ~ mathach, data = sm)
abline(sm.lm)
hist(sm$mathach, prob = T, main = "Mathematics Achievement", xlab = "MATHACH")
hist(sm$ses, prob = T, main = "Socio-economic Status")
lines(density(sm$ses), col = "red")
#FALLE
plot(sm[sm$Sector=="Catholic",])

#1c
par(defaultpar)

#2
mat = matrix(1:16, ncol = 4, nrow = 4)
layout(mat, widths = rep.int(1, ncol(mat)), heights = rep.int(1, nrow(mat)), respect = FALSE)
m = matrix(c(1,2,3,4),2,2)
m
layout(m, widths = c(2,3))
x = rnorm(100)
boxplot(x)
hist(x)
layout.show(n=5)









#EXERCISE 3

#1a
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt", destfile = "knee.txt")

#1b
knee = read.table("knee.txt", header = T, colClasses = "character")

#1c
install.packages("plyr")
require(plyr)
rename(knee, c("th" = "treatment", "gen" = "sex", "pain" = "agony"))

#1d
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt", destfile = "knee2.txt")
knee2 = read.table("knee2.txt", header = T, na.strings = "missing")

#1e
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee3.txt", destfile = "knee3.txt")
knee3 = read.table("knee3.txt", header = T, sep = ";")

#1f
knee2$age2 = ifelse(knee2$age>40, "old", "young")
save(knee2, file = "knee2.Rdata")
save(knee2, file = "knee2.raw", sep = ":")
save(knee2, file = "knee2.dat")
save(knee2, file = "knee2.csv")


#Question 4

#4a
my_factorial = function(n,k){
  if ((n-round(n)==0)&(k-round(k)==0)&n>0&k>0&(n-k>=0)){
    factorial(n)/factorial(k)*factorial(n-k)
  }else{
    cat("error\n n and k must be positive intergers, and n must be greater equal than k")
  } 
}

my_factorial(2,3)
my_factorial(3.5,1)


#4b
rm(list = ls())

running_mean = function(k,n){
  if ((k-round(k)==0)&(n-round(n)==0)&n>0&k>0){
    x = c(1:n)
    1/k * sum(x[1:k])
  }else{
    cat("Error\n k and n must be positive intergers")
  }
}
running_mean(2,4)
running_mean(1.5,2)
running_mean(3,-1)


#4c
n.root = function(n,x){
  x^(1/n)
}

n.root(2,4)
n.root(2,7)
n.root(3,7)


y = c(1:10)
for (i in 1:10)
  y[i]=n.root(i,500)






