#############
# Performance
#############
setwd("/Users/francisco/Desktop")
rm(list=ls())

# download price files
#download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Others/class4.R",destfile = "class4.R")

x <- rnorm(100000)
system.time({
  s <- 0
  for(i in 1:length(x)){s <- s + x[i]}   
})

system.time({
  s <- 0
  for(v in x){s <- s + v }
})

system.time(sum(x))

# The ‘user time’ is the CPU time 
# charged for the execution of user 
# instructions of the calling process. 
# The ‘system time’ is the CPU time 
# charged for execution by the system 
# on behalf of the calling process.
?proc.time()

x <- runif(10)
replicate(2, x^ 0.5)

x <- runif(10^5)
system.time(replicate(100, x^ 0.5))

install.packages("microbenchmark")
require(microbenchmark)
o <- microbenchmark(sqrt(x),x^ 0.5,times=100L)
o
boxplot(o)

# create Rprof.R
source('Rprof.R')
foo(reps=2,n=10)

Rprof("foo-prof.log", line.profiling = TRUE) 
foo()
Rprof(NULL)

summaryRprof("foo-prof.log", lines = 'show')

#install.packages("lineprof")
#require(lineprof)
#lineprof_example <- lineprof(foo())
#shine(lineprof_example)

Z <- rnorm(10000)
table(cut(Z, breaks = -6:6))
sum(table(cut(Z, breaks = -6:6, labels = FALSE)))
sum(hist(Z, breaks = -6:6, plot = FALSE)$counts)

sqrt2 <- function(x){
  out <- vector('numeric', length(x))
  for(i in seq_along(x)){
    out[i] <- sqrt(i)
    }
  return(out)
}

microbenchmark(
  sqrt2(1:10),
  sapply(1:10, sqrt))

xmat <- matrix(rnorm(100),10,10)
apply(xmat,2,sum)
colSums(xmat)

microbenchmark(
  apply(xmat,2,sum),
  colSums(xmat))

library(parallel)
x<-1:1e4
system.time(lapply(x, rnorm))
system.time(mclapply(x, rnorm, mc.cores = 8))

#########################
#numerics and simulations
#########################

k=20;(-2^(k-1)+1):(2^(k-1)-1)

.Machine$integer.max
.Machine$integer.max+1L


.Machine$double.xmin
.Machine$double.xmax
.Machine$double.xmax+1

.Machine$double.xmax==(.Machine$double.xmax+1)

tt<-4
ee<-2
beta<-2
for(m in 1:(beta^tt)){
  print(m*(beta^(ee-tt)))
}

2^1023 + 2^1022 + 2^1021>.Machine$double.xmax #false
2^1023 + 2^1022 + 2^1022>.Machine$double.xmax #true

2^(-1074)<.Machine$double.xmin #true
2^(-1075)<.Machine$double.xmin #true
2^(-1075)<.Machine$double.eps #true

2^(-1074) == 0 #false
2^(-1075) == 0 #true

1 / 2^(-1074) #inf
1 / 2^(-1075) #inf

2^(-52)<.Machine$double.eps
2^(-53)<.Machine$double.eps

x <- 1 + 2 ^ -52
x - 1 == 0

y <- 1 + 2 ^ -53
y - 1 == 0

x = 2^-c(10,20,30)
y1 = sin(x) - x
y2 = -x^3 / 6 * (1 - x^2 / 20)

abs(y1-y2)
abs(y1-y2)/y1

my.square = function(x) { x^2 }
optimize(f = my.square, interval = c(-2, 2))$minimum
optimize(f = my.square, interval = c(2, 3))

integrate(f = my.square, lower = -2, upper = 2)

set.seed(123)
rnorm(3)

set.seed(123)
rnorm(3)

K=1e4
n=200
set.seed(123)
x = matrix(rnorm(K*n,mean=3,sd=sqrt(2)),ncol=K)
u = matrix(rnorm(K*n,mean=0,sd=1),ncol=K)
v = abs(matrix(rnorm(K*n,mean=0,sd=1),ncol=K))
eps = u-v
beta = 2
y1 = beta*x+u
y2 = beta*x+eps
dim(y1)

# results
betas1 = vector("numeric",K)
betas2 = vector("numeric",K)
for(i in 1:K){
  betas1[i] = lm(y1[,i]~-1+x[,i])$coef
  betas2[i] = lm(y2[,i]~-1+x[,i])$coef
  }

boxplot(cbind(betas1,betas2))
