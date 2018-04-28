#R Basics

#Excercise 1 (Data structures and subsetting)

#Part a
x <- c(35:5,6:35)
y <- c(rep(letters[1:5],each=12),"e")
set.seed(1)
mat <- matrix(rexp(16,5), nrow = 4, ncol = 4)

t <- 35
gender <- rep(c("Male","Female"), each=3, times=2)
age <- c("23","48","37","37","19","54","21","20","41","26","35","32")
d <- data.frame(gender,age)
list2 <- list(t,d)

list1 <- list(x,y,mat,list2)

#Part b
  #(a)
x[c(4,7)]
  #(b)
mat[3,4]
  #(c)
d[1:6,]
  #(d)
d[d$gender == "female",2]

#Part c
  #(a)
factor(y,levels = c("c","d","b","a","e"))
  #(b)
mat[-1,-3]
  #(c)
d$age2 = ifelse(d$age>t, "old", "young")


#Excercise 2 (Loops and data manipulation)

#Part a
  #(a)
x <- matrix(rnorm(30000, mean=0, sd=sqrt(3)),nrow=2500,ncol=12)
  #(b)
y <- (x - mean(x)) / sd(x)
  #(c)
x <- x[rowSums(x>0)>6,]
  #(d)
matr <- mat[-1,-3]
maximo <- max(matr[,3])
minimo <- min(matr[,3])
maxrange <- max(matr)
minrange <- min(matr)
output <- matrix(nrow=1,ncol = 3)
for (val in 1:3){
  output[1,val]<- ifelse(matr[val,3] <= maximo & matr[val,3] >= minimo, TRUE, FALSE)
}
  #(e)
for (i in 1:dim(x)[1]) {
  xmin <- min(x[i, ])
  xmax <- max(x[i, ])
  x[i,which(x[i,]==xmax)] <- x[i,which(x[i,]==xmin)]
}
  #(f)
for (j in 1:12){
  i=1
  if (x[i,j]>0){
    x[i+1,j]<-x[i+1,j]+runif(1, min=-1, max=1)
  }
  if (x[i,j]<0){
    x[i+1,j] <- x[i+1,j] + runif(1, min = -2, max = 2)
  }
}
  #(g)
set.seed(2015)
w <- runif(10)
x <- runif(10)
y <- runif(10)
z <- runif(10)
dat <- data.frame(a = w, b = x, c = y, d = z)
dat

dat$overlapse = c(1:10)

for (i in 1:12){
if (dat$a[i]<dat$b[i]){
  min_1 <- dat$a[i]
  max_1 <- dat$b[i]
} else {
    min_1 <- dat$b[i]
    max_1 <- dat$a[i]
  }
if (dat$c[i]<dat$d[i]){
  min_2 <- dat$c[i]
  max_2 <- dat$d[i]
} else {
  min_2 <- dat$d[i]
  max_2 <- dat$c[i]
}

if (min_1 < min_2 & min_2 < max_1 | min_1 < max_2 & max_2 < max_1 | min_2 < min_1 & min_1 < max_2 | min_2 < max_1 & max_1 < max_2){
  dat$overlapse[i] = "TRUE"
} else {
    dat$overlapse[i] = "FALSE"
  }

}

#Excercise 3 (Basic graphical tools)

#Part a
x <- matrix(rnorm(30000, mean=0, sd=sqrt(3)),nrow=2500,ncol=12)

  #(a)
hist(x[ ,1], probability = TRUE)
lines(density(x[1, ]),col="red", lty="dashed")
dev.off()

  #(b)
qqplot(x[ ,1],x[ ,2], pch=25, cex= 0.7)
abline(a = 0, b = 1, col="green")

  #(c)
boxplot(x[1, ], x[4, ], x[7, ],names=c("Blue","Green","Yellow"), col=c("blue","green","yellow") )

