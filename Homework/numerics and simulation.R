#ejercicio 1

eddie <- function(x) {
  (1-cos(x))/x^2
}

#a)
curve(eddie, from=-15,to=15)
#b)
curve(eddie,from=-4*10^(-8), to= 4*10^(-8))

#ejercicio 2

# no correr
f <- 1
while (abs(p-pi)>10^(-5)){
n <- 10^f
x <- runif(n,-1,1)
y <- runif(n,-1,1)
z <- c(length=n)
for (i in 1:n){
  if (x[[i]]^2+y[[i]]^2<=1) {
    z[[i]] <-1
  }else{
    z[[i]] <-0
  } 
}
R <- sum(z)/n
p <- 4*R
f <- f+1
}

#ejercicio 3






