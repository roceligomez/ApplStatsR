#E1
#a

f <- function(x){
  (1 - cos(x))/x^2
}

curve(f, from = -15, to = 15)
curve(f, from = -4*10^(-8), to = 4*10^(-8))

#E2
Pi = 4
expo = 1
while (abs((pi-Pi)/pi) >= 10^-5){
rep = 10^expo
x <- runif(rep, min = -1, max = 1)
y <- runif(rep, min = -1, max = 1)
mat <- matrix(c(x,y), ncol = 2)
plot(x,y)

puntos <- 0
puntos <- sum(ifelse(mat[,1]^2 + mat[,2]^2 <= 1, 1, 0))

R = puntos/rep
Pi = 4*R
expo = expo + 1
}

#E3
index = 1000
transday = 2900
bussd = 20
montrd = 22
sterror = 0.00045

for (i in 1:transday*bussd*montrd){
transac = round(rnorm(1), digits = 4) + sterror
index = index + trunc(transac, digits = 3) 
}



