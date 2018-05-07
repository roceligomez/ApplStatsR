#ejercicio 2

#a)
x = 1:5 
running_mean = function(k) { 1 / k * sum(x[1:k]) } 
running_mean(2)

#b)

rm(list = ls()) 
x = 1:5 
running_mean = function(k) 
  { 1 / k * sum(x[1:k]) } 
running_mean(2)

#C)

rm(list = ls()) 

x = 1:5
running_mean = function(k) 
  if (k>=1){
    { 1 / k * sum(x[1:k]) } 
  }else{
    stop ("recuerda que k tiene que ser mayor o igual a 1")
  }

running_mean(2)

#ejercicio 3

  rm(list = ls()) 
  
  n.root <- function(x,n) {
    root <- numeric(length = n)
    for (i in 1:n) {
      groot <- function (x,i) {x^(1/i)}
      root[[i]] <- groot(x,n)
    }
    }
      

n.root(9,6)





