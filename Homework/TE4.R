#E1


my_factorial <- function(n){
  #factorial(n)
  if(n >= 0 & is.wholenumber(n)==TRUE){
    if(n==0){
      output=1
    }else{
      output=prod(1:n)
    }
  }else if(n<0){
    stop("n negativo")
  }else{
    stop("no entero")
  }
  
  return(output)
}

is.wholenumber = function(x){
  output=(x-round(x)==0)
  return(output)
}

my_binomial <- function(n,k){
  if (k > n){
    output = 0
  } else {
  a=my_factorial(n)
  b=my_factorial(k)
  c=my_factorial(n-k)
  output=a/b*c
  }
  return(output)
}

my_binomial(2, 3)
traceback()

#E2

running_mean = function(k,x){
  #if (x >= k){
  #v = 1:x
  if (length(x) >= k){
  output = 1/k*sum(x[1:k])
  #output = 1/k*sum(v[1:k])
  } else {
    stop(" x menor a k")
  }
  return(output)
}

x = 1:5
running_mean(2)
running_mean(5, c(1,2,3))
running_mean(5, c(1,2,3,4,5,6))

#E3
n.root <- function(n){
  for (i in 1:n){
    x.root <- function(x){
    x^(1/i)
    }
  }
}

