#Exercise 1 (Functions, Debugging & Condition Handling)

  #Pregunta a
#factorial debe ser mayor igual a 0 y ser un numero entero.
#!a=1*2*...*a

  #Pregunta b
my_factorial <- function(n){
  output = prod(1:n)      #tambien se puede utilizar factorial()
  return(output)
}

my_factorial(4)
my_factorial(4.999999)
my_factorial(4.9999999)
my_factorial(-10)

  #Pregunta c
is.wholenumber=function(x){
  output=(x-round(x)==0)      #se considera todos los numeros mayores a 0. Si no, saldra FALSE.
  return(output)
}

my_factorial <- function(n){
  if (n>=0 & is.wholenumber(n)==TRUE){
    if (n == 0){
      output = 1
    } else {
    output = prod(1:n)
    }
  } else if (n<0){
      stop("No es positivo")
  } else {
      stop("No es entero")
  }
  return(output)
}

my_factorial(-9)
my_factorial(0)
my_factorial(1.5)

  #Pregunta d
my_binomial <- function(n,k){
  output=my_factorial(n)/((my_factorial(k))*(my_factorial(n-k)))
  return(output)
}
traceback()

my_binomial(5,3)

  #Pregunta e
my_binomial <- function(n,k){
  if (k>n){
    return(0)
  } else {
  output=my_factorial(n)/((my_factorial(k))*(my_factorial(n-k)))
  return(output)
  }
}
my_binomial(2,3)

#Exercise 2 (Scope and enviroments)
  #Pregunta a
x = 1:5
running_mean = function(k){
  1 / k * sum(x[1:k])
}
running_mean(2)
        #si funciona todo bien

  #Pregunta b
x = 1:5
rm(list = ls())
running_mean = function(k){
  1 / k * sum(x[1:k])
}
running_mean(2)

  #Pregunta c
#No funciona porque rm(list = ls()) elimin?? el vector x
x = 1:5
rm(list = ls())
running_mean = function(k){
  x = 1:5
  1 / k * sum(x[1:k])
}
running_mean(6)

  #Pregunta d
running_mean = function(k){
  x = 1:5
  if (k > length(x)) {
    stop("k debe ser menor a x")
  } else if (k<1) {
    stop("k debe ser mayor igual que 1")
  } else if(is.integer(k)==FALSE){
    stop("k debe ser entero")
  } else{
  1 / k * sum(x[1:k])
  }
}

#Exercise 3 (Closures and enviroments)

n.root <- function(n){
  x.func <- function(x){
  output = x^(1/n)
  return(output)}
}

n.root(3)(8)
n.root(2)(4)

y = n.root(1:10)(500)




