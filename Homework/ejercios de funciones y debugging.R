# Function, Debugging & Condition Handling exercises 

#1
my_factorial=function(x){
  
 prod = prod(1:x)
 
 print(prod)
}

my_factorial(5)

is.integer(7L) # la razon por la cual la función is.integer no nos sirve es porque nuestra definicion de entero es diferente a la R

my_int= function(x){
 
   if(x==round(x,0)){
    return(TRUE)} 
  else {
    return(FALSE)}
}

my_factorial=function(x){
  
  if (my_int(x)==FALSE){
    message("error, el número no es un entero")
  }
else{ if(my_int(x)==T & x>0){
  prod = prod(1:x)
  print(prod)} 
  else{ if(x==0){
    print(1)}
    else{ stop("no puede ser menor que cero papu")}
  }
  
  }
  
}

my_factorial(9.4)

#creamos funcion my_binomial

my_binomial= function(x,k){
  if(k>x){
    print(0)
  }
  else{
  result=(my_factorial(x))/(my_factorial(k)*my_factorial(x-k))
  return(result)}
}
my_binomial(2,3)
traceback()

#2
#a
x = 1:5
running_mean = function(k) {
  1 / k * sum(x[1:k])
}
running_mean(2)
#b

rm(list = ls()) #esta es la razon por la cual no corre en la opción b , ya que se borra todos los elementos del workspace
running_mean = function(k) {
  x = 1:5
  1 / k * sum(x[1:k])
}

running_mean(2) 
# si colocamos x=1:5 dentro de la función esta correra independientemente del workspace

#3

n.root= function(n){
  output= function(x){
  root= x^(1/n)
  return(root)
  }
return(output)
  
}

n.root(2) 
n.root(3)

y=n.root(1:10)(500)
y
