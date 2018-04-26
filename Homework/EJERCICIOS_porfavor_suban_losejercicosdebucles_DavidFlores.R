###EJERCICIO NUMERO4####
my_factorial=function(n){
  output=prod(1:n)
  return(output)
}
is.integer(4)
[1] FALSE
is.integer(4L)
[1] TRUE

#LA DEFINICION DE R NOS DICE QUE 4L ES UN INTEGER

is.wholenumber=function(x){
  output=(x-round(x)==0)
  return(output)
}


my_factorial=function(n){
  if(is.wholenumber(n)==T & n>=0){
    if(n==0){
      output=1
    }else{output=prod(1:n)
    }
  } else if(is.wholenumber(n)==F){
    stop("BIK")
  }
  else if(n<0){
    stop("DOBLE BIKA")
  }
}


my_binomial=function(n,k){
  output=my_factorial(n)/(my_factorial(k)*my_factorial(n-k))
  return(output)
}

my_binomial(2,3)
traceback()



#EXERCISE 3

n.root<-function(n){
  output=function(x) {
    return(x^(1/n))
  }
  return(output)
}    



> n.root(2)(3)
> y=n.root(1:10)(500)
> y
