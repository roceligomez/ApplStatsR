#Question 4

#4 a
my_factorial = function(n,k){
  if ((n-round(n)==0)&(k-round(k)==0)&n>0&k>0&(n-k>=0)){
    factorial(n)/factorial(k)*factorial(n-k)
  }else{
    cat("error\n n and k must be positive intergers, and n must be greater equal than k")
  } 
}

my_factorial(2,3)
my_factorial(3.5,1)


#4 b
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


#4 c
n.root = function(n,x){
  x^(1/n)
}

n.root(2,4)
n.root(2,7)
n.root(3,7)


y = c(1:10)
for (i in 1:10)
y[i]=n.root(i,500)







