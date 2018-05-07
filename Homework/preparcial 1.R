
#255 pasos 
#¿cuántos discos se movieron?
#x=ax(-1)+beta

rm(list=ls())

#torre de hanoi
#a lo mate 3
pasos <- function (x) { 
  log2(x+1)}
pasos (255)

#a lo info eco

discos = c(1:10)

pupperino <- function(y) { 
  if (y==1) { output=1
  }else{
  output=2*pupperino(y-1)+1}
  return=output
}

pasos <-numeric(length = 10)
for (i in 1:10) {
pasos[[i]] <- pupperino(discos[[i]])
}

#matrices

figurita <- matrix(c(1,0,0,0,1,0,1,0,1),ncol=3,nrow=3)
image(figurita)

#pizzas



pedazos <- function(c){
  if (c==1) {output=2}
  else{
  output=pedazos(c-1)+c
  }
  return(output)
}

pedazos (1)





