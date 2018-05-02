setwd("D:/r")
#parte para marcar:
#a
#c
#b
#d
#b
#a
#d
#a
#a
#b

pregunta1:
rm(list=ls())
n=10
nvec=1:(n-1)
xnvec=vector("numeric", length(nvec))
xnvec[1]=1
for(k in 2:(n-1)){
  xnvec[k]=2*xnvec[k-1]+1
}
out <- cbind(nvec,xnvec)
out

#pregunta2
rm(list=ls())

a=matrix(c(1,0,0,0,1,0,1,0,1),3,3)
b=matrix(c(0,0,1,0,1,0,1,0,1),3, 3)
c=matrix(c(1,0,1,0,1,0,1,0,0),3,3)
d=matrix(c(1,0,1,0,0,0,1,0,1),3,3)

menos=matrix(c(1,0,1,1,0,1,1,0,1),3,3)

#a:
rptaa=a%*%(solve(a)%*%b)
rptaa==b
det(a)
#b:
rptab=a%*%(solve(a)%*%menos)
rptab==menos
#como si se puede hallar, b es incorrecta
#c:
rptac=a%*%(solve(a)%*%d)
rptac==d


