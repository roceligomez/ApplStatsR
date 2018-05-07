#ejercicio 1
setwd("D:/Info eco/R/ejercicios")
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/school_math.raw",destfile ="School_math.raw")
data=read.table("school_math.raw",header=TRUE)
#a 
  summary(data)
  head(data)
#b
  
  dev.off()
  opar <- par (mfrow=c(2,2))
  scatter.smooth(x=data$mathach,y=data$ses,main="achievement vs. socio-economic status")
  hist(data$mathach)
  hist(data$ses,breaks="fd")
  boxplot(data[data$school==1433,2],data[data$school==3377,2],data[data$school==4253,2],data[data$school==7919,2])
  par(opar)
  
  
  dev.off()
  opar <-par(mfrow=c(2,2),col.axis="red",lty=2,pch=15)
  scatter.smooth(x=data$mathach,y=data$ses,main="achievement vs. socio-economic status")
  hist(data$mathach)
  hist(data$ses,breaks="fd")
  boxplot(data[data$school==1433,2],data[data$school==3377,2],data[data$school==4253,2],data[data$school==7919,2])
  par(opar)

 
  
#ejercicio 2
  
  dev.off ()
  g <- matrix(c(1,2,1,3),2,2)
  layout (g)
  scatter.smooth(x=data$mathach,y=data$ses,main="achievement vs. socio-economic status")
  boxplot(data$mathach)
  boxplot(data$ses)
  
  par(opar)

#ejercicio 3
  
# a) 
   #install.packages("nlme")
  require(nlme)
  dev.off()
  
  eddie = lmList(mathach ~ ses|school,data)
  coef(eddie)
  
  par (mfrow=c(1,2))
  plot(as.factor(row.names(coef(eddie))),coef(eddie)[,1],ylab=expression(beta[0]))
  plot(as.factor(row.names(coef(eddie))),coef(eddie)[,2],ylab=expression(beta[1]))
  par(opar)
  
#b)
 
  plot(data$mathach,data$Ses,data=data[data$sex=="Female",2],ylab="ses",xlab="mathach",pch=15,bg="red",col="red")
  plot(data$mathach,data$Ses,data=data[data$sex=="Male",2],ylab="ses",xlab="mathach",pch=17,bg="green",col="green")
  
#C)
  #install.packages("lattice")
  require(lattice)
  require(nlme)
  
  
  

  