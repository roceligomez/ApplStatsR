#Exercise 1 (From basic plots to complex graphics)

download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/school_math.raw", destfile="school_math.raw")
data = read.table("school_math.raw",header = TRUE)

#Pregunta a
summary(data)     #muestra datos estad??sticos de cada vector del data frame
head(data)        #muestra los primeros 6 elementos del data frame

#Pregunta b
par(mfrow=c(2,2), col.lab = "red", pch = 15, lty = "dashed")
plot(mathach ~ ses, data)
abline(lm(mathach ~ ses, data))

hist(data$mathach)

hist(data$ses, prob=TRUE)
lines(density(data$ses))

boxplot(mathach ~ school, data)

dev.off()

#Exercise 2 (The layout function)

m <- matrix(c(1,0,2,3), 2, 2, byrow = TRUE)
layout(m, widths = c(3,1), heights = c(1,3))
layout.show(3)
par(mar = c(0,3,0,0))
boxplot(data$mathach, horizontal = T, axes = F)
par(mar = c(3,1,1,0))
plot(mathach ~ ses, data)
par(mar = c(3,1,1,0))
boxplot(data$ses, horizontal= F, axes = F)

dev.off()

#Exercise 3 (Lattice plots and grouped data)

  #Pregunta a
fml <- lmList(mathach ~ ses | school, data)
summary(fml)
par(mfrow=c(1,2))
plot(c(fml$`1433`$coefficients,fml$`3377`$coefficients,fml$`4253`$coefficients,fml$`7919`$coefficients),type="n",xlab="B0", ylab = "B1")
points(fml$`1433`$coefficients, col="Red")
points(fml$`3377`$coefficients, col="Green")
points(fml$`4253`$coefficients, col="Blue")
points(fml$`7919`$coefficients, col="Orange")
plot(mathach ~ ses, data, type="n")
points(y=data$mathach[1:35], x=data$ses[1:35], col="red")
points(y=data$mathach[36:80], x=data$ses[36:80], col="green")
points(y=data$mathach[81:138], x=data$ses[81:138], col="blue")
points(y=data$mathach[139:175], x=data$ses[139:175], col="orange")
abline(lm(mathach[1:35] ~ ses[1:35], data), col="red")
abline(lm(mathach[36:80] ~ ses[36:80], data), col="green")
abline(lm(mathach[81:138] ~ ses[81:138], data), col="blue")
abline(lm(mathach[139:175] ~ ses[139:175], data), col="orange")

  #Pregunta b
fml1 <- lmList(mathach ~ ses | Sex, data)
plot(mathach ~ ses, data, type="n")
f <- which(data$Sex=="Female")
m <- which(data$Sex=="Male")
points(y=data$mathach[f], x=data$ses[f], col="red", pch=15)
points(y=data$mathach[m], x=data$ses[m], col="green", pch=17)
legend("topright", pch = c(15,17), legend = c("Female","Male"), col = c("Red","Green"))

  #Pregunta c
plot(data)
plot(fml)
bwplot(c(fml$`1433`$residuals,fml$`3377`$residuals,fml$`4253`$residuals,fml$`7919`$residuals), xlab=" ",ylab=" ")
lme1 <- lme(mathach ~ ses, random = ~ 1 | school, data)
compareFits(coef(fml), coef(lme1))

