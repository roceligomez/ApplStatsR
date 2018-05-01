#download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/school_math.raw", destfile = "school_math.raw")
data = read.table("school_math.raw", header = T)

#E1

#a
summary(data)
head(data)

#b
dev.off()
x11()   # para ver el gráfico más grande 
opar = par(mfrow=c(1,1))
par(mfrow = c(2,2))

plot(mathach ~ ses, data=data)
modelo.lm <- lm(mathach ~ ses, data=data)
abline(modelo.lm)
hist(data$mathach)
hist(data$mathach, prob = T)
lines(density(data$mathach),col="red")
scul = c(unique(data$school))
boxplot(data$mathach[data$school==scul[1]],data$mathach[data$school==scul[2]],data$mathach[data$school==scul[3]],data$mathach[data$school==scul[4]])

par(pch = 15, lty = "dashed") # Falta

#c
par(opar)

#E2
dev.off()
x11()
m <- matrix(c(1,2,1,3),2,2)
layout(m)

plot(mathach ~ ses, data=data)
modelo.lm <- lm(mathach ~ ses, data=data)
abline(modelo.lm)
boxplot(data$mathach)
boxplot(data$ses)
par(opar)

#E3
#a
dev.off()
#install.packages("nlme")
require(nlme)

models = lmList(mathach ~ ses | school, data)
coef(models)

#B1 = c(0,0,0,0)
#B2 = c(0,0,0,0)
#for (i in 1:4){
#B1[i] = c(models[[i]]$coefficients)[1]
#B2[i] = c(models[[i]]$coefficients)[2]
#}

#par(mfrow = c(1,2))
#plot(scul,B1, ylab = expression(paste(beta[1])))
#plot(scul,B2, ylab = expression(paste(beta[2])))

par(par_defaults)
par(mfrow = c(1, 2))
plot(as.factor(row.names(coef(models))), coef(models)[, 1], xlab = "School", ylab = expression(beta[0]))
plot(as.factor(row.names(coef(models))), coef(models)[, 2], xlab = "School", ylab = expression(beta[1]))

par(opar)

#b
plot(mathach ~ ses, data = data[data$Sex == "Male",], col = "green", pch = 2)
plot(mathach ~ ses, data = data[data$Sex == "Female",], col = "red", pch = 0)

#c
dev.off()
x11()
require(nlme)
require(lattice)
data.new <- groupedData(mathach ~ ses | school, data)
plot(data.new)
plot(data)

bwplot(residuals(models))
models2 = lme(mathach ~ ses, data = data.new, random = ~1)
compareFits(coef(models), coef(models2))
comparePred(models, models2, primary = 1)

#Falta intervalos de confianza