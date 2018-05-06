#ADVANCEDGRAPHIPS
#EXERCISE 1
dat <- read.csv2( "https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/school_math.raw",sep = " ", stringsAsFactors = TRUE)
dat
#a
str(dat)
dat$mathach <- as.numeric(dat$mathach)
dat$ses <- as.numeric(dat$ses)
head(dat)
tail(dat,3)
summary(dat)
#b
#scatterplot
par(mfrow=c(2,2))
plot(dat$ses, dat$mathach)
title("Ses vs Mathach")
linearmodel <- lm(data = dat, mathach~ ses)
abline(linearmodel, lty = "dashed", lwd = 2)

hist(dat$mathach)

hist(dat$ses, freq = FALSE)
lines(density(dat$ses), lty = 3, col = "blue", lwd = 2)

boxplot(mathach ~ school, data = dat, col.axis = "red", 
        pch = 15 , lty = "dashed")
title("Boxplot")

#EXERCISE 2
matricita <- matrix(c(1,1,1,
                2,2,2,
                3,3,3),3,3)
layout(matricita, width = c(.5,1,1))
layout.show(3)
boxplot(dat$mathach,axes = FALSE)
boxplot(dat$ses, axes =FALSE)
plot(dat$mathach, dat$ses)
lineamodel <- lm(data = dat, ses ~ mathach)
abline(lineamodel, lty = "dashed", lwd = 2)




