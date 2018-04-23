#EJERCICIOS#
rm(list=ls())

#1_a#
set.seed(1234)
list1<-list(
  x = c(35:5,6:35),
  y = as.factor(c(rep(letters[1:5],each = 12),letters[5])),
  mat = matrix(rexp(16,rate = 5),4,4),
  list2 = list(t = 35,
            d=data.frame(gender = rep(c("male","female"),each = 3,times = 2),age = c(23,48,37,37,19,54,21,20,41,26,35,32)))
  )
list1


#1_b#

  #a# 
    list1$x[c(4,7)]
  #b#
    list1$mat[3,4]
  #c#
    list1$list2$d$gender[1:6]
  #d#
    list1$list2$d[c(list1$list2$d$gender == "female"),2]

#1_c#
  #a#
    list1$y = factor(list1$y,levels(list1$y))[c(3,4,2,1,5)]
    list1$y
  #b#
    list1$mat = list1$mat[-1,]
    list1$mat = list1$mat[,-3]
  #c#
    t = 40 #threshold#
    list1$list2$d$anoge2 <- ifelse(list1$list2$d$age>t,"old","young")
  #d#
    ub = 50 #upperbound#
    lb = 21 #lowerbound#
    list1$list2$d$d1 <- ifelse(list1$list2$d$age <= ub & list1$list2$d$age >= lb,1,0)
    list1$list2$d <- list1$list2$d[c(list1$list2$d$d1 == 1),]
    list1$list2$d <- list1$list2$d[,1:3]

#Excercise 2#
  #a#
    X <- matrix(rnorm(2500*12,0,sqrt(3)),2500,12)
  #b#
    Y <- matrix(0,2500,12)
    # for( i in 1:12){
    #   Y[,i]<-(X[,i]-mean(X[,i]))/sd(X[,i])
    # }
    Y <- scale(X)
  #c#
#    d2 <- 0
# 	for(i in 1:2500){
#     d2[i] <- ifelse(sum(X[i,]>6),1,0)
#     }
#    X <- X[c(d2 == 1),]
#     rm(d2)
    X <- X[rowSums(X > 0) >= 6,]
  #d#
  	result_1 <- 0
  	for(i in 1 : dim(list1$mat)[1]) {
  	  result_1[i] <- ifelse( list1$mat[i,3] <= max(list1$mat[i,1],list1$mat[i,2]) & list1$mat[i,3] >= min(list1$mat[i,1],list1$mat[i,2]),TRUE,FALSE) 
  	}
	  result_1 <- as.logical(result_1)
	#e#
  	for(i in 1 : dim(X)[1]) {
  	  X[i,which(X[i,] == max(X[i,]))] <- X[i,which(X[i,] == max(X[i,]))]
  	}
	#f#
  	X_2 <- matrix(rnorm(2500*12,0,sqrt(3)),2500,12)
	
	for(i in 1:2500){
	  if( X_2[i,1] >= 0) {if(i == 2500 ){} else {
	    X_2[i+1,1] <- ifelse(i == 2500,X_2[i,1], X_2[i+1,1] + runif(1,-1,1))
	  }} else { if(i == 2500 ){} else {
	    X_2[i+1,1] <- ifelse(i == 2500,X_2[i,1],X_2[i+1,1] + runif(1,-2,2))
	  }}
	}
	#g#
  	set.seed(2015)
  	w <- runif(10)
  	x <- runif(10)
  	y <- runif(10)
  	z <- runif(10)
  	dat <- data.frame(a = w, b = x, c = y, d = z)
  	dat$e <- 0
  	
  	 for(i in 1:10) {
  	   dat$e[i] <- as.logical(
  	      (max(dat$a[i],dat$b[i]) < min(dat$c[i],dat$d[i]))
  	      )
  	 }; dat$e <- as.logical(dat$e)

#Exercise 3# Graphs are saved.
  #a#
  	X_3 <- matrix(rnorm(2500*12,0,sqrt(3)),2500,12)
  	pdf("SennyQ_G(a).pdf")
    	hist_data <- hist(X_3[,1], plot = FALSE)
    	  hist(X_3[,1], prob=TRUE, ylim = c(0,max(hist_data$density)+0.01))
        lines(density(X_3[,1]),col = "red", lty = "dashed")
    dev.off()
	#b#
    pdf("SennyQ_G(b).pdf")
      qqplot(X_3[,1],X_3[,2], pch = ".", col = "brown")
      abline(a = 0, b = 1, lwd = 2)
    dev.off()
  #c#
    pdf("SennyQ_G(c).pdf")
      par(mfcol=c(1,3))
      boxplot(X_3[1,],xlab = "BLUE")
      boxplot(X_3[4,],xlab = "GREEN")
      boxplot(X_3[7,],xlab = "YELLOW")
    dev.off()