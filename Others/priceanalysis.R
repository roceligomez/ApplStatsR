# load data
data = read.table("pricedata.csv", header = TRUE)
data$Time = as.Date(data$Time, format = "%m/%d/%y")
n.subj = ncol(data) - 1
n.obs = nrow(data)

data = data[-(1:4), ]
n.subj = ncol(data) - 1
n.obs = nrow(data)

# Fill missing
for (i in 2:ncol(data)) {
    data[, i] = spline(seq_along(data[, i]), data[, i], xout = seq_along(data[, 
        i]))$y
}

# Inspection
opar <- par(mfrow = c(1, 1), bg = "white")
X11()
par(mfrow = c(4, 4))
for (j in 2:17) {
    plot(data[, 1], data[, j], type = "l", main = names(data)[j], ylab = "", 
        xlab = "")
}
par(opar)

plot(data[, 2], lwd = 2, type = "l", ylim = c(80, 250))
apply(data[, -1], 2, lines, lwd = 2)

# Basis
basis <- function(x, m, K) {
    n = length(x)
    step = (max(x) - min(x))/(K + 1)
    knots = seq(min(x) + step, max(x) - step, by = step)
    Z = outer(x, knots, "-")
    Z = (Z * (Z > 0))^m
    X = rep(1, n)
    for (i in 1:m) {
        X = cbind(X, x^i)
    }
    C = cbind(X, Z)
    list(X = X, Z = Z, C = C)
}

# Model
estimator <- function(x, y, m, K) {
    C = basis(x, m, K)$C
    return(C %*% solve(t(C) %*% C) %*% t(C) %*% y)
    
}

plot(log(data[, 2]), col = 8, type = "l")
lines(
  log(estimator(x = seq_along(data[, 2]), y = data[, 2], m = 1, K = floor(n.obs/50))),
  lwd = 4, col = "red")

hatmatrix = sapply(data[, -1], estimator, x = seq_along(data[, 2]), m = 2, K = floor(n.obs/50))

plot(hatmatrix[, 1], lwd = 2, type = "l", ylim = range(hatmatrix))
apply(hatmatrix, 2, lines, lwd = 2)
