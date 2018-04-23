# preamble
rm(list=ls())

# lists the loaded data sets:
data()

mydatacsv = read.csv("rand.csv",sep=";"); class(mydatacsv)
mydatatable = read.table("rand.csv",header = TRUE,sep=";"); class(mydatatable)

attach(mydatacsv)
attach(trees)


# also, but not recommended
# install.packages("xlsx")
# library("xlsx")

#library(foreign)

g
etwd()
path = "/Users/francisco/Desktop"
pathX = "/Users/francisco"
setwd(path)
dir()
dirname(pathX)
file.path(path)

download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Others/malo.R",destfile="malo.R")

mydatacsv2 = read.csv("rand2.csv",sep=";",na.strings = "-"); 

glmout = capture.output(summary(glm(case ~ spontaneous+induced,data = infert, family = binomial())))

write.csv(mydatacsv2,"mydatacsv2.csv",row.names = FALSE)

save.image(file = "mydata.RData")
load("mydata.RData")

mymatrix = as.matrix(mydatacsv2,dimnames=NULL)
dimnames(mymatrix) = NULL
write.matrix(mymatrix,file = "mymatrix2.csv",sep=";")

library(pryr)
x = 1:10
object_size(x)
object_size(trees)

f <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}

myplot <- function(x, y, mycol = "red") {
  if(missing(y)) {
    y <- x
    x <- 1:length(y)
  }
  plot(x, y, col = mycol)
}

myplot(1:20)
myplot(1:20, rnorm(20), mycol="darkgreen")

tsex <- function(sex = c("Male", "Female")) {
  match.arg(sex)
}
# if you want only some options to be admissible

myfun <- function(x, y){
  if(x < 0){
    return(NaN)
  }else{
    return( y * log(x))
  }
}

myfun <- function(x, y){
  if(x < 0){
    return(NaN)
  }else{
    #return(y * log(x))
    y * log(x)+1
  }
}


hist_2by2 <- function(data, args, ...) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow = c(2, 2))
  # do histrogram plots for all variables in the data frame
  invisible(apply(data, MARGIN = 2, hist, ...))
}

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d


f <- function(x, silent = FALSE) {
  s <- tryCatch(sum(x),
                error = function(e) {
                  warning("x of wrong type, sum is NA.")
                  if (!silent) print(e)
                  return(NA)
                },
                finally = cat("buh bye!\n")
  )
  s 
}

f("nonsense")
