# clean and select working directory
rm(list=ls())
setwd("/Users/francisco/Desktop")

# download price files
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Others/pricedata.csv",destfile = "pricedata.csv")
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Others/priceanalysis.R",destfile = "priceanalysis.R")

# functions
sizeofseq = function(n){ 
  object_size(seq_len(n)) 
}

seq_len(3L)
seq_len(3.5)
seq_len(-3)
seq_len("sometext")

sizeofseq = function(n){ 
  if(is.integer(n)&(n>0)){
    object_size(seq_len(n))
  }else{
    cat("n is not a positive integer\n")
  }
}

f <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}


f(1, 2, 3) # match by position. also do.call(f,list(1, 2, 3))
f(2, 3, abcdef = 1) # match by complete name  
f(2, 3, a = 1) # match by partial name and then by position
f(1, 3, b = 1) # partial name insufficient 
f(1, 3, bcde = 1) # partial name insufficient 

myplot <- function(x, y, mycol = "red"){
  if(missing(y)){
    y = x
    x = 1:length(y)}
  plot(x, y, col = mycol)
}

myplot(1:20)
myplot(1:20, rnorm(20), mycol="darkgreen")

match.arg("Male",c("Male","Female"))
match.arg("M",c("Male","Female"))
match.arg("F",c("Male","Female"))
match.arg(NULL,c("Male","Female"))

tsex <- function(sex = c("Male", "Female")) {
  match.arg(sex)
}

tsex("F") 
tsex() 
tsex("W")

myfun <- function(x, y){
  if(x < 0){
    return(NaN)
  }else{
    return( y * log(x))
  }
}

myfun(-1) 
myfun(2,3) 
myfun(2)

myplot <- function(x, y, myarg, ...){
  plot(x, y, ...)
}

myfun <- function(x, fun2.args = NULL, fun3.args = NULL, ...){
  # calculations
  fun1(x, ...)
  do.call(fun2, fun2.args)
  do.call("fun3", fun3.args)
  # further calculations
}

apply(trees, MARGIN = 2, hist)

hist_2by2 <- function(data, args, ...) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow = c(2, 2))
  # do histrogram plots for all variables in the data frame
  invisible(apply(data, MARGIN = 2, hist, ...))
}
x11();hist_2by2(trees,col="blue")

f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) {"a" + d}

f(10)

traceback()

message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}

f <- function(x) {
  if (!is.numeric(x))
    stop("supplied x is not numeric.")
  s <- sum(x)
  message("sum = ", s)
}

f <- function(x, silent = TRUE) {
  s <- try(sum(x), silent = silent)
  if (inherits(s, "try-error")) {
    warning("x of wrong type, returning NA.")
    return(NA) }
  s 
}

f.alt <- function(x, silent = TRUE) {
  s <- try(sum(x), silent = silent)
  if (class(s)=="try-error") {
    cat("My warning:\n >>> something is wrong, returning NA.\n")
    return(NA) }
  s 
}


f <- function(x, silent = FALSE) {
    s <- tryCatch(sum(x),
          error = function(e) {
          warning("x of wrong type, sum is NA.")
          if (!silent) print(e)
          return(NA)
                  },
          finally = cat("done!\n")
          ) 
    s 
}             

f("nonsense",silent=TRUE)


