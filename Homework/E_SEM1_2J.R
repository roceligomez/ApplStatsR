#ÍTEM 1.R BASIS
  #EXERCISE 1 (DATA STRUCTURES)
  set.seed(100)
    #PARTE A
    list1 = list(x = c(35:0, 1:35),
                 y = factor(rep(1:5,c(12,12,12,12,13)),
                             levels = c(1,2,3,4,5),
                             labels = c("a","b","c","d","e")),
                 mat = matrix(round(rexp(16,5),2), nrow = 4,ncol=4))
  
    list2 = list(t = c(35),
                 d = (data.frame(gender=rep(rep(c("male","female"),c(3,3)),2),
               age=c(23,48,37,37,19,54,21,20,41,26,35,32))))
          
    #PARTE B
    list1[[1]][c(4,7)] #a
    list1[[3]][3,4] #b
    list2[[2]][1:6,1] #c
    list2$d$age[list2$d$gender=="female"]
    list2[[2]]
    
    separado <- split(list2$d$age,list2$d$gender) #adicional
    separado
    
    #Es muy importante distinguir claramente entre Lst[[1]] y Lst[1]. `[[. . .]]' es el 
    #operador utilizado para seleccionar un solo elemento, mientras que `[. . .]' es un operador
    #general de indexado. Esto es, Lst[[1]] es el primer objeto de la lista Lst, y si es una lista
    #con nombres, el nombre no esta incluido. Por su parte, Lst[1], es una sublista de la lista
    #Lst consistente en la primera componente. Si la lista tiene nombre, este se transfiere a la
    #sublista.
    
    #PARTE C
    levels(list1$y)<-c("c","d","b","a","e")
    list1$mat[-1,-3] #borra fila 1 y columna 3
    list2$d <- cbind(list2$d, age2=ifelse(list2$d$age>list2$t,"old","young"))
    list2$d[list2$d$age<=50 & list2$d$age>21,] #solución por complemento

  #EXERCISE 2  
    #PARTE A
    X <- matrix(data=round(rnorm(2500*12,mean=0,sd=sqrt(3)),1), nrow = 2500,ncol=12)
    
    #PARTE B
    for(i in 1:ncol(X)){
      y_ij = matrix(data=(X-mean(X[,i]))/sd(X[,i]),nrow=12,ncol=1)
    }
      y_ij
    
    #PARTE C
    x2 <- ifelse(X>0,1,0)
    x3 <- rowSums(x2)>=6 #logical vector
    output = X[x3,]
    
    #PARTE D
    list1$mat
  
    for (i in 1:nrow(list1$mat)) {
    print((min(list1$mat[i,1:2])<=list1$mat[i,3])&
        (list1$mat[i,3]<=max(list1$mat[i,1:2])))
      }
    
    #PARTE E #####PREGUNTAR#####
    for (i in 1:nrow(X)){
      a=max(X[i,])
      b=min(X[i,])
      output=X[i,which(X[1,]==a)] <- X[i,which(X[1,]==b)]
    }
      output
    
    #PARTE F
    for (i in 1:2500){ 
      if ((X[i,1]>0) & i != 2500){
        X[i+1,1] = X[i+1,1]+runif(1,min=-1,max=1)
      }else if ((X[i,1]<0) & i!=2500){
        X[i+1,1] = X[i+1,1]+runif(1,min=-2,max=2)
        }
    }
    X
      
    #PARTE G ######PREGUNTAR####
    set.seed(2015)
    w <- runif(10)
    x <- runif(10)
    y <- runif(10)
    z <- runif(10)
    dat <- data.frame(a=w,b=x,c=y,d=z)
    
    for (i in 1:nrow(dat)){
      dat$e[i] = (min(dat[i,1],dat[i,2])<dat[i,3] &
              dat[i,3]<max(dat[i,1],dat[i,2])) ||
        (min(dat[i,1],dat[i,2])<dat[i,4] &  
           dat[i,4]<max(dat[i,1],dat[i,2])) 
    }
    dat
    
    
  #EXERCISE 3. BASIC GRAPHICAL TOOLS
    #PARTE A
    hist(X[,1], prob=T,main="Histograma 1")
    lines(density(X[,1]),col="red",lty="dashed")
    dev.off()
    
    #PARTE B
    qqplot(X[,1],X[,2],col="blue",pch=1)
    abline(a=0,b=1,col="red")
  
    #PARTE C
    boxplot(X[,1],X[,4],X[,7],names=c("blue","green","yellow"),main="graph 3")

#ÍTEM 2. ADVANCED GRAPHICS
  download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/school_math.raw",destfile = "school_math.raw")
  data_math = read.table("school_math.raw",header = T)
  
  #Exercise 1
    #PARTE A
    summary(data_math)
    head(data_math)
  
    #PARTE B
    dev.off()
    x11()
    opar = par(mfrow=c(1,1))
    par(mfrow=c(2,2))
    
    plot(mathach ~ ses, data=data_math)
    modelo.lm <- lm(mathach ~ ses, data=data_math)
    abline(modelo.lm)
    
    hist(data_math$mathach)
    
    hist(data_math$ses,prob=T)
    lines(density(data_math$ses),col="red")
  
    schools = c(unique(data_math$school))
    #incompleto
  
  #EXERCISE 2
    dev.off()
    x11()
    m <- matrix(c(1,2,1,3),2,2)
    layout(m)
    
#ÍTEM 3. DATA MANAGEMENT
  #Exercise 1 
    #PARTE A
    download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt",destfile = "knee.txt")
    w <- read.table("knee.txt", header=TRUE)
    #si se escribe false arroja un error porque la línea 1 del encabezado no tiene 5 entradas

    #PARTE B
    summary(w)
    head(w)
    str(w)
    names(w)
    
    #PARTE C
    w = read.table("knee.txt", header=T, colClasses = c(NA,"factor","numeric","factor","factor"),
                   col.names = c("treatment","age","sex","agony"))
    
    #PARTE D
    download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt",destfile = "knee2.txt")
    q = read.table("knee2.txt",header = TRUE, na.strings = "missing") 
    #el último argumento convierte los "missing" en NA
    
    #PARTE E
    download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee3.txt",destfile = "knee3.txt")
    y <- read.table("knee3.txt", header=T, sep=";")
    #el último argumento informa que los datos están separados por ";"
  
    #PARTE F
    
  #EXERCISE 2
    #PARTE 1
    download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/movies.csv",destfile = "movies.csv")
    csv_data <- read.table("movies.csv",header=T, sep=",")
    str(csv_data)
    as.Date(csv_data$release,format="%d.%m.%Y")
    as.Date(csv_data$end,format="%d.%m.%Y")
    
    #PARTE 2 ####revisar####
    days <- csv_data$end - csv_data$release
    csv_data <- cbind("days")
    
    #####FALTAN ALGUNAS PARTES####

#ÍTEM 4. FUNCTIONS
  #EXERCISE 1
    #PARTE A
    my_fun = function(a){
      print(a)
    }
    my_fun("Hola")
    
    #PARTE B
    my_factorial = function(n){
      output=prod(1:n) #producto de 1 hasta n, hay una funcion que se llama factorial
      return(output)  
    }
    
    my_factorial(4) 
    my_factorial(pi) #redondea el pi a 3
    my_factorial(-2) #arroja 0 para negativos
    
    #PARTE C
    #is.integer no debe ser usada, porque
    is.integer(4) #tipo de elemento es NUMERIC, devuelve FALSE
    is.integer(4L) #tipo de elemento es INTEGER, devuelve TRUE
    
    is.wholenumber = function(x){
      output = (x-round(x))==0
      return(output)
    }
    
    my_factorial = function(n){
      if (is.wholenumber(n)==T & n>=0){
        if (n==0){
          output = 1
        }else{
          output = prod(1:n)
        }
      }else if(is.wholenumber(n)==F){
        stop("error matemático")
      }
    else if (n<0){
        stop("error matemático")
     }
    }
    
    #PARTE D
    my_binomial = function(n,k){
      output = my_factorial(n)/(my_factorial(k)*my_factorial(n-k))
      return(output)
    }
      
    my_binomial(2,3) #sale error
    traceback()
    
  #EXERCISE 3
    #PARTE 1, 2 Y 3
    n.root <- function(n){
      output = function(x){
        return(x^(1/n))
      }
      return(output)
    }
    
    n.root(4) #solo arroja el código de objeto f(x) = x^(1/4)
    n.root(4)(3) #ahora arroja el resultado de f(x) = 3^(1/4)
    
    #PARTE 4
    n.root(1:10)(500)
      
  
  
  
  
         