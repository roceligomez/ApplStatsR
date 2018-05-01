#Data Management Exercises
#1
#1a
?load()
getwd()
setwd("C:/Users/USER/Desktop/esta aplicada 2018-I/R")
getwd()
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt",destfile ="Knee.txt")

read.table("Knee.txt",header=TRUE)
#si en header se pone false , no corre el codigo porque no se acomodan los valores con los nombres de las columnas

#1b y c
#la clases que existen en R son 7: character, complex, factor,integer,numeric,date y logical
read.table("Knee.txt",header=TRUE,colClasses =c("th"="factor","gen"="factor","pain"="factor"),col.names = c("treatment","age","sex","agony"))
#1d
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt",destfile ="Knee2.txt")
read.table("Knee2.txt",header = TRUE,na.strings = "missing")
#1e
read.table("Knee3.txt",header=TRUE,na.strings= ";") #no corre porque los missing values estan unidos a los valores
#1 f

Knee2=read.table("Knee2.txt",header = TRUE,na.strings ="missing")
Knee2
Knee2$nueva=ifelse(Knee2$age>40,"old","young")
Knee2

save(Knee2,file="knee2.Rdata")
write.csv(Knee2,file = "Knee2.csv")
#fomato .raw sin quote y con : de separador
write.table(Knee2,file="Knee2.raw",quote = FALSE,sep=":")
#formato .dat, sin nombres de columnas ni filas
write.table(Knee2,file="Knee2.dat",row.names = FALSE,col.names = FALSE)

#2
#2a
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/movies.csv",destfile ="movies.csv")
movies=read.csv("movies.csv",header = TRUE)
movies
class(movies$release) # con esta función hemos hallado que release es una variable factor
class(movies$end) # end tambien es factor
movies$release
estreno=as.Date(movies$release,"%d.%m.%Y")
movies$end
fin=as.Date(movies$end,format("%d.%m.%Y"))
#2b
estreno
fin
x= fin - estreno
x
movies$tiempo_total=x
movies$semanas=difftime(fin,estreno,units = "weeks")
movies
#2c
movies$year=format(estreno,"%Y")
movies
#2d
orden = movies[order(movies$semanas, decreasing = T),]
orden
#3
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/data.RData",destfile = "data.RData")
load("data.RData")
ls()

list1= c(unlist(list1,use.names = FALSE))
list1
ls()
#knee= data.matrix(Knee)

#4
require(foreign)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/blutdruck.sav",destfile = "blutdruck.sav")
read.spss("blutdruck.sav")
#se necesita tener instalado el paquete foreign

#b
install.packages("sas7bdat")
require(sas7bdat)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/s05_01.sas7bdat", destfile = "s05_01.sas7bdat")
read.sas7bdat("s05_01.sas7bdat")
#c
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/golf.dta",destfile = "golf.dta")
read.dta("golf.dta")

#this code was made Tarantino 97



