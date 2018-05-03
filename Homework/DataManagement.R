# 3. DATA MANAGEMENT 

#EJERCICIO 1: SPREADSHEET-LIKE DATA   
rm(list=ls())

#PARTE A.
getwd()
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt",destfile="knee.txt")
read.table("knee.txt")
#?

#PARTE B.
read.table("knee.txt",colClasses = c("factor",NA ,"factor","factor"))

#PARTE C.
read.table("knee.txt",colClasses = c("factor",NA ,"factor","factor"),col.names = c("treatment","age","sex","agony"))

#PARTE D.
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt",destfile="knee2.txt")
knee2=data.frame(read.table("knee2.txt"))
#nop, esta escrita la palabra nomas

#PARTE E. 
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee3.txt",destfile="knee3.txt")
read.table("knee3.txt", sep=";")
#aqui si; y como estaban separados por ";" cambio el default que son espacios para read.table

#PARTE F.
new=rep(T,length(knee2$age))
for(i in 1:length(knee2$age)) {
  new[i]=ifelse(knee2$age[i]>40,"OLD","YOUNG")
}
knee2=data.frame(knee2,new)

save(knee2,file="knee2g.Rdata")
save(knee2,file="knee2g.csv")   #write.table(knee2, file = "knee2.csv")  
write.table(knee2, file = "knee2.raw", quote = FALSE, sep = ":")    
write.table(knee2, file = "knee2.dat", col.names = FALSE, row.names = FALSE)
#guarda defrente el write.table

#EJERCICIO 2: HANDLING DATE OBJECTS AND ORDERING DATA IN R
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/movies.csv",destfile="movies.csv")
movies=read.csv("movies.csv")

#PARTE A. 
class(movies$end)  #factor
class(movies$release)  #factor

movies$release=as.Date(movies$release,format="%d.%m.%Y") #date
movies$end=as.Date(movies$end,format="%d.%m.%Y") #date

#PARTE B.
tot_dias=difftime(movies$end,movies$release, units=c("days"))
tot_sem=round(difftime(movies$end,movies$release, units=c("weeks")),digits=0)
movies=data.frame(movies,tot_dias,tot_sem)

#PARTE C.
year=format(movies$release,"%Y")
movies=data.frame(movies,year)

#PARTE D.
ranking=movies[order(movies$tot_sem,decreasing = TRUE),]

#EJERCICIO 3: CONVERTING OBJECTS AND *RData-OBJECTS 
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/data.RData?raw=true",destfile="data.RData")
load("data.RData")
ls(knee)  
ls(list1)

new_knee=data.matrix(knee)
new_vector=c(unlist(list1))  #que el vector no se guarda como parte de la data?

#EJERCICIO 4: FOREIGN DATA STRUCTURES
rm(list=ls())

#PARTE A.
install.packages("foreign")
library(foreign)
require(foreign)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/blutdruck.sav?raw=true",destfile="blutdruck.sav")
blud=read.spss("blutdruck.sav")

#PARTE B.
install.packages("sas7bdat")
library(sas7bdat)
require(sas7bdat)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/s05_01.sas7bdat?raw=true",destfile="sas.sas7bdat")
read.sas7bdat("sas.sas7bdat")

#PARTE C.
read.dta("https://github.com/franciscorosales-marticorena/ApplStatsR/blob/master/Exercises/golf.dta?raw=true")
