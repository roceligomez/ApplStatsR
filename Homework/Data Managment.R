#Exercise 1 (Spreadsheet-like data)

  #Pregunta a
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt", destfile = "knee.txt")
data = read.table("knee.txt", header = T)
read.table("knee.txt", header = F)
#Con head=F, no se puede pues est?? considerando que la linea 1 tiene 5 elementos y en realidad no los tiene, sino son las etiquetas.

  #Pregunta b
#la clases que existen en R son 7: character, complex, factor,integer,numeric,date y logical
class(data)
class(data$th)
class(data$gen)
class(data$pain)

  #Pregunta c
data = read.table("knee.txt", header = TRUE, col.names = c("treatment", "age", "sex", "agony"), colClasses = c("factor", "numeric", "factor", "factor"))

  #Pregunta d
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt", destfile = "knee2.txt")
data1 = read.table("knee2.txt", header = T, na.strings = "missing")

  #Pregunta e
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee3.txt", destfile = "knee3.txt")
data2 = read.csv("knee3.txt", header = T, sep = ";")

  #Pregunta f
data1$class <- ifelse(data1$age > 40, "old", "young")
save(data1, file = "datos1.Rdata")
write.table(data1, file = "datos1.raw", sep = ":")
write.table(data1, file = "datos1.dat", col.names = FALSE, row.names = FALSE)
write.csv(data1, file = "datos1.csv")

#Exercise 2 (Handling date objects and ordering data in R)

download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/movies.csv", destfile = "movies.csv")
movies = read.csv("movies.csv", header = TRUE)

  #Pregunta a
class(movies$release)
class(movies$end)
movies$release <- as.Date(movies$release, format = "%d.%m.%Y")
movies$end <- as.Date(movies$end, format = "%d.%m.%Y")

  #Pregunta b
dias <- movies$end - movies$release
semanas <- round(as.numeric(dias) / 7)
movies$days <- dias
movies$weeks <- semanas

  #Pregunta c
ano <- format(movies$release, "%y")
movies$year <- ano

  #Pregunta d
positions <- order(movies$weeks)
movies$ranking <- positions

#Exercise 3 (Converting objects and *.RData-objects)

download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/data.RData", destfile = "data.Rdata")
datas = load("data.Rdata")
ls()
list1 <- c(unlist(list1, use.names = F))
knee <- data.matrix(knee)
ls()

#Exercise 4 (Foreing data structures)

  #Parte a
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/blutdruck.sav", destfile = "blutdruck.sav")
install.packages("foreign")
library("foreign")
data = read.spss("blutdruck.sav")

  #Parte b
install.packages("sas7bdat")
library(sas7bdat)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/s05_01.sas7bdat", destfile = "s05_01.sas7bdat")
data1 = read.sas7bdat("s05_01.sas7bdat")

  #Parte c
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/golf.dta", destfile = "golf.dta")
golf = read.dta("golf.dta")
