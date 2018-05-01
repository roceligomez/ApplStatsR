#E1
#a
#download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt", destfile = "knee.txt")
setwd("D:/Documentos/RDoc/E3")
#data = read.table("knee.txt", header = F)
data = read.table("knee.txt", header = T)

#b
summary(data)
head(data)
str(data)
names(data)

#c
data = read.table("knee.txt", header = T,
                  colClasses = c(NA, "factor", "numeric", "factor", "factor"),
                  col.names = c("treatment", "age", "sex", "agony"))

#d
#download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt", destfile = "knee2.txt")
data2 = read.table("knee2.txt", header = T,
                   na.strings = "missing")

#e
#download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee3.txt", destfile = "knee3.txt")
data3 = read.table("knee3.txt", header = T,
                   sep = ";")

#f
data2$f <- factor(ifelse(data2$age > 40, "old", "young"))

save(data2, file = "data2.Rdata")

write.table(data2, file = "data2.raw", quote = F, sep = ":")

write.table(data2, file = "data2.dat", col.names = F, row.names = F)

write.table(data2, file = "data2a.csv")   
write.csv(data2, file = "data2b.csv")     #separado por ,
write.csv2(data2, file = "data2c.csv")    #separado por ;

#E2
#a
#download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/movies.csv", destfile = "movies.csv")
data = read.table("movies.csv", header = T,
                  sep = ",")

str(data)
data$release <- as.Date(data$release, format = "%d.%m.%Y")
data$end <- as.Date(data$end, format = "%d.%m.%Y")
str(data)

#b
data$days <- data$end - data$release
data$weeks <- trunc(data$days/7)

#c
data$year <- format(data$release,"%Y")

#d
data <- data[order(data$weeks, decreasing = T),]

#e
años <- c(unique(data$year))
maxu <- function(n){a = max(data$TotalGross[data$year == n])
  output = data$Title[data$TotalGross == a]
  return(output)}
sapply(años,maxu)


#E3
#a
#download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/data.RData", destfile = "data.RData")
load("data.RData")
ls()
list1 <- c(unlist(list1, use.names = F))
knee <- data.matrix(knee)

#E4
#a
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/s05_01.sas7bdat", destfile = "s05_01.sas7bdat")
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/golf.dta", destfile = "golf.dta")
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/blutdruck.sav", destfile = "blutdruck.sav")
#install.packages("sas7bdat")
require(foreign)
require(sas7bdat)
data1 = read.spss("blutdruck.sav", to.data.frame=TRUE)
data2 = read.sas7bdat("s05_01.sas7bdat")
data3 = read.dta("golf.dta")

