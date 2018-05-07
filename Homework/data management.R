#ejercicio 1 
setwd("D:/Info eco/R/ejercicios")
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee.txt",destfile="knee.txt")


#a)
data=read.table("knee.txt",header=T)
#data=read.table("knee.txt",header=F)

#b)

summary(data)
head(data)
str(data)
names(data)
class(colnames(data))

#C)

colnames(data)=c("treatment","age","sex","agony")

#d)

download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee2.txt",destfile="knee2.txt")
mag=read.table("knee2.txt",header=T,na.strings="missing")

#e)
download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/knee3.txt",destfile="knee3.txt")
jan=read.table("knee3.txt",header=T,sep=";")

#f)

  mag$evil= ifelse(mag$age>40,"old","young")

  save(mag,file="mag.Rdata")
  write.table(mag,file="mag1.raw",quote=F,sep=":")
  write.table(mag,file="mag2.dat",row.names=F,col.names=F)
  write.table(mag,file="mag3.csv")
 
#ejercicio 2

download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/movies.csv",destfile="movies.csv")
movies=read.table("movies.csv",header=T,sep=",")
  
#a)


movies$release <-as.Date (as.character(movies$release),"%d.%m.%Y")
movies$end <-as.Date(as.character(movies$end),"%d.%m.%Y")


#b)

movies$days=movies$end-movies$release
movies$weeks=difftime(movies$end,movies$release,units="weeks")

#c)

movies$year=format(movies$release,"%Y")

#d)

movies <- movies[order(-movies$weeks),]

#ejercicio 3

download.file("https://github.com/franciscorosales-marticorena/ApplStatsR/raw/master/Exercises/data.RData",destfile="data.Rdata")
load("data.Rdata")
ls()
data.matrix(movies,rownames.force = NA)
list1 <- c(unlist(list1,recursive=T,use.names = T))

#ejercicio 4

download.packages("blutdruck.sav",destdir="D:/Info eco/R/ejercicios")

