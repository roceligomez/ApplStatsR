#A

rm(list=ls())
plaintext = "ftbopdifdvboepsptbwpmwjpefmdjofbnbmgjubopftubcbwjfoepmbufmfwjtjpotfoubepfombtbmbzbqspwfdipqbsbefdjsmfrvfibcjbdpmhbepfmmjcspefejftuffofmufoefefspefspqbsptbmpnjspdpnptjopivcjfsbfoufoejepobebrvjfspefdjsejkpbnbmgjuboprvfopmpifdpmhbepqpsrvfqsfwjbnfoufmpivcjfsbnpkbepdpombnbohvfsbojqpsrvftfnfibzbdbjepbmbhvbtjnqmfnfoufmpifdpmhbepqpsrvftjqbsbwfsdpnpsftjtufbmbjoufnqfsjfmptfncbuftefftubobuvsbmfabeftfsujdbmbjefbftefevdibnqefkbsvomjcspefhfpnfusjbdpmhbepbmbjoufnqfsjfqbsbwfstjbqsfoefdvbuspdptbtefmbwjebsfbm"
t <- match(unlist(strsplit(plaintext,"")),letters)
table(t) #se repite más a b y la f (todo se corrió a la derecha una letra)

letritas <- function(m) {
  if (m==1) {output=27}else{
    output=m-1}
    return(output)}
vainilla <- length(496)
vainilla <-letritas(t)
revelacion <-letters[vainilla]

#B

rm(list=ls())


pasos <- 1000
viajes <- 100
retorno <- numeric(length = pasos)
prueba <- numeric(length = viajes)

for (v in 1:viajes) {
  Neal <- c(0,0)
for (i in 1:pasos) {
choice<-sample(1:4,1,replace=T)
if (choice==1) {Neal<-Neal+c(1,0)} else {
  if (choice==2) {Neal<-Neal+c(-1,0)} else {
    if (choice==3) {Neal<-Neal+c(0,1)} else{
      Neal<-Neal+c(0,-1)
    }
  }
}
retorno[i]=ifelse(Neal==c(-1,0),1,0)
}
prueba[v] <- sum(retorno)}

r<- ifelse(prueba>0,1,0)

prob <- sum(r)/viajes
