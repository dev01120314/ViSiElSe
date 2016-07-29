book <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S2/data/bookvar4.csv", sep=";", na.strings="", stringsAsFactors=FALSE)
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S2/data/data4.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)

library(ViSiElse)
# ViSigrid
data
vi <- visielse( X=data, pixel = 0.5)
vi <- visielse( X=data, book=book, pixel= 0.5, informer=NULL)
plot(vi@book)
plot(vi)
vi <- visielse( X=data, book=book, pixel= 0.5, informer=NULL)

data<-data[c(1,2,3),]
vi <- visielse( X=data, book=book, pixel= 2, informer=NULL)
plot(vi, scal.unit.tps = 2)
data
vi@MATp
