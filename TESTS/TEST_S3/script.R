book <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S4/data/bookvar4.csv", sep=";", na.strings="", stringsAsFactors=FALSE)
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S4/data/data5.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)
library(ViSiElse)


# quantity = "N"
vi <- visielse( X=data, book=book, pixel= 0.5, informer=NULL)
plot(vi, scal.unit.tps = 1)
vi <- visielse( X=data, book=book, pixel= 0.1, informer=NULL)
plot(vi, scal.unit.tps = 1)
vi <- visielse( X=data, book=book, pixel= 1, informer=NULL, ncolvect = 14)
plot(vi, scal.unit.tps = 1)

# quantity = "dens"
data
vi <- visielse( X=data, book=book, pixel= 0.5, informer=NULL,quantity = "dens")
plot(vi, scal.unit.tps = 1)
vi <- visielse( X=data, book=book, pixel= 0.1, informer=NULL,quantity = "dens")
plot(vi, scal.unit.tps = 1)
vi <- visielse( X=data, book=book, pixel= 1, informer=NULL, ncolvect = 14,quantity = "dens")
plot(vi, scal.unit.tps = 1)

