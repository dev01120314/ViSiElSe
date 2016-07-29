book <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S1/data/bookvar1.csv", sep=";", na.strings="", stringsAsFactors=FALSE)
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S1/data/data1.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)

library(ViSiElse)

# quantity = "N" : Valeur par default
data
vi <- visielse( data )
vi <- visielse( data , book=book)
vi <- visielse( data , book=book, pixel = 1)
plot( vi, scal.unit.tps = 10)
# quantity = "dens"
data
vi <- visielse( data , book=book, pixel = 1.5)
vi <- visielse( data , book=book, quantity = "dens", pixel = 1.5)

