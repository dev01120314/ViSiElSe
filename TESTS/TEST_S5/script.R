install.packages("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/DEV/ViSiElse_1.0.1.zip", repos = NULL, type = "win.binary")
bookvar <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S6/data/bookvar6.csv", dec="," , sep=";", na.strings="", stringsAsFactors=FALSE)
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S6/data/data6.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)
datasup <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S6/data/data6sup.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)

library(ViSiElse)

# Construction du book
book <- ConvertoViSibook( bookvar )
plot(book)
print(book)

# ViSigrid
vi <- buildViSiGrid( X=data, Xsup=datasup, book=book, pixel= 0.5, informer="mean")
plot(vi, scal.unit.tps = 1, alphaZones = 0.7)
vi@informers


