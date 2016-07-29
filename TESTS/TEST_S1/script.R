install.packages("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/DEV/ViSiElse_1.0.1.zip", repos = NULL, type = "win.binary")
bookvar <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S1/data/bookvar1.csv", sep=";", na.strings="", stringsAsFactors=FALSE)
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S1/data/data1.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)

library(ViSiElse)

# Construction du book

book <- ConvertoViSibook( bookvar)
plot(book)

# ViSigrid
vi <- buildViSiGrid( X=data, book=book, pixel=1)
plot(vi)
