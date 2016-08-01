install.packages("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/DEV/ViSiElse_1.0.1.tar.gz", repos = NULL, type = "source")
bookvar <- read.csv2("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S10/data/bookvar4.csv")
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S10/data/data4.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)

library(ViSiElse)

vi <- buildViSiGrid( X <- data , book = bookvar, pixel= 0.2)
vi@book
plot(vi@book)

# Autre data
bookvar <- read.csv2("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S10/data/bookvar3.csv")
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S10/data/data.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)
book <- ConvertoViSibook( bookvar)
vi <- buildViSiGrid( X <- data , book = book, pixel= 1)
plot(vi@book)
 plot(vi)

# Changement 1 : Pas de changement
plot(book)
n <- length(book@vars)
book <- changeShoworder( book, c(1:n))
plot(book)

# Changement 1 : Delay en haut
plot(book)
n <- length(book@vars)
delay <- book@showorder[ which(book@typeA == "l")]
punct <- book@showorder[ which(book@typeA == "p")]
book <- changeShoworder( book,v= c(delay,punct))
plot(book)

# Rajouter les variable non visible
showO <- book@showorder
index <- which( is.na(showO))
showO[index] <- seq( max( showO[-index])+1, max( showO[-index])+length(index) )
book@showorder <- showO
plot(book)

vi <- buildViSiGrid( X <- data , book = book)
plot(vi)

