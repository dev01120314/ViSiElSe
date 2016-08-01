install.packages("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/DEV/ViSiElse_1.0.1.zip", repos = NULL, type = "win.binary")
bookvar <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S4/data/bookvar4.csv", sep=";", na.strings="", stringsAsFactors=FALSE)
data <- read.csv("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S4/data/data5.csv", dec=",",sep=";", na.strings="", stringsAsFactors=FALSE)
library(ViSiElse)

# Construction du book
book <- ConvertoViSibook( bookvar )
plot(book)



# ViSigrid
vi <- buildViSiGrid( X=data, book=book, pixel= 0.5, informer="mean")
plot(vi, scal.unit.tps = 1)
vi@informers

mean(data[,2])
mean(data[,2]) - sqrt( var(data[,2]))
mean(data[,2]) + sqrt( var(data[,2]))

vi@informers
mean(data[,3])
mean(data[,3]) - sqrt( var(data[,3]))
mean(data[,3]) + sqrt( var(data[,3]))

vi@informers
mean(data[,3] - data[,2])
mean(data[,3]- data[,2]) - sqrt( var(data[,3]- data[,2]))
mean(data[,3]- data[,2]) + sqrt( var(data[,3]- data[,2]))


mean(data[,2]) + mean(data[,3] - data[,2])
mean(data[,2]) - sqrt( var(data[,2])) + mean(data[,3]- data[,2]) - sqrt( var(data[,3]- data[,2]))
mean(data[,2]) + sqrt( var(data[,2])) + mean(data[,3]- data[,2]) + sqrt( var(data[,3]- data[,2]))


vi <- buildViSiGrid( X=data, book=book, pixel= 0.1, informer=NULL)
plot(vi, scal.unit.tps = 1)
vi <- buildViSiGrid( X=data, book=book, pixel= 1, informer=NULL, ncolvect = 14)
plot(vi, scal.unit.tps = 1)

vi <- buildViSiGrid( X=data, book=book, pixel= 10, informer="mean")
vi@informers

vi <- buildViSiGrid( X=data, book=book, pixel= 0.5, informer="median")
vi@informers

quantile( data[,2])
quantile( data[,3])
quantile( data[,3] - data[,2])
quantile( data[,2]) + quantile( data[,3] - data[,2])
