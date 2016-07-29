install.packages("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/DEV/ViSiElse_1.0.1.zip", repos = NULL, type = "win.binary")
bookvar <- read.csv2("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S8/data/bookvar4.csv")
data <- read.csv2("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S8/data/data4.csv")
group<-data$gr
data<-data[,-5]
library(ViSiElse)

# Construction du book
book <- ConvertoViSibook( bookvar)
plot(book)

# ViSigrid
vi <- buildViSiGrid( X=data, book=book,pixel=1 )
plot(vi)
vi <- buildViSiGrid( X=data, book=book,pixel=1 , t_0 = "Action1")
plot(vi)
colnames(data)
vi <- buildViSiGrid( X=data, book=book,pixel=1,tests = TRUE, method = "cut", group=group )
plot(vi)
vi@testsP
vi@informers
mood.test( (data[which(group==1),3]-data[which(group==1),2]),   data[which(group==2),3]-data[which(group==2),2])

median(data[which(group==2),3]-data[which(group==2),2])
median( (data[which(group==1),3]-data[which(group==1),2]))

vi <- buildViSiGrid( X=data, book=book,pixel=1,tests = TRUE, method = "join", group=group )
plot(vi)
vi <- buildViSiGrid( X=data, book=book,pixel=1,tests = TRUE, method = "within", group=group)
plot(vi)

vi <- buildViSiGrid( X=data, book=book,pixel=1,tests = TRUE, method = "cut", group=group, informer= "mean")
plot(vi)
vi <- buildViSiGrid( X=data, book=book,pixel=1,tests = TRUE, method = "join", group=group, informer= "mean")
plot(vi)
vi <- buildViSiGrid( X=data, book=book,pixel=1,tests = TRUE, method = "within", group=group, informer= "mean")
plot(vi)


vi@informers
quantile(data[,2])
quantile(data[,3])
quantile(data[,3] - data[,2])
plot(vi)
# Group
vi <- buildViSiGrid( X = data, book = book, group = group, method = "cut", pixel = 1, test = TRUE )
plot(vi)
vi@informers

vi <- buildViSiGrid( X = data, book = book, group = group, method = "join", pixel = 1, test = TRUE )
plot(vi)
vi@informers

vi <- buildViSiGrid( X = data, book = book, group = group, method = "within", pixel = 1, test = FALSE)
plot(vi)

vi <- buildViSiGrid( X = data, book = book, group = group, method = "join" )
plot(vi)
