install.packages("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/DEV/ViSiElse_1.0.1.zip", repos = NULL, type = "win.binary")
bookvar <- read.csv2("I:/CEPOI/COMMUN/SIMULATION/ViSElSe/TESTDEV/TEST_S9/data/bookvar4.csv")
library(ViSiElse)
Action1 <- c(0, rlnorm(99))
Action2 <- Action1 + (rbeta(n = 100, shape=0.5,shape2=.7)^3  )*30 + 10
Action3 <- Action2 + rnorm(100, mean = 20, sd = 2)
id <- c(1:100)
data<- as.data.frame( cbind(id=id,Action1=Action1,Action2=Action2, Action3=Action3))


# Construction du book
book <- ConvertoViSibook( bookvar)
plot(book)

# ViSigrid
vi <- buildViSiGrid( X=data, book=book,pixel=1 )
plot(vi,alphaZones = 0.5)

# TIme 0 = Action 1
vi <- buildViSiGrid( X=data, book=book,pixel=1, t_0 = "Action1" )
plot(vi,alphaZones = 0.5)

# Changement type black zone
bookvar$BZLtype[4]<- "span"
bookvar$BZAfterFin[5]<- 30
bookvar$BZAfterFin[4]<- 15
book <- ConvertoViSibook( bookvar)
vi <- buildViSiGrid( X=data, book=book,pixel=1, t_0 = "Action1" )
plot(vi,alphaZones = 0.5)

# Autre jeu d'actions
Action1 <- c(0, rlnorm(99))
Action2 <- Action1 + 10+ runif(100)*5
Action3 <- Action2 + 15 + 10+ runif(100)*5
id <- c(1:100)
data<- as.data.frame( cbind(id=id,Action1=Action1,Action2=Action2, Action3=Action3))

# Autre jeu d'actions
Action1 <- c(1:20)
Action2 <- Action1 + 10+ runif(100)*5
Action3 <- Action2 + 15 + 10+ runif(100)*5
id <- c(1:100)
data<- as.data.frame( cbind(id=id,Action1=Action1,Action2=Action2, Action3=Action3))

# Temps de début Action 3
vi <- buildViSiGrid( X=data, book=book,pixel=1, t_0 = "Action3" )
plot(vi,alphaZones = 0.5)
