coffee <- c(  58, 11,  5, 53, 53, 59, 24, 59, 46, 20)
fill_coffee <- c(162,  57,103,154,165,132,  74, 107, 104,  93)
fill_water <- c(  66,  92,54, 78, 74, 114, 91, 129, 71, 56)
push_B <- c( 74, 99, 62, 84, 83, 120, 95, 129, 80, 63 )
drink <- c( 472, 176, 475, 283, 265, 207, 234, 184, 490, 520)
X <- data.frame(id = seq(1,10), coffee, fill_coffee,fill_water,push_B,drink)
path <- 'C:/ViSiElSe/ViSiElSe'
list_function <- list.files( paste0(path,"/R" ))
library(Matrix)
for (i in seq_along( list_function) ){ 
  source( paste0(path,"/R/",list_function[i]) , echo = TRUE)
}
visi1 <- visielse(X,doplot = FALSE)
library(grid)
plot(visi1)
#### Changing the pixel of time
