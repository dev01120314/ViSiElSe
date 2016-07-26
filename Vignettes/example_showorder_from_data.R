# Example working right for showorder_from_data

library(ViSiElse)
source('C:/ViSiElSe/Vignettes/showorder_from_data.R')
source('C:/ViSiElSe/Vignettes/.book_showorder_from_data.R')
source('C:/ViSiElSe/Vignettes/root_visibook.R')
source('C:/ViSiElSe/Vignettes/convert_visibook_graph.R')

# Dataset :
          coffee <- c( 1,1,1)
          capsule <- c( 5,5,5)
          fill_coffee <- c(10,10,10)
          fill_water <- c( 20,20,20)
          push_B <- c( 30,30,30 )
          drink <- c( 40,40,40)
X <- data.frame(id = seq(1,3), coffee,capsule,drink ,fill_coffee,fill_water,push_B)

#
book <- showorder_from_data(X)  # Created a ViSibook, which order is obtained from realisation order of the datas
plot(book)


