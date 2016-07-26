coffee <- c( NA,NA,NA,NA,0,0,0,0,0,0,NA)
capsule <- c( 0,0,0,0,NA,NA,NA,NA,NA,NA,NA)
fill_coffee <- c(10,10,10,10,0,0,0,0,0,0,NA)
fill_water <- c( 20,20,20,20,20,20,20,20,20,20,NA)
element_abs <-c( rep( NA,10), 10)
push_B <- c( 30,30,30,30,30,30,30,30,30,30,NA )
drink <- c( 40,40,40,40,40,40,40,40,40,40,NA)
X <- data.frame(id = seq(1,11), coffee,capsule ,element_abs,fill_coffee,fill_water,push_B,drink)
coffee <- c( 1,1,2,NA,NA,NA)
capsule <- c( NA,NA,NA,NA,NA,NA)
fill_coffee <- c( NA,NA,NA,NA,NA,NA)
fill_water <- c( NA,NA,NA,NA,NA,NA)
element_abs <-c( NA,NA,NA,NA,NA,NA)
push_B <-c( 35,35,38,31,NA,NA)
drink <- c( NA,NA,NA,NA,NA,NA)
Xsup <- data.frame(id = c(1,7,7,6,7,6), coffee,capsule ,element_abs,fill_coffee,fill_water,push_B,drink)


coffee <- c( 1,1,1)
capsule <- c( 5,5,5)
fill_coffee <- c(10,10,10)
fill_water <- c( 20,20,20)
push_B <- c( 30,30,30 )
drink <- c( 40,40,40)
X <- data.frame(id = seq(1,3), coffee,capsule,drink ,fill_coffee,fill_water,push_B)
coffee <- c(1, 1,1,2)
capsule <- c( NA,NA,NA,NA)
fill_coffee <- c( NA,NA,NA,NA)
fill_water <- c( NA,NA,NA,NA)
element_abs <-c( NA,NA,NA,NA)
push_B <-c( NA,NA,NA,NA)
drink <- c( NA,NA,NA,NA)
Xsup <- data.frame(id = c(3,1,2,2), coffee,capsule ,fill_coffee,fill_water,push_B,drink)


#
visi1 <- visielse( X )
book <- ConvertFromViSibook( visi1@book ) # Convert book into data.frame
add_delay <- c( "delay_coffee_push","Preparation","l","7","coffee","fill_coffee")
book[7,] <- add_delay
book <- ConvertoViSibook(book)

########

#### Extration of the basics visibook
library(ViSiElse)
showorder_from_data(X)


g2 <- make_ring(10) %>%
          set_vertex_attr("name", value = letters[1:10])
E(g2)$wheigth
path(E(g2))  <- c(1,1,1,1,1,2,2,2,2,2)
igraph::E(g2)$step <- c(1,1,1,1,1,2,2,2,2,2)


g3 <- delete_edges( g2, E(g2)[which( E(g2)$step == 1 )])
plot(g3)

set_
?E
showorder_from_data(X)

plot(g)

showorder <- dfs(g,root=source)$order$name
book@showorder <- rep( NA , dim(book)[1] )
for ( i in which( book@vars %in% showorder) ){
          book@showorder[i] <- which(showorder == book@vars[i] )
}


showorder_from_data(X)



book <- ViSibookfromDATA(X, idsubject = 1)

g <- convert_visibook_graph(X, book,typeg = "weigthed_vertice")
source <- root_visibook( g, book)

showorder <- dfs(g,root=source)$order$name
book@showorder <- rep( NA , dim(book)[1] )
for ( i in which( book@vars %in% showorder) ){
          book@showorder[i] <- which(showorder == book@vars[i] )
}

return(book)
### Step 1 finding All roots : No incomming edge
root_visibook <- function( g, book){
          incoming <- c()
          for (action in book@vars) {
                    incoming <- c( incoming , length(igraph::neighbors(g, v=action, mode = c( "in") ) ) )
          }

          source <- book@vars[ which( incoming == 0 )]
          if( length(source) > 1){
                    ### To work out later
          }
return(source)
}


incoming
igraph::V(g)

igraph::dominator_tree(g, root="coffee")$leftout
igraph::dominator_tree(g, root="fill_coffee")$leftout



library(igraph)
g <- graph_from_literal(R-+A:B:C, A-+D, B-+A:D:E, C-+F:G, D-+L,
                        E-+H, F-+I, G-+I:J, H-+E:K, I-+K, J-+I,
                        K-+I:R, L-+H)
plot(g)
dtree <- dominator_tree(g, root="A")
neighbors(g, v="A", mode = c( "in") )

layout <- layout_as_tree(dtree$domtree, root="R")
layout[,2] <- -layout[,2]
plot(dtree$domtree, layout=layout, vertex.label=V(dtree$domtree)$name)

plot(g, layout=layout.circle)

clique_g <-unique( unlist( max_cliques(g) ) )

ivs_g <- unlist(maximal_ivs(g))
clique_g
visibook

### Isomorphimse
isomorphic(g,g)

