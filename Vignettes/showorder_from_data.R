#' .book_showorder_from_data : Construction of the book to run the function shoorder_from_data
#' @param obj a data.frame containing execution times and idsubject on the first column or a visibook
#' @return a dataframe.
.book_showorder_from_data <- function(obj ){
          if (class(obj) != "ViSibook") {
           obj <- ViSibookfromDATA( obj , idsubject = 1)
          }
          obj <- ConvertFromViSibook( obj )
          obj <- obj[order(obj$showorder),]
          # if there is delay with showorders
          #
          if (any( obj$typeA == "l" & !is.na( obj$showorder) ) ) {
                    for (index_d in which( obj$typeA == "l") ) { # index_d = 7
                              # if showorder deb si na
                              if (is.na( obj$showorder[ which( obj$vars == obj$deb[index_d] ) ] ) ) {
                                        # if is na showorder fin
                                        if ( is.na( obj$showorder[ which( obj$vars == obj$fin[index_d] ) ] ) ) {
                                                  obj$showorder[ which( obj$vars == obj$deb[index_d] ) ] <- obj$showorder[ index_d ]
                                                  obj$showorder[ index_d ] <- NA
                                        }else{# if showorder fin not na
                                                  obj$showorder[ index_d ] <- NA
                                        }
                              }else{# if showorder deb not na
                                        obj$showorder[ index_d ] <- NA
                              }
                    }

          }
          obj <- obj[which( !is.na(obj$showorder) ),]
          return(obj)
}
#' root_visibook select a root for a graph
#'
#' @title Function \code{root_visibook}
#' @rdname root_visibook
#' @aliases root_visibook
#' @export root_visibook
#' @param g a graph
#' @param Xsup a data.frame containing supplemetary times when there is
#' @param book a data.frame corresponding to a ViSibook
#' @return a ViSibook object.
root_visibook <- function( g, book){
          # Deletion of repetitive actions
          g <- igraph::delete_edges( g, igraph::E(g)[which( igraph::E(g)$step_rep > 1 )] )

          incoming <- c()
          for (action in book$vars) { # action="coffee"
                    incoming <- c( incoming , length(igraph::neighbors(g, v=action, mode = c( "in") ) ) )
          }

          source <- book$vars[ which( incoming == 0 )]
          if( length(source) > 1){
                    # Retain mininum incoming : The vertex must poeple started with if equality takes the fist in the book$vars
                    source <- book$vars[ which.min( incoming )]
          }else{
                    if( length(source) == 0){
                              source <- book$vars[ which.min( incoming )]
                    }
          }
          return(source)
}

#' convert_visibook_graph Convert a visibook into a graph
#'
#' @title Function \code{convert_visibook_graph}
#' @rdname convert_visibook_graph
#' @aliases convert_visibook_graph
#' @export convert_visibook_graph
#' @param book a ViSiBook
#' @param X a data.frame containing execution times
#' @param typeg weighted_vertice : Only one vertices exists between two action weigthed by the number of individuals having pass through
#'                   vertices : One vertive is created for each passage
#' @return a ViSibook object.
#' @seealso See \code{\linkS4class{ViSibook}} to get the definitions of the columns
#' and see  \code{\link{plot-ViSigrid-method}} for examples.
convert_visibook_graph <- function(X, book, Xsup=NULL , typeg = "weigthed_vertice"){
          # X idsubject = 1
          if( !is.null(Xsup) ){ # If supplementary datas : merging X and Xsup
          ## Maximum number of repetition
                    nsup <- max( table( Xsup$id ) )
                    cnames <- c( "id",rep( book$vars , nsup + 1 ) )
          ## Sorting individuals occurences
                    sort_idsup <- sort(Xsup$id,index.return=TRUE)
                    Xsup <- Xsup[sort_idsup$ix,]
                    sort_idsup <- sort_idsup$x
                    # attribute 1 to all that sup 1 time
                    countOc <- rep(1, length(sort_idsup ) )

                    # sort occurances with pertumations
                    countOc_mem <- rep(0, length(sort_idsup))
                    for( pern in seq(nsup,2,-1) ) { # pern = nsu
                    per <- sort_idsup[c(seq(pern,length(sort_idsup)),seq(1,(pern-1) ) )]
                    countOc_ <- as.numeric(sort_idsup == per) - countOc_mem
                    countOc_mem <- as.numeric(countOc_ | countOc_mem )
                    countOc[which( countOc_ == 1 )] <- rep( pern, sum( countOc_ == 1 ) )
                    }
          ### increase the number of column of X
                    temp <- matrix( rep( NA , nsup*( dim(X)[2]-1 )*dim( X )[ 1 ] ) , nrow = dim( X )[ 1 ] , ncol = nsup*( dim( X )[ 2 ]-1) )
                    X <-  cbind(X, temp )
          ### fill the matrix
                    rep_step_c <- rep(1,dim(book)[1] )
                    for (i in seq(1,nsup)) { # i = 2
                              indexl <- which( X$id %in% Xsup$id[ which( countOc == i )] )
                              indexc <- seq(2 , ( dim(book)[1] +1 ) ) + ( i*( dim(book)[1] ))
                              X[ indexl , indexc]  <- Xsup[which( countOc == i),-1]
                              rep_step_c <- c( rep_step_c, rep( i+1 , dim(book)[1] ))
                    }
          }else{
                    cnames <- c("id", book$vars)
                    rep_step_c <- rep(1,dim(book)[1] )
          }

          ####
          # Mofification time when there are equal times
          minX <- 0.000001
          for( i in seq( 1 , dim(X)[1] )) {
                    x <- as.numeric( X[i,-1][ which( !is.na(X[i,-1]))])
                    rep <-  names(which( table(x)>1))
                    if ( length( rep ) > 0 ){
                              for (v in rep){ # v = rep
                              x[ which( as.character(x) == v ) ] <-x[ which( as.character(x) == v ) ] + seq( 0, sum( as.character(x) == v ) - 1 )*minX
                              }
                    }
                    X[i,-1][ which( !is.na(X[i,-1]))] <- x
          }
          g <- igraph::graph.empty( n = dim( book )[1] , directed = TRUE)
          igraph::V(g)$name <- book$vars

          if (typeg == "vertices" ) {
          # Extraction of existing edge in the dataset : 1 individuals 1 vertex
          newedge <- c()
          step_rep <- c()
          for ( i in seq_along(X)[-1] ) { # i = 2
                    action <- cnames[i]
                    index <- which( !is.na( X[, i ] ) )
                    temp <- X[ index , -c(1,i) ]  - X[ index , i ]
                    tnames <- cnames[ -c(1,i) ]
                    trep_step_c <- rep_step_c[-(i-1)]
                    temp1 <- unlist( apply(temp, MARGIN=1, FUN = function(x){
                              if( any( x[which( !is.na(x)) ] < 0 )){
                                        x[which( !is.na(x)) ][ which( x[which( !is.na(x)) ] <= 0 ) ] <- rep(NA ,sum( x[which( !is.na(x)) ] <= 0 ))
                              }
                              return( which.min( x ) )
                    }
                    ))
                    n <- length( temp1 )
                    if( n>0 ){
                              newedge_ <- rep( NA , n*2 )
                              newedge_[ seq(1,length( newedge_),2) ] <- rep( action , n)
                              newedge_[ seq(2,length( newedge_),2) ] <- unlist( tnames[ temp1 ] )
                              newedge <- c( newedge , newedge_ )
                              step_rep <- c(step_rep, trep_step_c[ temp1 ] )
                    }
          }
          g <- igraph::add.edges(g, newedge)
          igraph::E(g)$step_rep <- step_rep
          }
          if (typeg == "weigthed_vertice" ) {
                    newedge <- c()
                    weightedge <- c()
                    step_rep <- c()
                    for ( i in seq_along(X)[-1] ) { # i = 2
                              action <- cnames[i]
                              index <- which( !is.na( X[, i ] ) )
                              temp <- X[ index , -c(1,i) ]  - X[ index , i ]
                              tnames <- cnames[ -c(1,i) ]
                              trep_step_c <- rep_step_c[-(i-1)]
                              temp1 <- unlist( apply(temp, MARGIN=1, FUN = function(x){
                                        if( any( x[which( !is.na(x)) ] < 0 )){
                                                  x[which( !is.na(x)) ][ which( x[which( !is.na(x)) ] <= 0 ) ] <- rep(NA ,sum( x[which( !is.na(x)) ] <= 0 ))
                                        }
                                        return( which.min( x ) )
                              }
                              ))
                              n <- length( unique(temp1) )
                              if (n > 0) {
                              newedge_ <- rep( NA , n*2 )
                              newedge_[ seq(1,length( newedge_)-1,2) ] <- rep( action , n)
                              newedge_[ seq(2,length( newedge_),2) ] <-  unlist( tnames[ sort(unique(temp1)) ] )
                              newedge<-c( newedge , newedge_ )
                              # Weightedge : sum of occurences
                              weightedge <- c( weightedge, unlist( lapply(sort(unique(temp1)) , FUN = function(x)( sum( temp1 == x ) ) ) ))
                              step_rep <- c(step_rep, trep_step_c[ sort(unique(temp1)) ] )
                              }
                    }
                    g <- igraph::add.edges(g, newedge)
                    igraph::E(g)$weight <-  weightedge
                    igraph::E(g)$width <- weightedge
                    igraph::E(g)$step_rep <- step_rep
          }
          return(g)
}


#' showorder_from_data Extract the showorder from the data
#'
#' @title Function \code{showorder_from_data}
#' @rdname showorder_from_data
#' @aliases showorder_from_data
#' @export showorder_from_data
#' @param X a data.frame containing execution times
#' @param Xsup a data.frame containing supplemetary times when there is
#' @param book a ViSiBook
#' @return a ViSibook object.
#' @seealso See \code{\linkS4class{ViSibook}} to get the definitions of the columns
#' and see  \code{\link{plot-ViSigrid-method}} for examples.
showorder_from_data <- function( X, book=NULL, Xsup=NULL , idsubject = 1 ){
          if( is.null(book)){
                    book_ <- .book_showorder_from_data(obj = X)
          }else{
                    book_ <- .book_showorder_from_data(obj = book)
          }
          g <- convert_visibook_graph(X, book_,typeg = "vertices")
          source <- root_visibook( g, book_)
          showorder <- igraph::dfs(g, root=source )$order$name

          if ( !is.null(book)  ) {
                    # Insert the new show order in the book
                    index_bv <- which( !is.na( book@showorder) & book@typeA == "p")
                    temp_showorder <- book@showorder
                    temp_showorder[ index_bv[sort(  book@vars[ index_bv ] , index.return = TRUE)$ix ]] <- sort(showorder , index.return = TRUE)$ix
                    #
                    if (any( book@typeA == "l" & !is.na( book@showorder) ) ) {
                              for (index_d in which( book@typeA == "l") ) { # index_d = 6
                                        # if showorder deb si na
                                        if (is.na( book@showorder[ which( book@vars == book@deb[index_d] ) ] ) ){
                                                  # if is na showorder fin
                                                  if ( is.na( book@showorder[ which( book@vars == book@fin[index_d] ) ] ) ) {
                                                            temp_showorder[ index_d ] <- temp_showorder[  which( book@vars == book@deb[index_d] ) ]
                                                            temp_showorder[  which( book@vars == book@deb[index_d] ) ] <- NA
                                                       ## index_fin <- which( book@vars == book@fin[index_d] )
                                                  }else{# if showorder fin not na : add delay before fin
                                                            index_fin <- which( book@vars == book@fin[index_d] )
                                                            temp <- which( temp_showorder >= temp_showorder[ index_fin ] )
                                                            temp_showorder[ index_d ]  <-  temp_showorder[ index_fin ]
                                                            temp_showorder[ temp ] <- temp_showorder[ temp ] + 1

                                                  }
                                        }else{# if showorder deb not na
                                             index_deb <- which( book@vars == book@deb[index_d] )
                                             temp_showorder[ which( temp_showorder > temp_showorder[ index_deb ] )] <-  temp_showorder[ which( temp_showorder > temp_showorder[ index_deb ] )] + 1
                                             temp_showorder[ index_d ]  <-  temp_showorder[ index_deb ] + 1
                                        }
                              }
                              book@showorder <- temp_showorder
                    }
                    book@showorder <- temp_showorder

          }else{
          book_$showorder[ sort(  book_$vars , index.return = TRUE)$ix ] <- sort(showorder , index.return = TRUE)$ix
          book_ <- ConvertoViSibook( book_ )
          book <- book_
          }

          return(book)


}
