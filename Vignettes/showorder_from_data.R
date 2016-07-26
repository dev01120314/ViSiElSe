#' showorder_from_data Extract the showorder from the data
#'
#' @title Function \code{showorder_from_data}
#' @rdname showorder_from_data
#' @aliases showorder_from_data
#' @export showorder_from_data
#' @param X a data.frame containing execution times
#' @param book a ViSiBook
#' @return a ViSibook object.
#' @seealso See \code{\linkS4class{ViSibook}} to get the definitions of the columns
#' and see  \code{\link{plot-ViSigrid-method}} for examples.
showorder_from_data <- function( X, book=NULL, idsubject = 1 ){
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
