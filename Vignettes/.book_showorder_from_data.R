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
