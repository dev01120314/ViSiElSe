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
