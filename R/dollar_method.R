

#' @rdname cash-.treelabel
#' @export
.DollarNames.treelabel <- function(x, pattern = ""){
  vertex_names <- .tree_vertex_names(.get_tree(x))
  grep(pattern, vertex_names, value = TRUE)
}

#' Dollar access the underlying values inside a treelabel
#'
#' @param x the treelabel vector
#' @param name the name of the vertex in the tree
#' @param pattern the search pattern that is matched against the vertices
#'   in the tree.
#'
#' @return a boolean or numeric vectors with the column from `tl_score_matrix(x)[,name]`
#'
#' @export
`$.treelabel` <- function(x, name){
  vertex_names <- .tree_vertex_names(.get_tree(x))
  if(! name %in% vertex_names){
    stop("'name = ", toString(name, width=40), "' is not a valid vertex: ", toString(vertex_names, width=60))
  }
  tl_score_matrix(x)[,name]
}

