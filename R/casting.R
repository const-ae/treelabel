
#' @importFrom vctrs vec_ptype2 vec_cast
NULL


#' @export
vec_ptype2.treelabel.treelabel <- function(x, y, ...){
   # Potentially check if the one tree is a subtree of the other?
  stopifnot(igraph::identical_graphs(.get_tree(x), .get_tree(y)))
  stopifnot(.get_tree_root(x) == .get_tree_root(y))
  x
}

#' @export
vec_ptype2.treelabel.character <- function(x, y, ...){
  x
}

#' @export
vec_ptype2.character.treelabel <- function(x, y, ...){
  y
}

#' @export
vec_cast.treelabel.treelabel <- function(x, to, ...){
  stopifnot(igraph::identical_graphs(.get_tree(x), .get_tree(to)))
  stopifnot(.get_tree_root(x) == .get_tree_root(to))
  x
}

#' @export
vec_cast.treelabel.character <- function(x, to, ...){
  treelabel(x, tree = .get_tree(to), tree_root = .get_tree_root(to), distances = .get_distances(to))
}

#' #' @export
#' vec_cast.character.treelabel <- function(x, to, ...){
#'   treelabel(tree = attr(x, "tree"), tree_root = attr(x, "tree_root"))
#' }

