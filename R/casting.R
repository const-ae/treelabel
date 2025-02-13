
#' @importFrom vctrs vec_ptype2 vec_cast
NULL


#' @export
vec_ptype2.treelabel.treelabel <- function(x, y, ..., x_arg = "", y_arg = ""){
   # Potentially check if the one tree is a subtree of the other?
  if(! igraph::identical_graphs(.get_tree(x), .get_tree(y))){
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg, details = "tree of 'x' and 'y' is not identical")
  }
  if(.get_tree_root(x) != .get_tree_root(y)){
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg, details = "tree_root of 'x' and 'y' is not identical")
  }
  if(is.logical(tl_score_matrix(x)) && ! is.logical(tl_score_matrix(y))){
    y
  }else{
    # Either both are numeric or both are logical or x is numeric and y is logical.
    # Anyways, the output should be a treelabel with a numeric score matrix
    x
  }
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
vec_cast.treelabel.treelabel <- function(x, to, ..., x_arg = "", to_arg = ""){
  stopifnot(igraph::identical_graphs(.get_tree(x), .get_tree(to)))
  stopifnot(.get_tree_root(x) == .get_tree_root(to))
  if(! igraph::identical_graphs(.get_tree(x), .get_tree(to))){
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, y_arg = to_arg, details = "tree of 'x' and 'to' is not identical")
  }
  if(.get_tree_root(x) != .get_tree_root(to)){
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, y_arg = to_arg, details = "tree_root of 'x' and 'to' is not identical")
  }
  if(is.logical(tl_score_matrix(x)) && ! is.logical(tl_score_matrix(to))){
    x <- tl_as_numeric(x)
  }
  x
}

# NOTE: From the vctrs casting FAQ:
# Please note that for historical reasons, the order of the classes in the method name is in reverse order of the arguments in the
# function signature. The first class represents to, whereas the second class represents x.

#' @export
vec_cast.treelabel.character <- function(x, to, ...){
  x <- treelabel(x, tree = .get_tree(to), tree_root = .get_tree_root(to), distances = .get_distances(to))
  if(is.numeric(tl_score_matrix(to))){
    tl_as_numeric(x)
  }else{
    x
  }
}


