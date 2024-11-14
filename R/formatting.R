




#' @export
format.treelabel <- function(x, ...){
  names <- tl_name(x)
  scores <- tl_get_score(x, names)
  paste0(names, "(", sprintf("%.2f", scores), ")")
}


#' @importFrom vctrs obj_print_footer
#' @export
obj_print_footer.treelabel <- function(x, ...){
  vertex_names <- .tree_vertex_names(attr(x, "tree"))
  cat("# Tree: ", toString(vertex_names, width = 40), "\n", sep = "")
}

#' @importFrom vctrs vec_ptype_abbr
#' @export
vec_ptype_abbr.treelabel <- function(x, ...) {
  "tl"
}


.tree_vertex_names <- function(tree){
  igraph::V(tree) |> igraph::as_ids()
}

.label_names <- function(x){
  colnames(vctrs::field(x, "data"))
}
