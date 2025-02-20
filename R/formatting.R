




#' @export
format.treelabel <- function(x, ...){
  names <- tl_name(x)
  scores <- tl_get(x, names)
  if(is.logical(scores)){
    names
  }else{
    res <- paste0(names, "(", sprintf("%.2f", scores), ")")
    res[is.na(names) | is.na(scores)] <- NA
    res
  }
}


#' @importFrom vctrs obj_print_footer
#' @export
obj_print_footer.treelabel <- function(x, ...){
  vertex_names <- .tree_vertex_names(.get_tree(x))
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
  colnames(tl_score_matrix(x))
}

#### This is necessary for pretty printing the
#### treelabel in S4Vectors::DataFrame's

setOldClass("treelabel")

#' @importFrom S4Vectors showAsCell
NULL
#' @export
methods::setMethod("showAsCell", "treelabel",
                   function(object){
                     format.treelabel(object)
                   })
