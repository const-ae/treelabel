

#' Low-level function to access the data underlying a tree label
#'
#' @export
tl_score_matrix <- function(x){
  stopifnot(is_treelabel(x))
  vctrs::field(x, "data")
}
