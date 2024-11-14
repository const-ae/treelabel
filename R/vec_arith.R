#' @importFrom vctrs vec_proxy_equal
#' @export
vec_proxy_equal.treelabel <- function(x, ...){
  tibble::as_tibble(vctrs::field(x, "data"))
}

#' @importFrom vctrs vec_arith
#' @export
#' @method vec_arith treelabel
vec_arith.treelabel <- function(op, x, y, ...){
  UseMethod("vec_arith.treelabel", y)
}

#' @export
vec_arith.treelabel.default <- function(op, x, y, ...){
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.treelabel treelabel
vec_arith.treelabel.treelabel <- function(op, x, y, ...){
  stopifnot(igraph::identical_graphs(attr(x, "tree"), attr(y, "tree")))

  op_fn <- getExportedValue("base", op)
  args <- vctrs::vec_recycle_common(x, y)
  new_data <- op_fn(vctrs::field(args[[1L]], "data"), vctrs::field(args[[2L]], "data"))
  new_treelabel(new_data, attr(x, "tree"))
}

#' @export
#' @method vec_arith.treelabel numeric
vec_arith.treelabel.numeric <- function(op, x, y, ...){
  op_fn <- getExportedValue("base", op)
  args <- vctrs::vec_recycle_common(x, y)

  new_data <- op_fn(vctrs::field(args[[1L]], "data"), args[[2L]])
  new_treelabel(new_data, attr(x, "tree"))
}

#' @importFrom vctrs vec_arith.numeric
#' @export
#' @method vec_arith.numeric treelabel
vec_arith.numeric.treelabel <- function(op, x, y, ...){
  op_fn <- getExportedValue("base", op)
  args <- vctrs::vec_recycle_common(x, y)

  new_data <- op_fn(args[[1L]], vctrs::field(args[[2L]], "data"))
  new_treelabel(new_data, attr(y, "tree"))
}
