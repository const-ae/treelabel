#' @importFrom vctrs vec_proxy_equal
#' @export
vec_proxy_equal.treelabel <- function(x, ...){
  mat <- tl_score_matrix(x)
  tibble::as_tibble(mat)
}

#' @importFrom vctrs vec_proxy_compare
#' @export
vec_proxy_compare.treelabel <- function(x, ...){
  stop("'treelabel' does not support relative comparisons (e.g., 'min', 'max', '<', '>', 'pmin', ...)")
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
  stopifnot(igraph::identical_graphs(.get_tree(x), .get_tree(y)))

  op_fn <- getExportedValue("base", op)
  args <- vctrs::vec_recycle_common(x, y)
  new_data <- op_fn(vctrs::field(args[[1L]], "data"), vctrs::field(args[[2L]], "data"))
  .treelabel_like(new_data, like = x)
}

#' @export
#' @method vec_arith.treelabel numeric
vec_arith.treelabel.numeric <- function(op, x, y, ...){
  op_fn <- getExportedValue("base", op)
  args <- vctrs::vec_recycle_common(x, y)

  new_data <- op_fn(vctrs::field(args[[1L]], "data"), args[[2L]])
  .treelabel_like(new_data, like = x)
}

#' @importFrom vctrs vec_arith.numeric
#' @export
#' @method vec_arith.numeric treelabel
vec_arith.numeric.treelabel <- function(op, x, y, ...){
  op_fn <- getExportedValue("base", op)
  args <- vctrs::vec_recycle_common(x, y)

  new_data <- op_fn(args[[1L]], vctrs::field(args[[2L]], "data"))
  .treelabel_like(new_data, like = x)
}

#' @importFrom vctrs vec_math
#' @export
vec_math.treelabel <- function(.fn, .x, ...){
  summary_fncs <- c("prod", "sum", "any", "all", "cummax", "cummin", "cumprod", "cumsum", "mean")
  fn <- switch(.fn,
    "prod" = matrixStats::colProds,
    "sum" = matrixStats::colSums2,
    "any" = matrixStats::colAnys,
    "all" = matrixStats::colAlls,
    "cummax" = matrixStats::colCummaxs,
    "cummin" = matrixStats::colCummins,
    "cumprod" = matrixStats::colCumprods,
    "cumsum" = matrixStats::colCumsums,
    "mean" = matrixStats::colMeans2,
    getExportedValue("base", .fn)
  )
  new_data <- if(.fn %in% summary_fncs){
    vec <- fn(tl_score_matrix(.x), ..., useNames = TRUE)
    matrix(vec, nrow = 1, dimnames = list(NULL, names(vec)))
  }else{
    fn(tl_score_matrix(.x), ...)
  }
  .treelabel_like(new_data, like = .x)
}

