
#' dplyr-style across functions for treelabels
#'
#' @param .cols tidyselect expression of the column which contain the treelabel
#'   columns
#' @param expr an unquoted expression evaluated with `tl_eval` for each column.
#' @param ... addtional parameters passed on to `across`
#' @param na.rm boolean that decides if missing values are ignored.
#'
#' @seealso [dplyr::across()]
#'
#' @export
tl_across <- function(.cols, expr, ...){
  if(! requireNamespace("dplyr")) stop("This function depends on 'dplyr' package. Please install it.")
  dplyr::across({{.cols}}, \(x){
    stopifnot(is_treelabel(x))
    tl_eval(x, {{expr}})
  }, ...)
}

#' @export
#' @rdname tl_across
tl_if_any <- function(.cols, expr, ...){
  if(! requireNamespace("dplyr")) stop("This function depends on 'dplyr' package. Please install it.")
  dplyr::if_any({{.cols}}, \(x){
    stopifnot(is_treelabel(x))
    tl_eval(x, {{expr}})
  }, ...)
}

#' @export
#' @rdname tl_across
tl_if_all <- function(.cols, expr, ...){
  if(! requireNamespace("dplyr")) stop("This function depends on 'dplyr' package. Please install it.")
  dplyr::if_all({{.cols}}, \(x){
    stopifnot(is_treelabel(x))
    tl_eval(x, {{expr}})
  }, ...)
}

#' @export
#' @rdname tl_across
tl_sum_across <- function(.cols, expr, ..., na.rm = TRUE){
  mat <- as.matrix(tl_across(.cols = {{.cols}}, {{expr}}, ...))
  rowSums(mat, na.rm = na.rm)
}

#' @export
#' @rdname tl_across
tl_mean_across <- function(.cols, expr, ..., na.rm = TRUE){
  mat <- as.matrix(tl_across({{.cols}}, {{expr}}, ...))
  rowMeans(mat, na.rm = na.rm)
}

#' @export
#' @rdname tl_across
tl_countNAs_across <- function(.cols, expr, ..., na.rm = TRUE){
  mat <- as.matrix(tl_across({{.cols}}, {{expr}}, ...))
  matrixStats::rowCounts(mat, value = NA)
}
