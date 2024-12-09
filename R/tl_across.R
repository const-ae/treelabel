
#' dplyr-style across functions for treelabels
#'
#' @param .cols tidyselect expression of the column which contain the treelabel
#'   columns
#' @param expr an unquoted expression evaluated with `tl_eval` for each column.
#' @param na.rm boolean that decides if missing values are ignored.
#' @param mode the evaluation subtly differs if the results is numeric or logical
#'
#' @seealso [dplyr::across()]
#'
#' @export
tl_across <- function(.cols, expr, mode = c("logical", "numeric")){
  mode <- rlang::arg_match(mode)
  eval_fnc <- switch (mode,
    logical = tl_is,
    numeric = tl_eval
  )
  dplyr::across({{.cols}}, \(x){
    stopifnot(is_treelabel(x))
    eval_fnc(x, {{expr}})
  })
}

#' @export
#' @rdname tl_across
tl_if_any <- function(.cols, expr){
  dplyr::if_any({{.cols}}, \(x){
    stopifnot(is_treelabel(x))
    tl_is(x, {{expr}})
  })
}

#' @export
#' @rdname tl_across
tl_if_all <- function(.cols, expr){
  dplyr::if_all({{.cols}}, \(x){
    stopifnot(is_treelabel(x))
    tl_is(x, {{expr}})
  })
}

#' @export
#' @rdname tl_across
tl_sum_across <- function(.cols, expr, na.rm = TRUE){
  mat <- as.matrix(tl_across(.cols = {{.cols}}, {{expr}}, mode = "numeric"))
  rowSums(mat, na.rm = na.rm)
}

#' @export
#' @rdname tl_across
tl_mean_across <- function(.cols, expr, na.rm = TRUE){
  mat <- as.matrix(tl_across({{.cols}}, {{expr}}, mode = "numeric"))
  rowMeans(mat, na.rm = na.rm)
}

#' @export
#' @rdname tl_across
tl_count_across <- function(.cols, expr, na.rm = TRUE){
  mat <- as.matrix(tl_across({{.cols}}, {{expr}}, mode = "logical"))
  rowSums(mat, na.rm = na.rm)
}

#' @export
#' @rdname tl_across
tl_fraction_across <- function(.cols, expr, na.rm = TRUE){
  mat <- as.matrix(tl_across({{.cols}}, {{expr}}, mode = "logical"))
  rowMeans(mat, na.rm = na.rm)
}

#' @export
#' @rdname tl_across
tl_countNAs_across <- function(.cols, expr, na.rm = TRUE){
  mat <- as.matrix(tl_across({{.cols}}, {{expr}}))
  matrixStats::rowCounts(mat, value = NA)
}
