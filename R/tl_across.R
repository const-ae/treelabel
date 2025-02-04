
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
  if(rlang::quo_is_missing(rlang::enquo(expr))){
    dplyr::across({{.cols}}, ...)
  }else{
    dplyr::across({{.cols}}, \(x){
      stopifnot(is_treelabel(x))
      tl_eval(x, {{expr}})
    }, ...)
  }
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


.prepare_across_fun <- function(.cols, expr, ...){
  df <- tl_across(.cols = {{.cols}}, {{expr}}, ...)
  type <- vctrs::vec_ptype_common(!!! df)
  if(is_treelabel(type)){
    names <- colnames(tl_score_matrix(type))
    mat <- do.call(cbind, lapply(df, \(x) as.vector(tl_score_matrix(x))))
    reconstr_fnc <- \(res){
      .treelabel_like(matrix(res, ncol = length(names), dimnames = list(NULL, names)), type)
    }
  }else if(is.matrix(type)){
    names <- lapply(df, colnames)
    for(idx in seq_along(names)){
      all.equal(names[[idx]], names[[1]])
    }
    mat <- do.call(cbind, lapply(df, \(x) as.vector(x)))
    reconstr_fnc <- \(res){
      matrix(res, ncol = ncol(type), dimnames = list(NULL, names[[1]]))
    }
  }else{
    mat <- as.matrix(df)
    reconstr_fnc <- identity
  }
  list(matrix = mat, reconstr_fnc = reconstr_fnc)
}

#' @export
#' @rdname tl_across
tl_sum_across <- function(.cols, expr, ..., na.rm = TRUE){
  tmp <- .prepare_across_fun({{.cols}}, {{expr}}, ...)
  tmp$reconstr_fnc(rowSums(tmp$matrix, na.rm = na.rm))
}

#' @export
#' @rdname tl_across
tl_mean_across <- function(.cols, expr, ..., na.rm = TRUE){
  tmp <- .prepare_across_fun({{.cols}}, {{expr}}, ...)
  tmp$reconstr_fnc(rowMeans(tmp$matrix, na.rm = na.rm))
}

#' @export
#' @rdname tl_across
tl_countNAs_across <- function(.cols, expr, ..., na.rm = TRUE){
  tmp <- .prepare_across_fun({{.cols}}, {{expr}}, ...)
  tmp$reconstr_fnc(matrixStats::rowCounts(tmp$matrix, value = NA))
}
