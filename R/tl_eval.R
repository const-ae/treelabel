
#' Evaluate an expression in the tree label environment
#'
#' @param x `treelabel` vector
#' @param expr an unquoted expression that is evaluated in the treelabel
#'   environment. Can, for example, be used to check if a particular
#'   score is higher than a threshold
#' @param ... additional arguments passed on to `rlang::eval_tidy`
#'
#' **Note**: Do not perform any stateful calculation in `expr` as it
#'   is evaluated multiple times
#'
#' @export
tl_eval <- function(x, expr, ...){
  stopifnot(is_treelabel(x))
  # A <- .eval_impl(x, {{expr}}, ...)
  B <- .eval_impl(tl_atmost(x), {{expr}}, ...)
  C <- .eval_impl(tl_atleast(x), {{expr}}, ...)
  ifelse(B == C, B, NA)
}

.eval_impl <- function(x, expr, ...){
  expr <- rlang::enquo(expr)
  data <- tl_score_matrix(x)
  data_tib <- tibble::as_tibble(data)
  mask <- rlang::as_data_mask(data_tib)
  mask$.tl <- rlang::as_data_pronoun(data_tib)
  rlang::eval_tidy(expr, data = mask, ...)
}

#' Get the score for a name of a tree node
#'
#' @param x `treelabel` vector
#' @param name the name of one of the nodes in the tree
#'
#' @export
tl_get <- function(x, name){
  name <- vctrs::vec_cast(name, to = "character")
  args <- vctrs::vec_recycle_common(x, name)
  x <- args[[1L]]; name <- args[[2L]]

  data <- tl_score_matrix(x)
  if(! all(stats::na.omit(name) %in% colnames(data))){
    stop("Illegal name")
  }

  col <- match(name, colnames(data)) - 1
  data[col * nrow(data) + seq_len(nrow(data)) - 1 + 1]
}

# #'
# #' @rdname tl_eval
# #' @export
# tl_is <- function(x, expr, ...){
#   A <- tl_eval(x, {{expr}}, ...)
#   stopifnot(rlang::is_logical(A))
#   B <- tl_eval(tl_atmost(x), {{expr}}, ...)
#   C <- tl_eval(tl_atleast(x), {{expr}}, ...)
#   stopifnot(rlang::is_logical(B))
#   # This takes A if A is not NA.
#   # If is.na(A) the result is FALSE if B is FALSE,
#   # otherwise it is NA.
#   A %|% ifelse(A == B, A, NA)
# }

