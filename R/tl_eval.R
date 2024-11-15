
#' Evaluate an expression in the tree label environment
#'
#' @param x `treelabel` vector
#' @param expr an unquoted expression that is evaluated in the treelabel
#'   environment. Can, for example, be used to check if a particular
#'   score is higher than a threshold
#' @param ... additional arguments passed on to `rlang::eval_tidy`
#'
#' @export
tl_eval <- function(x, expr, ...){
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
