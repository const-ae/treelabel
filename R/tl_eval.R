
#'
#'
#' @export
tl_eval <- function(x, expr, ...){
  expr <- rlang::enquo(expr)
  data <- vctrs::field(x, "data")
  data_tib <- tibble::as_tibble(data)
  mask <- rlang::as_data_mask(data_tib)
  mask$.tl <- rlang::as_data_pronoun(data_tib)
  rlang::eval_tidy(expr, data = mask, ...)
}

#' @export
tl_get_score <- function(x, name){
  name <- vctrs::vec_cast(name, to = "character")
  data <- vctrs::field(x, "data")
  if(length(name) == 1){
    data[,name]
  }else if(length(name) == length(x)){
    col <- match(name, colnames(data)) - 1
    data[col * nrow(data) + seq_len(nrow(data)) - 1 + 1]
  }else{
    stop("'length(name)' must be one or match the length of 'x'.")
  }
}
