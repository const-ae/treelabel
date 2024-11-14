

tl_set_score <- function(x, label, score,
                         propagate_NAs_down = TRUE,
                         propagate_values_up = FALSE){
  stopifnot(is_treelabel(x))
  label <- vctrs::vec_cast(label, "character")

  args <- vctrs::vec_recycle_common(x, label, score)
  x <- args[[1L]]; label <- args[[2L]]; score <- args[[3L]]


  new_data <- .assign_to_matrix(vctrs::field(x, "data"), ids = seq_along(x),
                                labels = label, scores = score)
  res <- new_treelabel(new_data, attr(x, "tree"))
  if(propagate_values_up){
    res <- .propagate_score_up(res, overwrite = TRUE)
  }
  if(propagate_NAs_down){
    res <- .propagate_NAs_down(res)
  }
  res
}
