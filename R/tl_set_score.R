

tl_set_score <- function(x, label, score,
                         propagate_NAs_down = TRUE,
                         propagate_score_up = c("sum", "cumsum", "none"),
                         overwrite = TRUE){
  stopifnot(is_treelabel(x))
  label <- vctrs::vec_cast(label, "character")

  args <- vctrs::vec_recycle_common(x, label, score)
  x <- args[[1L]]; label <- args[[2L]]; score <- args[[3L]]


  new_data <- .assign_to_matrix(tl_score_matrix(x), ids = seq_along(x),
                                labels = label, scores = score)
  res <- .treelabel_like(new_data, x)
  res <- .propagate_score_up(res, mode = propagate_score_up, overwrite = overwrite)
  if(propagate_NAs_down){
    res <- .propagate_NAs_down(res)
  }
  res
}




#' Replace each NA with the maximum value that it could have
#'
#' `tl_atmost`: In pseudo-notation, this function calculates for any node `n` that is `NA`,
#' `score = score(parent(n)) - sum(children(parent(n)), na.rm=TRUE)`.
#'
#' `tl_atleast`: In pseudo-notation, this function calculates for any node `n` that is `NA`,
#' `score = 0`.
#'
#' @param x `treelabel` vector
#'
#' @returns the modified `treelabel` vector
#'
#' @export
tl_atleast <- function(x){
  tl_replace_NAs(x)
}

#' @rdname tl_atleast
#' @export
tl_atmost <- function(x){
  .prepare_tree_traversal(x, \(data, nodes, children){
    for(idx in nodes){
      max_remain <- pmax(0, data[,idx] - matrixStats::rowSums2(data, cols = children[[idx]], na.rm=TRUE))
      for(child in children[[idx]]){
        old_vals <- data[, child]
        data[, child] <- old_vals %|% vctrs::vec_cast(max_remain, old_vals)
      }
    }
    .treelabel_like(data, like = x)
  })
}


#' Replace `NA`s with zeros
#'
#' Replaces all `NA`s except in score matrix with zeros, except for the
#' "root" column. This function is useful for comparing values.#'
#'
#' @param x `treelabel` vector
#'
#'
#'
#' @export
tl_replace_NAs <- function(x){
  stopifnot(is_treelabel(x))

  mat <- tl_score_matrix(x)
  root_col <- mat[,1,drop=FALSE]
  rest_cols <- mat[,-1,drop=FALSE]
  rest_cols[is.na(rest_cols)] <- 0
  rest_cols[is.na(root_col), ] <- NA
  .treelabel_like(cbind(root_col, rest_cols), x)
}

#' Convert the underlying data-representation from logical to numeric
#'
#' @param x `treelabel` vector
#'
#' @export
tl_as_logical <- function(x){
  stopifnot(is_treelabel(x))
  mat <- tl_score_matrix(x)
  new_mat <- as.logical(mat)
  attributes(new_mat) <- attributes(mat)
  .treelabel_like(new_mat, x)
}

#' @export
#' @rdname tl_as_logical
tl_as_numeric <- function(x){
  stopifnot(is_treelabel(x))
  mat <- tl_score_matrix(x)
  new_mat <- as.numeric(mat)
  attributes(new_mat) <- attributes(mat)
  .treelabel_like(new_mat, x)
}
