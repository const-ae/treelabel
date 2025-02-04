

# tl_set_score <- function(x, label, score,
#                          propagate_NAs_down = TRUE,
#                          propagate_score_up = c("sum", "cumsum", "none"),
#                          overwrite = TRUE){
#   stopifnot(is_treelabel(x))
#   label <- vctrs::vec_cast(label, "character")
#
#   args <- vctrs::vec_recycle_common(x, label, score)
#   x <- args[[1L]]; label <- args[[2L]]; score <- args[[3L]]
#
#
#   new_data <- .assign_to_matrix(tl_score_matrix(x), ids = seq_along(x),
#                                 labels = label, scores = score)
#   res <- .treelabel_like(new_data, x)
#   res <- .propagate_score_up(res, mode = propagate_score_up, overwrite = overwrite)
#   if(propagate_NAs_down){
#     res <- .propagate_NAs_down(res)
#   }
#   res
# }

#' Modify the treelabel values
#'
#' @param ... tidyeval dots which work like `dplyr::mutate()`.
#' @param .propagate_NAs_down boolean that decides if the score of children is to `NA` if
#'   their parents score is `NA`.
#' @inheritParams treelabel
#'
#' @returns the modified treelabel vector
#' @export
tl_modify <- function(x, ..., .propagate_NAs_down = TRUE){
  tree <- .get_tree(x)
  vertex_names <- .tree_vertex_names(tree)
  vars <- rlang::enquos(..., .named = FALSE)

  new_names <- names(vars)
  if(! all(new_names == "" | new_names %in% vertex_names)){
    wrong_name <- new_names[which(! new_names %in% vertex_names)]
    stop("Assigning to '", toString(wrong_name, width = 30), "' is not possible as they are not vertices.")
  }
  for(idx in seq_along(vars)){
    n <- new_names[idx]
    res <- .eval_impl(x, expr = !!vars[[idx]])

    mat <- tl_score_matrix(x)
    if(is.data.frame(res) || is.matrix(res)){
      res <- as.matrix(res)
      if(nrow(res) != 1 && nrow(res) != nrow(mat)){
        stop("The length of the value must be 1 or ", nrow(mat), " and not ", nrow(res))
      }
      if(nrow(res) == 1){
        # Recycle to match nrow of mat
        res <- matrix(res, nrow = nrow(mat), ncol = ncol(res), byrow = TRUE,
                      dimnames = list(NULL, colnames(res)))
      }
      nn <- colnames(res)
      if(n != ""){
        stop("The name for the ", idx, " element must be empty (not '", n, "'),",
             "beause the expression evaluated to a data.frame/matrix.")
      }
      if(! all(nn %in% vertex_names)){
        wrong_name <- nn[which(! nn %in% vertex_names)]
        stop("Assigning to '", toString(wrong_name, width = 30), "' is not possible as they are not vertices.")
      }
      mat[,nn] <- res
    }else{
      mat[,n] <- res
    }
    x <- .treelabel_like(mat, x)
    if(.propagate_NAs_down){
      x <- .propagate_NAs_down(x)
    }
  }
  x
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
  data <- tl_score_matrix(x)
  children <- .get_children(x)
  dists <- .get_distances(x)
  for(idx in order(dists)){
    max_remain <- pmax(0, data[,idx] - matrixStats::rowSums2(data, cols = children[[idx]], na.rm=TRUE))
    for(child in children[[idx]]){
      old_vals <- data[, child]
      data[, child] <- old_vals %|% vctrs::vec_cast(max_remain, old_vals)
    }
  }
  .treelabel_like(data, like = x)
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


#' Modify tree of an existing treelabel
#'
#' @param x `treelabel` vector
#' @param new_tree an igraph object that describes the new tree
#' @param tree_root the name of the tree root. Defaults to the
#'   to the name of the tree root of `x`.
#' @param ... arguments passed to [`treelabel.matrix`]
#'
#' @returns a `treelabel` vector described by the new tree.
#'
#' @export
tl_modify_tree <- function(x, new_tree, tree_root = tl_tree_root(x), ...){
  dat <- tl_score_matrix(x)
  vertices <- igraph::V(new_tree)$name
  old_sel <- intersect(colnames(dat), vertices)
  new_sel <- setdiff(vertices, colnames(dat))
  new_mat <- cbind(dat[,old_sel,drop=FALSE], matrix(NA, nrow = nrow(dat), ncol = length(new_sel), dimnames = list(NULL, new_sel)))
  treelabel(new_mat, tree = new_tree, tree_root = tree_root, ...)
}
