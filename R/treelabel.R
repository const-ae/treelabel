
#' Make a tree label vector
#'
#' @param x object that is converted to a `treelabel`
#' @param tree `igraph` object that represents the hierarchical relationship
#'   of the labels. Must satisfy `igraph::is_tree`. (_)
#' @param tree_root the name of the root of the tree. Default: `"root"`.
#' @param propagate_up mode how scores are propagated through the tree.
#'  \describe{
#'    \item{`"sum"`}{each node where the score is `NA` is replaced by the
#'      sum of its children's scores}
#'    \item{`"cumsum"`}{each node is replaced by the
#'      sum of its children's scores plus its own score (where `NA` is treated as zero)}
#'    \item{`"none"`}{No values other values in the tree structure are modified}
#'   }
#' @param ... additional arguments. Provided for compatability.
#'
#'
#' @export
treelabel <- function(x, tree,  tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  if(missing(x)){
    treelabel.missing(tree = tree, tree_root = tree_root, ...)
  }else{
    UseMethod("treelabel")
  }
}

#' @export
#' @rdname treelabel
treelabel.matrix <- function(x, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  res <- new_treelabel(x, tree, tree_root = tree_root, ...)
  res <- .propagate_score_up(res, mode = propagate_up, overwrite = FALSE)
  if(is.logical(x)){
    res <- tl_as_logical(res)
  }
  res
}

#' @importFrom rlang `%||%`
#' @export
#' @rdname treelabel
treelabel.list <- function(x, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  # vctrs records currently do not support being named
  x <- unname(x)
  # Convert to list of numerics
  x <- lapply(x, \(.x){
    if(is.null(.x)){
      NA
    }else if(rlang::is_character(.x)){
      rlang::rep_named(.x, 1)
    }else if(is.factor(.x)){
      rlang::rep_named(as.character(.x), 1)
    }else if(rlang::is_atomic(.x)){
      .x
    }else{
      stop("The content of the list must be atomic vectors (e.g., character, numerics or factors). Not a ", typeof(x))
    }
  })

  names <- vctrs::list_unchop(lapply(x, \(.x) names(.x) %||% rlang::rep_along(.x, NA_character_) ))
  vals <- unname(vctrs::list_unchop(x))
  ids <- rep(seq_along(x), times = lengths(x))
  res <- .treelabel_from_id_label_score(ids, names, vals, tree = tree, tree_root = tree_root, propagate_up = propagate_up, ...)
  res
}

#' @export
#' @rdname treelabel
treelabel.numeric <- function(x, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  .treelabel_from_id_label_score(seq_along(x), names(x), unname(x), tree = tree, tree_root = tree_root, propagate_up = propagate_up, ...)
}

#' @export
#' @rdname treelabel
treelabel.logical <- function(x, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  res <- .treelabel_from_id_label_score(seq_along(x), names(x), unname(x), tree = tree, tree_root = tree_root, propagate_up = propagate_up, ...)
  res |>
    tl_replace_NAs() |>
    tl_as_logical()
}

#' @export
#' @rdname treelabel
treelabel.character <- function(x, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  res <- treelabel(rlang::rep_named(x, 1), tree = tree, tree_root = tree_root, propagate_up = propagate_up, ...)
  res |>
    tl_replace_NAs() |>
    tl_as_logical()
}

#' @export
#' @rdname treelabel
treelabel.factor <- function(x, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  treelabel(as.character(x), tree = tree, tree_root = tree_root, propagate_up = propagate_up, ...)
}

#' @export
#' @rdname treelabel
treelabel.missing <- function(x, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  treelabel(character(0L), tree, tree_root = tree_root, propagate_up = propagate_up)
}

.treelabel_like <- function(data, like){
  new_treelabel(data, .get_tree(like), tree_root = .get_tree_root(like),
                distances = .get_distances(like))
}

.treelabel_from_id_label_score <- function(ids, labels, scores, tree, tree_root = "root", propagate_up = c("sum", "cumsum", "none"), ...){
  stopifnot(igraph::is_tree(tree))
  vertex_names <- .tree_vertex_names(tree)


  uniq_names <- setdiff(unique(labels), c(NA, ""))
  if(! all(uniq_names %in% vertex_names)){
    stop(toString(paste0("'", uniq_names[which(! uniq_names %in% vertex_names)], "'"), width = 40), " not in tree")
  }

  empty_label_pos <- which(labels == "")
  na_score_pos <- which(is.na(scores))
  if(length(setdiff(empty_label_pos, na_score_pos)) > 0){
    stop("The labels contain an empty string at pos ", toString(setdiff(empty_label_pos, na_score_pos), width = 40),
         ", but the corresponding score is not 'NA'.")
  }

  ids <- as.factor(ids)
  data <- matrix(NA, nrow = nlevels(ids), ncol = length(vertex_names),
                 dimnames = list(NULL, vertex_names))
  data <- .assign_to_matrix(data, labels = labels, ids = ids, scores = scores)
  res <- new_treelabel(data, tree, tree_root = tree_root, ...)
  res <- .propagate_score_up(res, mode = propagate_up, overwrite = FALSE)
  res
}

new_treelabel <- function(data, tree, tree_root = "root", distances = NULL){
  if(! inherits(data, "matrix")){
    stop("'data' must be a matrix")
  }

  if(! tree_root %in% .tree_vertex_names(tree)){
    stop("'tree_root=", tree_root, "' is not in vertices.")
  }
  tree <- .make_tree(tree, root = tree_root)

  vertex_names <- .tree_vertex_names(tree)
  if(! all(colnames(data) %in% vertex_names) &&
     all(vertex_names %in% colnames(data))){
       stop("The 'data' colnames must match exactly the vertex names")
  }
  # Very useful to do things in a bottom up or top down order
  # The distances match the columns of the data
  # The order of the tree vertices is still random
  if(is.null(distances)){
    distances <- igraph::distances(tree, v = tree_root, mode = "out")
    data <- data[,vertex_names[order(distances)],drop=FALSE]
    distances <- sort(distances)
  }else{
    stopifnot(! is.unsorted(distances))
  }
  vctrs::new_rcrd(
    list(data = data),
    tree = tree,
    tree_root = tree_root,
    distances = distances,
    class = "treelabel"
  )
}


#' Check if a vector is a treelabel
#'
#' @param x object
#'
#'
#' @export
is_treelabel <- function(x){
  inherits(x, "treelabel")
}

#' Extract tree or root from treelabel
#'
#' @param x object
#'
#' @export
tl_tree <- function(x){
  .get_tree(x)
}

#' @rdname tl_tree
#' @export
tl_tree_root <- function(x){
  .get_tree_root(x)
}


.assign_to_matrix <- function(data, labels, ids, scores){
  ids <- as.factor(ids)
  if(any(is.na(ids)) || any(is.na(levels(ids)))){
    stop("'NA's in the 'id' column are not allowed.")
  }

  assign_df <- tibble::tibble(
    col_zi = match(labels, colnames(data)) - 1,
    row_zi = match(ids, unique(ids)) - 1,
    val = scores
  )
  assign_df <- assign_df[! is.na(assign_df$col_zi),]
  data[assign_df$col_zi * nrow(data) + assign_df$row_zi + 1] <- assign_df$val
  data
}


# Get attributes in a consistent way
.get_tree <- function(x){
  attr(x, "tree")
}

.get_tree_root <- function(x){
  attr(x, "tree_root")
}

.get_distances <- function(x){
  attr(x, "distances")
}

.get_children <- function(x){
  tree <- .get_tree(x)
  colnames <- colnames(tl_score_matrix(x))
  children <- lapply(igraph::V(tree), \(v){
    match(igraph::neighbors(tree, v, mode = "out")$name, colnames)
  })
  names(children) <- igraph::V(tree)$name
  children <- children[colnames]
  children
}

