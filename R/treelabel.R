
#' Make a tree label vector
#'
#' @param x object that is converted to a `treelabel`
#' @param tree `igraph` object that represents the hierarchical relationship
#'   of the labels. Must satisfy `igraph::is_tree`. (_)
#' @param tree_root the name of the root of the tree. Default: `"root"`.
#' @param ... additional arguments. Provided for compatability.
#'
#'
#' @export
treelabel <- function(x, tree,  tree_root = "root", ...){
  if(missing(x)){
    treelabel.missing(tree = tree, tree_root = tree_root, ...)
  }else{
    UseMethod("treelabel")
  }
}

#' @export
#' @rdname treelabel
treelabel.matrix <- function(x, tree, tree_root = "root", ...){
  .propagate_score_up(new_treelabel(x, tree, tree_root = tree_root, ...), overwrite = FALSE)
}

#' @importFrom rlang `%||%`
#' @export
#' @rdname treelabel
treelabel.list <- function(x, tree, tree_root = "root", ...){
  names <- unlist(lapply(x, \(.x){
    if(is.null(.x)){
      NULL
    }else{
      names(.x) %||% rlang::rep_along(.x, NA)
    }
  }))
  vals <- unname(unlist(x))
  ids <- rep(seq_along(x), times = lengths(x))
  .treelabel_from_id_label_score(ids, names, vals, tree = tree, tree_root = tree_root, ...)
}

#' @export
#' @rdname treelabel
treelabel.numeric <- function(x, tree, tree_root = "root", ...){
  .treelabel_from_id_label_score(seq_along(x), names(x), unname(x), tree = tree, tree_root = tree_root, ...)
}

#' @export
#' @rdname treelabel
treelabel.logical <- function(x, tree, tree_root = "root", ...){
  res <- .treelabel_from_id_label_score(seq_along(x), names(x), unname(x), tree = tree, tree_root = tree_root, ...)
  res |>
    tl_replace_NAs() |>
    tl_as_logical()
}

#' @export
#' @rdname treelabel
treelabel.character <- function(x, tree, tree_root = "root", ...){
  res <- treelabel(rlang::rep_named(x, 1), tree = tree, tree_root = tree_root, ...)
  res |>
    tl_replace_NAs() |>
    tl_as_logical()
}

#' @export
#' @rdname treelabel
treelabel.factor <- function(x, tree, tree_root = "root", ...){
  treelabel(as.character(x), tree = tree, tree_root = tree_root, ...)
}

#' @export
#' @rdname treelabel
treelabel.missing <- function(x, tree, tree_root = "root", ...){
  treelabel(character(0L), tree, tree_root = tree_root)
}

.treelabel_like <- function(data, like){
  new_treelabel(data, .get_tree(like), tree_root = .get_tree_root(like),
                distances = .get_distances(like))
}

.treelabel_from_id_label_score <- function(ids, labels, scores, tree, tree_root = "root", ...){
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
  res <- .propagate_score_up(res, overwrite = FALSE)
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

