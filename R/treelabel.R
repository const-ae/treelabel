
#' Make a tree label vector
#'
#' @param x object that is converted to a `treelabel`
#' @param tree `igraph` object that represents the hierarchical relationship
#'   of the labels. Must satisfy `igraph::is_tree`. (_)
#' @param tree_root the name of the root of the tree. Default: `"root"`.
#' @param ... additional arguments. Provided for compatability.
#' @param id,label,score strings that specify the names of the respective
#'   columns in the `treelabel.data.frame` constructor.
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
  .propagate_score_up(new_treelabel(x, tree, tree_root = tree_root), overwrite = FALSE)
}

#' @export
#' @rdname treelabel
treelabel.data.frame <- function(x, tree, tree_root = "root", id = "id", label = "label", score = "score", ...){
  stopifnot(igraph::is_tree(tree))
  stopifnot(c(id, label, score) %in% colnames(x))
  vertex_names <- .tree_vertex_names(tree)

  uniq_names <- stats::na.omit(unique(x[[label]]))
  if(! all(uniq_names %in% vertex_names)){
    stop(toString(paste0("'", uniq_names[which(! uniq_names %in% vertex_names)], "'"), width = 40), " not in tree")

  }

  ids <- as.factor(x[[id]])
  data <- matrix(NA, nrow = nlevels(ids), ncol = length(vertex_names),
                 dimnames = list(NULL, vertex_names))
  data <- .assign_to_matrix(data, labels = x[[label]], ids = ids, scores = x[[score]])
  .propagate_score_up(new_treelabel(data, tree, tree_root = tree_root), overwrite = FALSE)
}

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
  treelabel(data.frame(id = ids, label = names, score = vals),
            tree = tree, tree_root = tree_root, ...)
}

#' @export
#' @rdname treelabel
treelabel.numeric <- function(x, tree, tree_root = "root", ...){
  treelabel(data.frame(id =  seq_along(x), label = names(x), score = unname(x)),
            tree = tree, tree_root = tree_root, ...)
}

#' @export
#' @rdname treelabel
treelabel.logical <- function(x, tree, tree_root = "root", ...){
  treelabel(data.frame(id =  seq_along(x), label = names(x), score = unname(x)),
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
  vctrs::field(res, "data") <- mat != 0
  res
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
  new_treelabel(data, attr(like, "tree"), tree_root = attr(like, "tree_root"),
                distances = attr(like, "distances"))
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

