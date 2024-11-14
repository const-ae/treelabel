
#' Make a tree label vector
#'
#' @export
treelabel <- function(x, tree, ...){
  UseMethod("treelabel")
}

#' @export
#' @rdname treelabel
treelabel.matrix <- function(x, tree, ...){
  .propagate_score_up(new_treelabel(x, tree), overwrite = FALSE)
}

#' @export
#' @rdname treelabel
treelabel.data.frame <- function(x, tree, id = "id", label = "label", score = "score"){
  stopifnot(igraph::is_tree(tree))
  stopifnot(c(id, label, score) %in% colnames(x))
  vertex_names <- .tree_vertex_names(tree)

  uniq_names <- na.omit(unique(x[[label]]))
  if(! all(uniq_names %in% vertex_names)){
    stop(toString(paste0("'", uniq_names[which(! uniq_names %in% vertex_names)], "'"), width = 40), " not in tree")

  }

  ids <- as.factor(x[[id]])
  data <- matrix(NA_real_, nrow = nlevels(ids), ncol = length(vertex_names),
                 dimnames = list(NULL, vertex_names))
  data <- .assign_to_matrix(data, labels = x[[label]], ids = ids, scores = x[[score]])
  .propagate_score_up(new_treelabel(data, tree), overwrite = FALSE)
}

#' @export
#' @rdname treelabel
treelabel.list <- function(x, tree, ...){
  names <- unlist(lapply(x, names))
  vals <- unname(unlist(x))
  ids <- rep(seq_along(x), times = lengths(x))
  treelabel(data.frame(id = ids, label = names, score = vals), tree = tree)
}

#' @export
#' @rdname treelabel
treelabel.numeric <- function(x, tree, ...){
  treelabel(data.frame(id =  seq_along(x), label = names(x), score = unname(x)), tree = tree)
}

#' @export
#' @rdname treelabel
treelabel.character <- function(x, tree, ...){
  treelabel(rlang::rep_named(x, 1), tree = tree)
}

#' @export
#' @rdname treelabel
treelabel.factor <- function(x, tree, ...){
  treelabel(as.character(x), tree = tree)
}


new_treelabel <- function(data, tree){
  if(! inherits(data, "matrix")){
    stop("'data' must be a matrix")
  }
  vertex_names <- .tree_vertex_names(tree)
  if(! all(colnames(data) %in% vertex_names) &&
     all(vertex_names %in% colnames(data))){
       stop("The 'data' colnames must match exactly the vertex names")
  }
  # Very useful to do things in a bottom up or top down order
  # The distances match the columns of the data
  # The order of the tree vertices is still random
  distances <- igraph::distances(tree, v = "root", mode = "out")
  data <- data[,vertex_names[order(distances)],drop=FALSE]
  distances <- sort(distances)

  vctrs::new_rcrd(
    list(data = data),
    tree = tree,
    distances = distances,
    class = "treelabel"
  )
}


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
    row_zi = match(ids, levels(ids)) - 1,
    val = scores
  )
  assign_df <- assign_df[! is.na(assign_df$col_zi),]
  data[assign_df$col_zi * nrow(data) + assign_df$row_zi + 1] <- assign_df$val
  data
}



