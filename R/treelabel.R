
#' Make a tree label vector
#'
#' @export
treelabel <- function(x, tree, ...){
  UseMethod("treelabel")
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
  if(any(is.na(ids))){
    stop("'NA's in the 'id' column are not allowed.")
  }
  x$..col_zi = match(x[[label]], vertex_names) - 1
  x$..row_zi = match(as.integer(ids), levels(ids)) - 1
  x <- x[! is.na(x$..col_zi),]

  data <- matrix(NA_real_, nrow = nlevels(ids), ncol = length(vertex_names),
                 dimnames = list(NULL, vertex_names))
  data[x$..col_zi * nrow(data) + x$..row_zi + 1] <- x[[score]]
  .propagate_score_up(new_treelabel(data, tree))
}

#' @export
#' @rdname treelabel
treelabel.list <- function(x, tree, ...){
  stopifnot(igraph::is_tree(tree))
  vertex_names <- .tree_vertex_names(tree)
  data <- matrix(NA_real_, nrow = length(x), ncol = length(vertex_names),
                 dimnames = list(NULL, vertex_names))

  assign_df <- tibble::tibble(name = unlist(lapply(x, names)), val = unname(unlist(x)))
  uniq_names <- na.omit(unique(assign_df$name))
  if(! all(uniq_names %in% vertex_names)){
    stop(toString(paste0("'", uniq_names[which(! uniq_names %in% vertex_names)], "'"), width = 40), " not in tree")

  }
  assign_df$col_zi = match(assign_df$name, vertex_names) - 1
  assign_df$row_zi = rep(seq_len(nrow(data)) - 1, times = lengths(x))
  assign_df <- assign_df[! is.na(assign_df$col_zi),]
  data[assign_df$col_zi * nrow(data) + assign_df$row_zi + 1] <- assign_df$val
  .propagate_score_up(new_treelabel(data, tree))
}

#' @export
#' @rdname treelabel
treelabel.numeric <- function(x, tree, ...){
  stopifnot(igraph::is_tree(tree))
  vertex_names <- .tree_vertex_names(tree)
  data <- matrix(NA_real_, nrow = length(x), ncol = length(vertex_names),
                 dimnames = list(NULL, vertex_names))
  names <- names(x)
  uniq_names <- na.omit(unique(names))
  if(! all(uniq_names %in% vertex_names)){
    stop(toString(paste0("'", uniq_names[which(! uniq_names %in% vertex_names)], "'"), width = 40), " not in tree")

  }
  # zi means that the indices start at 0
  assign_df <- tibble::tibble(col_zi = match(names, vertex_names) - 1,
                              row_zi = seq_len(nrow(data)) - 1,
                              val = x)
  assign_df <- assign_df[! is.na(assign_df$col_zi),]
  data[assign_df$col_zi * nrow(data) + assign_df$row_zi + 1] <- assign_df$val
  .propagate_score_up(new_treelabel(data, tree))
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



.clamp <- function(x, min = 0, max = 1){
  pmin(x, max) |> pmax(min)
}

#' @importFrom rlang `%|%`
.propagate_score_up <- function(x){
  data <- vctrs::field(x, "data")
  colnames <- colnames(data)
  tree <- attr(x, "tree")
  dists <- attr(x, "distances")
  children <- lapply(igraph::V(g), \(v){
    match(igraph::neighbors(tree, v, mode = "out")$name, colnames)
  })
  for(idx in order(dists, decreasing = TRUE)){
    child_sum <- .clamp(matrixStats::rowSums2(data, cols = children[[idx]], na.rm=TRUE))
    na_count <- matrixStats::rowCounts(data, value = NA_real_, cols = children[[idx]], na.rm=TRUE)
    child_sum[na_count == length(children[[idx]])] <- NA
    data[,names(children)[idx]] <- data[,names(children)[idx]] %|% child_sum
  }
  new_treelabel(data, tree)
}

