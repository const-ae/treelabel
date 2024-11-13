
#' Make a tree label vector
#'
#' @export
treelabel <- function(x, tree){
  UseMethod("treelabel")
}

#' @export
#' @rdname treelabel
treelabel.list <- function(x, tree){
  stopifnot(igraph::is_tree(tree))
  vertex_names <- .tree_vertex_names(tree)
  data <- matrix(NA_real_, nrow = length(x), ncol = length(vertex_names),
                 dimnames = list(NULL, vertex_names))

  for(idx in seq_along(x)){
    sel <- names(x[[idx]])
    if(any(! sel %in% colnames(data))){
      stop(paste0(sel[which(! sel %in% colnames(data))], collapse = ", "), " not in tree")
    }
    data[idx, sel] <- unname(x[[idx]])
  }
  new_treelabel(data, tree)
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

  vctrs::new_rcrd(list(data = data), tree = tree, class = "treelabel")
}

#' @export
format.treelabel <- function(x, ...){
  data <- vctrs::field(x, "data")
  max_idx <- apply(data, 1, \(row) which.max(row))
  colnames(data)[max_idx]
}


#' @importFrom vctrs obj_print_footer
#' @export
obj_print_footer.treelabel <- function(x, ...){
  vertex_names <- .tree_vertex_names(attr(x, "tree"))
  cat("# Tree: ", toString(vertex_names, width = 40), "\n", sep = "")
}


.tree_vertex_names <- function(tree){
  igraph::V(tree) |> igraph::as_ids()
}

.label_names <- function(x){
  .tree_vertex_names(attr(x, "tree"))
}

.clamp <- function(x, min = 0, max = 1){
  pmin(x, max) |> pmax(min)
}

#' @importFrom rlang `%|%`
.propagate_score_up <- function(x){
  data <- vctrs::field(x, "data")
  colnames <- colnames(data)
  tree <- attr(x, "tree")
  dists <- igraph::distances(tree, v = "root", mode = "out")
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

