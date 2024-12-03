


.clamp <- function(x, min = 0, max = 1){
  pmin(x, max) |> pmax(min)
}

#' @importFrom rlang `%|%`
.propagate_score_up <- function(x, overwrite = FALSE){
  data <- tl_score_matrix(x)
  colnames <- colnames(data)
  tree <- .get_tree(x)
  dists <- .get_distances(x)
  children <- lapply(igraph::V(tree), \(v){
    match(igraph::neighbors(tree, v, mode = "out")$name, colnames)
  })
  for(idx in order(dists, decreasing = TRUE)){
    child_sum <- matrixStats::rowSums2(data, cols = children[[idx]], na.rm=TRUE)
    na_count <- matrixStats::rowCounts(data, value = NA_real_, cols = children[[idx]], na.rm=TRUE)
    child_sum[na_count == length(children[[idx]])] <- NA
    cur_val <- data[,names(children)[idx]]
    if(overwrite){
      data[,names(children)[idx]] <- ifelse(is.na(cur_val), child_sum, pmax(cur_val, child_sum))
    }else{
      data[,names(children)[idx]] <- cur_val %|% child_sum
    }
  }
  .treelabel_like(data, like = x)
}


.propagate_NAs_down <- function(x){
  data <- tl_score_matrix(x)
  colnames <- colnames(data)
  tree <- .get_tree(x)
  dists <- .get_distances(x)
  children <- lapply(igraph::V(tree), \(v){
    match(igraph::neighbors(tree, v, mode = "out")$name, colnames)
  })
  for(idx in order(dists)){
    data[is.na(data[,idx]), children[[idx]]] <- NA
  }
  .treelabel_like(data, like = x)
}
