


.clamp <- function(x, min = 0, max = 1){
  pmin(x, max) |> pmax(min)
}

#' @importFrom rlang `%|%`
.propagate_score_up <- function(x, overwrite = FALSE){
  data <- tl_score_matrix(x)
  colnames <- colnames(data)
  tree <- attr(x, "tree")
  dists <- attr(x, "distances")
  children <- lapply(igraph::V(g), \(v){
    match(igraph::neighbors(tree, v, mode = "out")$name, colnames)
  })
  for(idx in order(dists, decreasing = TRUE)){
    child_sum <- matrixStats::rowSums2(data, cols = children[[idx]], na.rm=TRUE)
    na_count <- matrixStats::rowCounts(data, value = NA_real_, cols = children[[idx]], na.rm=TRUE)
    child_sum[na_count == length(children[[idx]])] <- NA
    if(overwrite){
      cur_val <- data[,names(children)[idx]]
      data[,names(children)[idx]] <- ifelse(is.na(cur_val), child_sum, pmax(cur_val, child_sum))
    }else{
      data[,names(children)[idx]] <- data[,names(children)[idx]] %|% child_sum
    }
  }
  new_treelabel(data, tree)
}


.propagate_NAs_down <- function(x){
  data <- tl_score_matrix(x)
  colnames <- colnames(data)
  tree <- attr(x, "tree")
  dists <- attr(x, "distances")
  children <- lapply(igraph::V(g), \(v){
    match(igraph::neighbors(tree, v, mode = "out")$name, colnames)
  })
  for(idx in order(dists)){
    data[is.na(data[,idx]), children[[idx]]] <- NA
  }
  new_treelabel(data, tree)
}
