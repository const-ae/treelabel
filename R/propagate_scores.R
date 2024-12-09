


.clamp <- function(x, min = 0, max = 1){
  pmin(x, max) |> pmax(min)
}

#' @importFrom rlang `%|%`
.propagate_score_up <- function(x, mode = c("sum", "cumsum", "none"), overwrite = FALSE){
  mode <- rlang::arg_match(mode)
  if(mode == "none"){
    return(x)
  }
  data <- tl_score_matrix(x)
  colnames <- colnames(data)
  children <- .get_children(x)
  dists <- .get_distances(x)

  for(idx in order(dists, decreasing = TRUE)){
    child_sum <- matrixStats::rowSums2(data, cols = children[[idx]], na.rm=TRUE)
    na_count <- matrixStats::rowCounts(data, value = NA_real_, cols = children[[idx]], na.rm=TRUE)
    child_sum[na_count == length(children[[idx]])] <- NA_real_
    val <- as.numeric(data[,names(children)[idx]])
    res <- rep(NA_real_, length(val))
    v_is_na <- is.na(val)
    c_is_na <- is.na(child_sum)

    new_val <- if(mode == "sum" & overwrite){
      child_sum
    }else if(mode == "sum" & ! overwrite){
      val %|% child_sum
    }else if(mode == "cumsum"){
      # new_val is only used if there are no NAs
      val + child_sum
    }

    res[  v_is_na &   c_is_na] <- NA
    res[  v_is_na & ! c_is_na] <- child_sum[  v_is_na & ! c_is_na]
    res[! v_is_na &   c_is_na] <- val[! v_is_na &   c_is_na]
    res[! v_is_na & ! c_is_na] <- new_val[! v_is_na & ! c_is_na]

    data[,names(children)[idx]] <- res
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
