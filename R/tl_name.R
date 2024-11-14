
#' Get a single name for a treelabel
#'
#' Returns the name of the first node from the bottom where the score
#' is higher than the threshold. If multiple nodes pass the threshold and are equally far away
#' from the root, the one with the higher score is returned.
#'
#' @param x `treelabel` vector
#' @param threshold the threshold against which each score is compared.
#'
#' @export
tl_name <- function(x, threshold = 0){
  data <- tl_score_matrix(x)
  colnames <- colnames(data)
  dist <- attr(x, "distances")
  .select_passing_score_by_level(data, threshold = threshold, label = colnames, dist = dist)
}


.select_passing_score_by_level <- function(score, threshold, label, dist){
  vapply(seq_len(nrow(score)), \(idx){
    row <- score[idx,]
    max_score <- -Inf
    max_dist <- -Inf
    sel_label <- NA_character_
    for(j in which(row > threshold)){
      if(dist[j] > max_dist){
        max_dist <- dist[j]
        max_score <- row[j]
        sel_label <- label[j]
      }else if(dist[j] == max_dist & row[j] > max_score){
        max_score <- row[j]
        sel_label <- label[j]
      }else{
        # Do nothing
      }
    }
    sel_label
  }, FUN.VALUE = label[1L])
}

