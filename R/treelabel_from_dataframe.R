#' Convert data.frame to treelabel
#'
#' @param id,label,score strings that specify the names of the respective
#'   columns in the `treelabel.data.frame` constructor.
#' @param id the column name of `x` specifying which elements belong together.
#' @param label,score the column names of `x` that specify the names and confidence scores
#'   of each entry
#' @param name the name of the `treelabel` column in the output `data.frame`.
#'
#' @returns a `data.frame` with one row per unique `id` and two columns:
#'   The first is a vector of the ids and the second is the `treelabel` vector.
#'
#' @export
treelabel_from_dataframe <- function(x, tree, tree_root = "root", id = "id", label = "label", score = "score", name = "treelabel", ...){
  stopifnot(igraph::is_tree(tree))
  stopifnot(is.character(id) && length(id) == 1)
  stopifnot(is.character(label) && length(label) == 1)
  stopifnot(is.character(score) && length(score) == 1)
  stopifnot(c(id, label, score) %in% colnames(x))
  stopifnot(is.character(name) && length(name) == 1)
  vertex_names <- .tree_vertex_names(tree)

  uniq_names <- setdiff(unique(x[[label]]), c(NA, ""))
  if(! all(uniq_names %in% vertex_names)){
    stop(toString(paste0("'", uniq_names[which(! uniq_names %in% vertex_names)], "'"), width = 40), " not in tree")
  }

  empty_label_pos <- which(x[[label]] == "")
  na_score_pos <- which(is.na(x[[score]]))
  if(length(setdiff(empty_label_pos, na_score_pos)) > 0){
    stop("The labels contain an empty string at pos ", toString(setdiff(empty_label_pos, na_score_pos), width = 40),
         ", but the corresponding score is not 'NA'.")
  }

  ids <- as.factor(x[[id]])
  # data <- matrix(NA, nrow = nlevels(ids), ncol = length(vertex_names),
  #                dimnames = list(NULL, vertex_names))
  # data <- .assign_to_matrix(data, labels = x[[label]], ids = ids, scores = x[[score]])
  # res <- new_treelabel(data, tree, tree_root = tree_root, ...)
  # res <- .propagate_score_up(res, overwrite = FALSE)
  res <- .treelabel_from_id_label_score(ids, x[[label]], x[[score]], tree = tree, tree_root = tree_root, ...)
  if(is.logical(x[[score]])){
    res <- res |>
      tl_replace_NAs() |>
      tl_as_logical()
  }

  df <- data.frame(x1 = unique(ids), x2 = res)
  colnames(df) <- c(id, name)
  df
}
