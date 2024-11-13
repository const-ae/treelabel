test_that("tree definition is easy", {


  edges <- c("root", "immune cell",
             "root", "epithelial cell",
             "immune cell", "myeloid cell",
             "immune cell", "lymphoid cell",
             "lymphoid cell", "t cell",
             "lymphoid cell", "b cell",
             "t cell", "CD4 t cell",
             "t cell", "CD8 t cell",
             "t cell", "treg cell",
             "myeloid cell", "neutrophil",
             "myeloid cell", "dendritic cell",
             "myeloid cell", "macrophage")

  g <- igraph::graph(edges, directed = TRUE)
  plot(g, layout = igraph::layout_as_tree(g, root = "root"))

})


test_that("making a tree labels is easy", {

  label_list <- list(c("immune cell" = 1, "lymphoid cell" = 0.99, "b cell" = 0.7),
       c("immune cell" = 1, "lymphoid cell" = 0.99, "t cell" = 0.6, "b cell" = 0.3, "CD4 t cell" = 0.59),
       c("immune cell" = 1, "lymphoid cell" = 0.99, "t cell" = 0.6, "b cell" = 0.2, "CD8 t cell" = 0.59),
       c("immune cell" = 1, "lymphoid cell" = 0.99, "t cell" = 0.89, "b cell" = 0.1, "CD4 t cell" = 0.89),
       c("myeloid cell" = 1),
       c("myeloid cell" = 1, "dendritic cell" = 1))

  tmp <- treelabel(label_list, tree = g)
  tmp
  tmp2 <- .propagate_score_up(tmp)
  vctrs::vec_data(tmp2)
})

