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
       c("immune cell" = 1, "lymphoid cell" = 0.99, "t cell" = 0.6, "b cell" = 0.3, "CD4 t cell" = 0.59),
       c("immune cell" = 1, "lymphoid cell" = 0.99, "t cell" = 0.6, "b cell" = 0.2, "CD8 t cell" = 0.59),
       c("immune cell" = 1, "lymphoid cell" = 0.99, "t cell" = 0.89, "b cell" = 0.1, "CD4 t cell" = 0.89),
       c("myeloid cell" = 1),
       c("myeloid cell" = 1, "dendritic cell" = 1))

  tmp <- treelabel(label_list, tree = g)
  tmp


  tib <- dplyr::bind_rows(lapply(label_list, tibble::enframe), .id = "id")
  tmp2 <- treelabel(tib, tree = g, id = "id", label = "name", score = "value")

  (tmp2 + tmp) / 2
  2 + tmp

  tl_eval(tmp2, `lymphoid cell` > `t cell`)
  names <- tl_name(tmp2)

  tl_eval(tmp2, !!rlang::sym(names[1]))

  c(factor(1), tmp2)

  vec <- c("CD4 t cell" = 0.9, "dendritic cell" = 0.2, 'neutrophil' = 0.84, "t cell" = NA)
  tl3 <- treelabel(vec, tree = g)
  treelabel(round(vec), tree = g)
  treelabel(names(vec), tree = g)
  treelabel(as.factor(names(vec)), tree = g)

  vec2 <- c("CD4 t cell", "dendritic cell", 'neutrophil', NA)
  treelabel(vec2, g)
  treelabel(c("root" = 3, 5), g)

  str(rlang::rep_named(c("1", NA, "3"), 2))

  data.frame(x = 1:3, y = c(5, NA, 5)) == data.frame(x = 1:3, y = c(5, NA, 5))


  tibble::tibble(x = c(1,1,1, 1), y = c(5, NA, 5, NA)) |>
    unique()


  tibble::tibble(cell_id = 1:7, label1 = tmp, label2 =  tmp2 + rnorm(7, sd = 0.02)) |>
    dplyr::filter(tl_eval(label1, `lymphoid cell`) > tl_eval(label2, `lymphoid cell`))
    # dplyr::filter(tf_evals(starts_with("label"), `lymphoid cell` > 0.8) |> rowMeans())
    # dplyr::mutate(as.matrix(across(starts_with("label"), \(x) tl_eval, `lymphoid cell` > 0.8)) |> rowMeans())

  tl_eval(tmp, `lymphoid cell`)




  tibble::tibble(cell_id = 1:7, label1 = tmp, label2 =  tmp2 + rnorm(7, sd = 0.02)) |>
    # mutate(across("cell_id", \(x) x + 3))
    # mutate(tl_across(c("label1", "label2"), `t cell` + 1))
    # mutate(tl_across(starts_with("label"), `t cell` + 1))
    dplyr::filter(tl_if_any(dplyr::starts_with("label"), `t cell` > 0.3))

  tibble::tibble(cell_id = 1:7, label1 = tmp, label2 =  tmp2 + rnorm(7, sd = 0.02),
                 label3 = treelabel("dendritic cell", tree = g)) |>
    # dplyr::mutate(new = tl_across(dplyr::starts_with("label"), `dendritic cell`))
    # dplyr::filter(tl_if_any(dplyr::starts_with("label"), `t cell` > 0.8))
    # dplyr::filter(tl_if_any(dplyr::starts_with("label"), `t cell` > 0.8))
    dplyr::mutate(new = tl_countNAs_across(dplyr::starts_with("label"), `dendritic cell`))


  tmp3 <- tl_set_score(tmp, "root", NA)
  vctrs::field(tmp3, "data")

  vctrs::field(dplyr::if_else(tl_eval(tmp, `t cell` > 0.5), tmp, tl_set_score(tmp, "root", NA)), "data")

  tl_set_score(tmp, "lymphoid cell", NA) |> vctrs::field("data")
  tl_set_score(tmp[7], "CD8 t cell", 17, propagate_values_up = TRUE) |> vctrs::field("data")

  indirect_eval <- function(expr){
    tl_eval(tmp, {{expr}})
  }
  indirect_eval(`t cell`)
})


test_that("tree vertices are order by distances to root", {
  edges2 <- c("A", "B",
             "B", "D",
             "A", "C"
             )
  g2 <- igraph::graph(edges2, directed = TRUE)

  plot(g2,layout = igraph::layout_as_tree(g2, root = "A"))
  igraph::V(g2)
})
