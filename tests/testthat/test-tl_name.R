test_that(".select_by_level works", {
  set.seed(1)
  score <- matrix(rnorm(120 * 10), nrow = 120, ncol = 10)
  label <- letters[1:10]
  dist <- rpois(n = 10, lambda = 2)
  thres <- 1.0

  sel <- .select_passing_score_by_level(score, threshold = thres, label = label, dist = dist)
  sel2 <- sapply(seq_len(nrow(score)), \(idx){
    tib <- tibble::tibble(score = score[idx,], label = label, dist) |>
      dplyr::filter(score >= thres) |>
      dplyr::arrange(-dist, -score)
    tib$label[1]
  })
  testthat::expect_equal(sel, sel2)
})


test_that("tl_name works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )

  char_vec <- c("Bird", "Mammal", "Parrot", "Cat")
  vec <- treelabel(char_vec, tree, "Animal")
  expect_equal(tl_name(vec), char_vec)

  vec <- treelabel(list(c("Mammal" = 0.99, "Dog" = 0.7, "Cat" = 0.3)), tree, "Animal")
  expect_equal(tl_name(vec), "Dog")
})

