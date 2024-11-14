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

