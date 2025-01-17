test_that("vec_math works as expected", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )


  vec <- treelabel(
    list(c(Animal = 1, Eagle = 0.3, Bird = 0.9, Mammal = 0.01),
         c(Animal = 1, Mammal = 0.8, Dog = 0.7),
         NA,
         c(Bird = 1)
    ), tree = tree, tree_root = "Animal")

  sum <- sum(vec, na.rm=TRUE)
  expect_equal(drop(tl_score_matrix(sum)),
               colSums(tl_score_matrix(vec), na.rm=TRUE))

  prod <- prod(vec, na.rm=TRUE)
  expect_equal(drop(tl_score_matrix(prod)),
               matrixStats::colProds(tl_score_matrix(vec), na.rm=TRUE))
  mean <- mean(vec, na.rm=TRUE)
  expect_equal(drop(tl_score_matrix(mean)),
               colMeans(tl_score_matrix(vec), na.rm=TRUE))

})
