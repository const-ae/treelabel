test_that("tl_replace_NAs works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )


  vec <- treelabel(
    list(c(Eagle = 0.3, Bird = 0.9),
         c(Animal = 1, Mammal = 0.8, Dog = 0.7),
         NA,
         c(Bird = 1)
    ), tree = tree, tree_root = "Animal")


  mat <- tl_score_matrix(vec)
  mat2 <- mat
  mat[is.na(mat)] <- 0
  mat[is.na(mat2[,1]),] <- NA
  expect_equal(tl_score_matrix(tl_replace_NAs(vec)), mat)
})

test_that("tl_atmost works", {
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


  vec_mod <- tl_atmost(vec)
  expect_equal(tl_score_matrix(vec_mod)[1,],
               c(Animal = 1, Bird = 0.9, Mammal = 0.01, Parrot = 0.6, Eagle = 0.3, Dog = 0.01, Cat = 0.01))
  expect_equal(tl_score_matrix(vec_mod)[2,],
               c(Animal = 1, Bird = 0.2, Mammal = 0.8, Parrot = 0.2, Eagle = 0.2, Dog = 0.7, Cat = 0.1))

  vec_mod2 <- tl_atmost(tl_as_logical(vec))
  expect_equal(tl_score_matrix(vec_mod2)[1,],
               c(Animal = TRUE, Bird = TRUE, Mammal = TRUE, Parrot = FALSE, Eagle = TRUE, Dog = TRUE, Cat = TRUE))
  expect_equal(tl_score_matrix(vec_mod2)[2,],
               c(Animal = TRUE, Bird = FALSE, Mammal = TRUE, Parrot = FALSE, Eagle = FALSE, Dog = TRUE, Cat = FALSE))
})
