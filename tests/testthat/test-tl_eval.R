test_that("tl_eval works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )


  vec1 <- treelabel(c("Bird" = 0.8), tree = tree, tree_root = "Animal")
  expect_equal(tl_eval(vec1, Bird > 0.5), TRUE)
  expect_equal(tl_eval(vec1, Eagle > 0.5), NA)
  expect_equal(tl_eval(vec1, Eagle > -0.3), TRUE)
  expect_equal(tl_eval(vec1, Eagle > 0.9), FALSE)


  tl_eval(tl_atmost(vec1), Eagle)
  tl_eval(tl_atleast(vec1), Bird)

  skip("'tl_eval' cannot handle comparisons with two missing values (i.e., ranges)")
  expect_equal(tl_eval(vec1, Parrot > Eagle), NA)

})

test_that("tl_eval can handle references to the score matrix", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )
  vec <- treelabel(
    list(c(Animal = 1, Eagle = 0.3, Bird = 0.9, Mammal = 0.01),
         c(Animal = 1, Mammal = 0.8, Dog = 0.7),
         NA,
         c(Bird = 0.8, Mammal = 0.1)
    ), tree = tree, tree_root = "Animal")

  expect_equal(tl_eval(vec, .scores), tl_score_matrix(vec))
})

test_that("tl_eval can handle complex result values", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )
  vec <- treelabel(
    list(c(Animal = 1, Eagle = 0.3, Bird = 0.9, Mammal = 0.01),
         c(Animal = 1, Mammal = 0.8, Dog = 0.7),
         NA,
         c(Bird = 0.8, Mammal = 0.1)
    ), tree = tree, tree_root = "Animal")

  dat_return <- tl_eval(vec, data.frame(test = Animal, test2 = Bird))
  expect_s3_class(dat_return, "data.frame")

  tib_return <- tl_eval(vec, tibble::tibble(test = Animal, test2 = Bird))
  expect_s3_class(tib_return, "tbl_df")

  # igraph does not support comparison
  expect_error(
    gr_return <- tl_eval(vec, igraph::graph_from_edgelist(cbind("H", letters)))
  )
  gr_return <- tl_eval(vec, igraph::graph_from_edgelist(cbind("H", letters)), check_bounds = FALSE)
  expect_s3_class(gr_return, "igraph")
})
