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
