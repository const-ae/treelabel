test_that("casting works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )

  tree2 <- igraph::graph_from_literal(root - one)


  vec1 <- treelabel("Bird", tree = tree, tree_root = "Animal")
  vec2 <- treelabel("one", tree = tree2, tree_root = "root")

  expect_equal(vctrs::vec_ptype2(vec1, "Eagle"), vctrs::vec_init(vec1, n=0))
  expect_equal(vctrs::vec_ptype2("Eagle", vec1), vctrs::vec_init(vec1, n=0))
  expect_error(vctrs::vec_ptype2(vec1, vec2))



  expect_equal(vctrs::vec_c(vec1, "Eagle"), treelabel(c("Bird", "Eagle"), tree = tree, tree_root = "Animal"), ignore_attr = "tree")
  expect_equal(vctrs::vec_c(vec1, NA), treelabel(c("Bird", NA), tree = tree, tree_root = "Animal"), ignore_attr = "tree")

  expect_error(vctrs::vec_c(vec1, c("test","hello", "world", "Eagle")))

  expect_equal(vctrs::vec_c("Eagle", vec1), treelabel(c("Eagle", "Bird"), tree = tree, tree_root = "Animal"), ignore_attr = "tree")
  expect_equal(vctrs::vec_c(NA, vec1), treelabel(c(NA, "Bird"), tree = tree, tree_root = "Animal"), ignore_attr = "tree")

})
