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

  vec3 <- treelabel(c(Dog = 0.5), tree = tree, tree_root = "Animal")
  expect_equal(tl_replace_NAs(vctrs::vec_c(vec1, vec3)),
               tl_replace_NAs(treelabel(c("Bird" = 1, "Dog" = 0.5), tree = tree, tree_root = "Animal")), ignore_attr = "tree")
})


test_that("coercion works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )

  vec <- treelabel(c("Bird", "Dog", "Mammal"), tree, tree_root = "Animal")
  expect_equal(c(vec, "Dog"), treelabel(c("Bird", "Dog", "Mammal", "Dog"), tree, tree_root = "Animal"),
               ignore_attr = "tree")

  vec2 <- treelabel(c("Bird" = 0.3, "Dog" = 0.7, "Mammal" = 1), tree, tree_root = "Animal")
  expect_equal(c(vec2, vec[1]), c(vec2, tl_as_numeric(vec[1])), ignore_attr = "tree")
  expect_equal(c(vec[1], vec2), c(tl_as_numeric(vec[1]), vec2), ignore_attr = "tree")

  expect_equal(c(vec2, "Bird"), c(vec2, tl_as_numeric(vec[1])), ignore_attr = "tree")
  expect_equal(vctrs::vec_c("Bird", vec2), c(tl_as_numeric(vec[1]), vec2), ignore_attr = "tree")
})


test_that("merging different trees works", {
  vec1 <- treelabel(c("Bird", "Dog", "Dolphin"))
  tree <- igraph::graph_from_literal(
    root -+ Bird : Mammal,
    Mammal -+ Dog,
    Bird -+ Eagle
  )
  vec2 <- treelabel(c("Bird", "Dog", "Eagle"), tree = tree)
  new_tree <- tree |> igraph::add_vertices(1, name = "Dolphin") |> igraph::add_edges(c("root", "Dolphin"))
  expect_true(.check_parent_child_relations(tl_tree(c(vec1, vec2)), "root", new_tree, "root"))
})

