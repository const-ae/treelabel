
test_that("tree checking works", {
  tree <- igraph::graph_from_literal(
    Animal -+ Bird : Mammal,
    Bird -+ Parrot : Eagle,
    Mammal -+ Dog : Cat
  )
  undir_tree <- .igraph_as_undirected(tree)
  # plot(circle)

  expect_true(.is_tree(tree, "Animal"))
  expect_false(.is_tree(tree, "Bird"))
  expect_true(igraph::is_tree(undir_tree))
  expect_false(.is_tree(undir_tree, "Animal"))


  circle <- igraph::graph_from_literal(
    A--+B,
    B--+C,
    C--+D,
    D--+A
  )

  expect_false(.is_tree(circle, "A"))
})

test_that("tree checking works", {
  tree <- igraph::graph_from_literal(
    Animal -+ Bird : Mammal,
    Bird -+ Parrot : Eagle,
    Mammal -+ Dog : Cat
  )

  undir_tree <- .igraph_as_undirected(tree)
  tree2 <- .make_tree(undir_tree, root = "Animal")
  expect_true(.is_tree(tree2, root = "Animal"))

  tree3 <- .make_tree(undir_tree, root = "Eagle")
  expect_true(igraph::is_tree(tree3))
  expect_false(.is_tree(tree3, root = "Animal"))

  tree4 <- .make_tree(tree3, "Animal")
  expect_true(.is_tree(tree4, root = "Animal"))
})
