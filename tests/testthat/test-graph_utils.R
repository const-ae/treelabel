
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


test_that("tree compatibility check works", {
  tree1 <- igraph::graph_from_literal(A -+ B : C)
  tree1_root <- "A"
  tree2 <- igraph::graph_from_literal(A -+ B, B -+ C)
  tree2_root <- "A"
  expect_error(.check_tree_compatible(tree1, tree1_root = "A",
                                      tree2, tree2_root = "A"))

  .graph_entry_exit_times(igraph::make_tree(10), root = 1)

  tree1 <- igraph::graph_from_literal(A -+ B : C)
  tree2 <- igraph::graph_from_literal(A -+ B)
  expect_true(.check_tree_compatible(tree1, tree1_root = "A",
                                     tree2, tree2_root = "A"))

  tree1 <- igraph::graph_from_literal(A -+ B : C)
  tree2 <- igraph::graph_from_literal(A -+ C)
  expect_true(.check_tree_compatible(tree1, tree1_root = "A",
                                     tree2, tree2_root = "A"))

  tree1 <- igraph::graph_from_literal(A -+ B -+ C)
  tree2 <- igraph::graph_from_literal(A -+ C -+ B)
  expect_error(.check_tree_compatible(tree1, tree1_root = "A",
                                     tree2, tree2_root = "A"))

})


test_that("tree compatibility check works", {
  tree1 <- igraph::graph_from_literal(A -+ B : C,
                                      C -+ D)
  tree2 <- igraph::graph_from_literal(A -+ D)
  res <- .merge_trees(tree1, tree1_root = "A", tree2, tree2_root = "A")
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree1, "A"))
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree2, "A"))
  expect_true(.is_tree(res$tree, res$tree_root))

  tree1 <- igraph::graph_from_literal(A -+ B : C, C -+ D)
  tree2 <- igraph::graph_from_literal(A -+ D)
  .merge_trees(tree1, tree1_root = "A", tree2, tree2_root = "A")
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree1, "A"))
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree2, "A"))
  expect_true(.is_tree(res$tree, res$tree_root))

  tree1 <- igraph::graph_from_literal(A -+ B : C, C -+ D)
  tree2 <- .singleton_igraph("B")
  .merge_trees(tree1, tree1_root = "A", tree2, tree2_root = "B")
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree1, "A"))
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree2, "B"))
  expect_true(.is_tree(res$tree, res$tree_root))

  tree1 <- igraph::graph_from_literal(A -+ B : C, C -+ D)
  tree2 <- igraph::graph_from_literal(C -+ E : D, D -+ F : H)
  .merge_trees(tree1, tree1_root = "A", tree2, tree2_root = "C")
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree1, "A"))
  expect_true(.check_tree_compatible(res$tree, res$tree_root, tree2, "C"))
  expect_true(.is_tree(res$tree, res$tree_root))

})
