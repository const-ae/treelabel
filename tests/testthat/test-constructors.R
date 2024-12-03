test_that("treelabel constructors works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )
  ref_tree <- igraph::graph_from_literal(
    Animal -+ Bird : Mammal,
    Bird -+ Parrot : Eagle,
    Mammal -+ Dog : Cat
  )

  vec <- treelabel(c("Eagle", "Dog", NA, "Bird"), tree = tree, tree_root = "Animal")

  # Test object structure
  expect_true(is_treelabel(vec))
  expect_equal(vctrs::fields(vec), "data")
  expect_setequal(names(attributes(vec)), c("names", "tree", "tree_root", "distances", "class"))
  expect_true(igraph::identical_graphs(.get_tree(vec), ref_tree))
  expect_equal(.get_tree_root(vec), "Animal")
  expect_equal(.get_distances(vec), c(0, 1, 1, 2, 2, 2, 2))

  # Test names in sets
  vertex_names <- c("Animal", "Bird", "Mammal", "Dog", "Cat", "Parrot", "Eagle")
  data <- vctrs::field(vec, "data")
  expect_setequal(colnames(data), vertex_names)
  expect_equal(colnames(data)[1], vertex_names[1])
  expect_setequal(colnames(data)[2:3], vertex_names[2:3])
  expect_setequal(colnames(data)[4:7], vertex_names[4:7])
  col_match <- match(vertex_names, colnames(data))

  # Test data matrix
  mat <- matrix(c(1, 1, 0, 0, 0, 0, 1,
                  1, 0, 1, 1, 0, 0, 0,
                  rep(NA, 7),
                  1, 1, 0, 0, 0, 0, 0),
                nrow = 4, byrow = TRUE,
                dimnames = list(NULL, vertex_names))
  expect_equal(data, mat[,col_match] == 1)
})


test_that("treelabel constructors works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )


  vec1 <- treelabel(c("Eagle", "Dog", NA, "Bird"), tree = tree, tree_root = "Animal")

  vec2 <- treelabel(c("Eagle" = 1, "Dog" = 1, NA, "Bird" = 1), tree = tree, tree_root = "Animal")
  vec3 <- treelabel(
    list(c(Eagle = 1, Bird = 1),
         c(Animal = 1, Mammal = 1, Dog = 1),
         NA,
         c(Bird = 1)
     ), tree = tree, tree_root = "Animal")
  vec4 <- treelabel(c("Eagle" = TRUE, "Dog" = TRUE, NA, "Bird" = TRUE), tree = tree, tree_root = "Animal")

  expect_equal(vec1, vec2 |> tl_replace_NAs() |> tl_as_logical(), ignore_attr = "tree")
  expect_equal(vec2, vec3, ignore_attr = "tree")
})

test_that("treelabel_from_dataframe works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )


  vec1 <- treelabel(c("Eagle", "Dog", NA, "Bird"), tree = tree, tree_root = "Animal")
  dat <- data.frame(uniq_names = c(1,"b",3,"a"), n = c("Eagle", "Dog", NA, "Bird"), s = TRUE)
  dat2 <- treelabel_from_dataframe(dat, tree, tree_root = "Animal",
                                   id = "uniq_names", label = "n", score = "s", name = "output")
  expect_equal(dat2$output, vec1, ignore_attr = "tree")

  dat <- rbind(dat,
               data.frame(uniq_names = "a", n = "Parrot", s = TRUE))
  dat3 <- treelabel_from_dataframe(dat, tree, tree_root = "Animal",
                                   id = "uniq_names", label = "n", score = "s", name = "output")
  expect_equal(nrow(dat3), 4)
  expect_equal(dat3$output[4], treelabel("Parrot", tree, "Animal"), ignore_attr = "tree")
})



