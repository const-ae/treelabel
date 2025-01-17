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

test_that("treelabel list constructor can handle all forms of empty as NA", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )
  lst <- list(c(Eagle = 1, Bird = 1),
              c(Animal = 1, Mammal = 1, Dog = 1),
              NA,
              c(Bird = 1)
  )
  vec1 <- treelabel(lst, tree = tree, tree_root = "Animal")

  # NULL is treated as NA
  lst[3] <- list(NULL)
  vec2 <- treelabel(lst, tree = tree, tree_root = "Animal")

  lst[[3]] <- NA
  vec3 <- treelabel(lst, tree = tree, tree_root = "Animal")

  expect_equal(vec2, vec1, ignore_attr = "tree")
  expect_equal(vec3, vec1, ignore_attr = "tree")
})

test_that("treelabel list constructor works with different inputs",{
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )
  lst <- list(
    c("Eagle", "Bird"),
    c("Mammal")
  )
  vec1 <- treelabel(lst, tree = tree, tree_root = "Animal")
  lst[[2]] <- factor(c("Mammal"))
  vec2 <- treelabel(lst, tree = tree, tree_root = "Animal")

  lst[[2]] <- c("Mammal" = 1.0)
  vec3 <- treelabel(lst, tree = tree, tree_root = "Animal")

  expect_equal(vec2, vec1, ignore_attr = "tree")
  expect_equal(vec3, vec1, ignore_attr = "tree")

  names(lst) <- c("first_entry", "second_entry")
  vec4 <- treelabel(lst, tree = tree, tree_root = "Animal")
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

test_that("propagation works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )

  score_list <- list(
    c(Eagle = 0.8, Parrot = 0.1),
    c(Eagle = 0.8, Parrot = 0.1, Bird = 0.1)
  )
  vec1 <- treelabel(score_list, tree = tree, tree_root = "Animal", propagate_up = "none")
  vec2 <- treelabel(score_list, tree = tree, tree_root = "Animal", propagate_up = "sum")
  vec3 <- treelabel(score_list, tree = tree, tree_root = "Animal", propagate_up = "cumsum")

  ref <- array(c(NA, NA, NA, 0.1, NA, NA, 0.1, 0.1, 0.8, 0.8, NA, NA, NA, NA),
                dim = c(2L, 7L), dimnames = list(NULL, c("Animal", "Bird", "Mammal", "Parrot", "Eagle", "Dog", "Cat")))
  expect_equal(tl_score_matrix(vec1), ref)

  ref[1,"Bird"] <- 0.9
  ref[1,"Animal"] <- 0.9
  ref[2,"Animal"] <- 0.1
  expect_equal(tl_score_matrix(vec2), ref)

  ref[2,"Bird"] <- 1.0
  ref[2,"Animal"] <- 1.0
  expect_equal(tl_score_matrix(vec3), ref)
})


test_that("propagation works", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )

  res <- replicate(20, {
    treelabel(sample(igraph::V(tree)$name, size = 100, replace = TRUE), tree, "Animal")
  })

  lapply(res, \(x) colnames(tl_score_matrix(x)))[1:5]
  lapply(res, \(x) .get_distances(x))[1:5]
})



