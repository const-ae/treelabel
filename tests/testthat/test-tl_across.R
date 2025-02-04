test_that("tl_across and derivatives work", {
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

  dat <- data.frame(x = vec, y = vec * 2)
  expect_equal(dat |> dplyr::mutate(tl_across(c(x, y), Bird)),
               data.frame(x = tl_get(dat$x, "Bird"), y = tl_get(dat$y, "Bird")))

  sum <- dat |> dplyr::mutate(new = tl_sum_across(c(x, y), Bird)) |> dplyr::pull(new)
  expect_equal(sum, c(2.7, 0, 0, 2.4))

  res <- dat |> dplyr::mutate(new = tl_sum_across(c(x, y), .scores)) |> dplyr::pull(new)
  expect_true(is.matrix(res))

  res <- dat |> dplyr::mutate(new = tl_mean_across(c(x, y))) |> dplyr::pull(new)
  expect_s3_class(res, "treelabel")
})
