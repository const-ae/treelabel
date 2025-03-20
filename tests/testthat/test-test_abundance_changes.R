test_that("aggregation works as expected", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )
  animal <- c("Parrot", "Eagle", "Dog", "Cat")
  df <- rbind(
    data.frame(group = "A",
               sample = sample(1:4, size = 100, replace = TRUE),
               animal = sample(animal, size = 1000, prob = c(0.4, 0.3, 0.2, 0.1), replace = TRUE)),
    data.frame(group = "B",
               sample = sample(1:4, size = 100, replace = TRUE),
               animal = sample(animal, size = 1000, replace = TRUE))
  )

  df$tl <- treelabel(df$animal, tree = tree, tree_root = "Animal")

  # Cannot select column that is not a treelabel
  expect_error(
    sum_treelabels_in_dataframe(data = df, aggregate_by = c(group, sample), treelabels = group)
  )

  res <- sum_treelabels_in_dataframe(data = df, aggregate_by = c(group, sample), treelabels = where(is_treelabel))
  expect_setequal(res$sample, 1:4)
  expect_equal(sum(res$group == "A"), 4)
  expect_equal(sum(res$group == "B"), 4)
  expect_s3_class(res$tl, "treelabel")
  manual_count <- tapply(df$animal, paste0(df$group, "-", df$sample), \(x){
    sum(x == "Parrot")
  })
  expect_setequal(tl_get(res$tl, "Parrot"), manual_count)

  res2 <- sum_treelabels_in_dataframe(data = df, aggregate_by = c(group, sample), treelabels = where(is_treelabel), reference = Animal)
  expect_equal(res2, res, ignore_attr = "tree")
})


test_that("test_abundance_change works as expected", {
  tree <- igraph::graph_from_literal(
    Animal - Bird : Mammal,
    Bird - Parrot : Eagle,
    Mammal - Dog : Cat
  )
  animal <- c("Parrot", "Eagle", "Dog", "Cat")
  df <- rbind(
    data.frame(group = "A",
               sample = sample(1:4, size = 100, replace = TRUE),
               animal = sample(animal, size = 1000, prob = c(0.4, 0.3, 0.2, 0.1), replace = TRUE)),
    data.frame(group = "B",
               sample = sample(1:4, size = 100, replace = TRUE),
               animal = sample(animal, size = 1000, replace = TRUE))
  )

  df$tl <- treelabel(df$animal, tree = tree, tree_root = "Animal")

  # This prints a message about the default contrast
  suppressMessages({
    res <- test_abundance_changes(df, design = ~ group, aggregate_by = c(group, sample),
                                  targets = vars(Dog, Cat), reference = Animal, model = "poisson")
  })
  expect_equal(nrow(res), 2)
  expect_equal(res$treelabel, c("tl", "tl"))
  expect_setequal(res$target, c("Dog", "Cat"))

  ref_dat <- test_abundance_changes(df, design = ~ group, aggregate_by = c(group, sample),
                                    reference = Animal, return_aggregated_data = TRUE)
  fit <- glm(target_count ~ group + offset(log(reference_count)), data = ref_dat[ref_dat$target == "Cat",], family = "poisson")
  sum_ref <- summary(fit)
  expect_equal(
    unlist(res[res$target == "Cat",c("LFC", "LFC_se", "pval")]),
    sum_ref$coefficients[2,c("Estimate", "Std. Error", "Pr(>|z|)")],
    ignore_attr = "names", tolerance = 1e-4
  )
})

