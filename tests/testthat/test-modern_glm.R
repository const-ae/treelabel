test_that("modern_glm works like lm", {

  y <- rnorm(10)
  df <- data.frame(
    test = rep_len(c("a", "b"), length(y))
  )
  fit <- modern_glm(y, design = ~ test, family = gaussian(), col_data = df, ridge_penalty = 0)
  trad_lm_fit <- lm(y ~ test, data= df)
  sum_trad <- summary(trad_lm_fit)
  expect_equal(unlist(test_diff(fit, cond(test = "a"))),
               c(sum_trad$coefficients[1,c("Estimate", "Std. Error", "Pr(>|t|)")], sum_trad$sigma^2),
               ignore_attr = "names")
  expect_equal(test_diff(fit, cond(test = "a") - cond(test = "b")),
               test_diff(trad_lm_fit, cond(test = "a") - cond(test = "b")))

  # Play with ridge_penalty
  fit_tiny_penalty <- modern_glm(y, design = ~ test, family = gaussian(), col_data = df, ridge_penalty = 1e-6)
  expect_equal(test_diff(fit_tiny_penalty, cond(test = "a") - cond(test = "b")),
               test_diff(trad_lm_fit, cond(test = "a") - cond(test = "b")))

  # Handle redundancy in design matrix
  X <- model.matrix(~ test, data = df)
  fit2 <- modern_glm(y, design = cbind(X, 3), family = gaussian(), col_data = df, ridge_penalty = 1e-6)
  expect_equal(test_diff(fit2, c(0,1,0)),
               test_diff(trad_lm_fit, c(0,1)))

  # Handle full-zero columns
  fit3 <- modern_glm(y, design = cbind(X, 0, 0), family = gaussian(), col_data = df, ridge_penalty = 1e-6)
  expect_equal(test_diff(fit3, c(0,1,0,0)),
               test_diff(trad_lm_fit, c(0,1)))
  expect_equal(fit3$coefficients[3:4], c(0,0), ignore_attr = "names")
})


test_that("modern_glm works like glm", {
  y <- rnbinom(10, mu = 12, size = 1/0.3)
  df <- data.frame(
    test = rep_len(c("a", "b"), length(y)),
    cont = rnorm(10),
    total = round(rnorm(n = 10, mean = 50, sd  = 3))
  )
  y <- pmin(y, df$total)

  fit <- modern_glm(y, design = ~ test + cont + offset(log(total)), family = quasipoisson(),
                    col_data = df, ridge_penalty = 0, allow_underdispersion = TRUE)
  trad_glm_fit <- glm(y ~ test + cont + offset(log(total)), data= df, family = quasipoisson())

  sum_trad <- summary(trad_glm_fit)
  expect_equal(unlist(test_diff(fit, cond(test = "a"))),
               c(sum_trad$coefficients[1,c("Estimate", "Std. Error", "Pr(>|t|)")], sum_trad$dispersion),
               ignore_attr = "names", tolerance = 1e-3)
  expect_equal(test_diff(fit, cond(test = "a") - cond(test = "b")),
               test_diff(trad_glm_fit, cond(test = "a") - cond(test = "b")),
               tolerance = 1e-3)


  fit <- modern_glm(cbind(y, total - y) ~ test + cont, family = binomial(),
                    col_data = df, ridge_penalty = 0)
  trad_glm_fit <- glm(y/total ~ test + cont, data= df, weights = df$total, family = binomial())

  sum_trad <- summary(trad_glm_fit)
  expect_equal(unlist(test_diff(fit, cond(test = "a"))),
               c(sum_trad$coefficients[1,c("Estimate", "Std. Error", "Pr(>|z|)")], sum_trad$dispersion),
               ignore_attr = "names", tolerance = 1e-3)
  expect_equal(test_diff(fit, cond(test = "a") - cond(test = "b")),
               test_diff(trad_glm_fit, cond(test = "a") - cond(test = "b")),
               tolerance = 1e-3)

  # Handle redundancy in design matrix
  trad_glm_fit <- glm(y ~ test + offset(log(total)), data= df, family = poisson())
  X <- model.matrix(~ test, data = df)
  fit2 <- modern_glm(y, design = cbind(X, 3), offset = offset(log(df$total)), family = poisson(), col_data = df, ridge_penalty = 0)
  expect_equal(test_diff(fit2, c(0,1, 0)),
               test_diff(trad_glm_fit, cond(test = "b") - cond(test = "a")),
               tolerance = 1e-3)

  fit3 <- modern_glm(y, design = cbind(X, 0, 0), offset = offset(log(df$total)), family = poisson(), col_data = df, ridge_penalty = 1e-5)
  expect_equal(test_diff(fit3, c(0,1,0,0)),  test_diff(trad_glm_fit, c(0,1)))
  expect_equal(fit3$coefficients[3:4], c(0,0), ignore_attr = "names")
})


test_that("modern_glm can handle tricky inputs", {
  y = c(0, 0, 0, 0, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  col_dat = structure(list(stage = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 2L, 1L, 2L, 1L, 3L, 2L, 1L, 2L, 1L, 3L, 2L, 1L, 2L, 1L), levels = c("Normal", "Pre", "Tumor" ), class = "factor"),
                           cohort = structure(c(9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 9L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 10L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L, 11L),
                                             levels = c("E_MTAB_12305", "CRC_htan", "Eso_cxg", "GSE188900", "GSE181919", "GSE125449", "GSE136103", "GSE182159", "GSE136831", "GSE189357", "LUAD_htan", "LUSC_lab", "GSE229413", "STAD_cxg", "GSE134520", "standford", "GSE193304", "36368318"), class = "factor")),
                      class = "data.frame", row.names = c(NA, -41L))
  design = ~stage + cohort
  offset =  c(6.69950034016168, 7.60090245954208, 2.70805020110221, 8.04782935745784, 7.79688034278352, 8.88834286910928, 8.20467182895081, 7.67878899819915, 8.11072758297449, 8.08641027532378, 7.39387829010776, 7.26612877955645, 7.06731984865348, 7.76089319585102, 7.55066124310534, 6.99117688712121, 7.22256601882217, 5.51745289646471, 7.4151751096133, 8.42901750051251, 7.64156444126097, 8.16763571524637, 8.51599247083972, 8.25634777291802, 7.2841348061952, 8.99640430141289, 8.4986218058308, 7.54960916515453, 7.49886973397693, 7.31121838441963, 7.50604217851812, 7.69256964806791, 7.22402480828583, 6.69332366826995, 7.70391020961631, 7.94378269245863, 6.56667242980324, 8.12622252945853, 7.8935720735049, 6.5875500148248, 6.81454289725996)

  coef <- modern_glm(y, design = design, col_data = col_dat, offset = offset, family = "poisson", ridge_penalty = 1e-4)$coefficients
  unmentioned_coef <- coef[! names(coef) %in% paste0("cohort", unique(col_dat$cohort)) & grepl("^cohort", names(coef))]
  expect_equal(unmentioned_coef, rep(0, length(unmentioned_coef)), ignore_attr = "names")
})

test_that("modern_glm can handle single level factors", {
  y <- rnorm(10)
  df <- data.frame(x = rep("a", 10))
  res <- modern_glm(y, design = ~ x, family = "gaussian", col_data = df)
  expect_equal(res$coefficients, c(mean(y), 0), ignore_attr = "names")
})
