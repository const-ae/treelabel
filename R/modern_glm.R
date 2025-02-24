
modern_glm <- function(y, design = ~ 1, family, col_data = NULL, offset = 0, allow_underdispersion = NULL,
                       ridge_penalty = 1e-5){
  if(inherits(y, "formula")){
    if(length(design) != 2 || design != ~ 1){
      stop("If the first argument is already a formula, the second argument must not be set. Please call this function like this:\n",
           "'modern_glm(y, design = ~ a + b + c, ...)'", call. = FALSE)
    }
    extr <- glmGamPoi:::extract_data_from_formula(y, col_data, parent.frame())
    y <- extr$data
    design <- extr$design
  }

  fam <- handle_family_arg(family)

  attr(design, "ignore_degeneracy") <- TRUE
  design <- handle_design_parameter(design, matrix(nrow = 0, ncol = if(is.matrix(y)) nrow(y) else length(y)), col_data)
  X <- design$design_matrix
  rank_original <- qr(X)$rank
  if(! is.null(design$offset)){
    offset <- offset + design$offset
  }
  if(length(offset) != 1 && length(offset) != nrow(X)){
    stop("'offset' must be either a vector of length 1 or match the number of rows in the design matrix.")
  }else if(length(offset) == 1){
    offset <- rep(offset, nrow(X))
  }

  # Add Ridge penalty. Need to remove ridge penalty later in dispersion calculation
  k <- ncol(X)
  n <- nrow(X)
  lambda <- glmGamPoi:::handle_ridge_penalty_parameter(ridge_penalty, X)
  if(! is.matrix(lambda)){
    lambda <- diag(lambda, nrow = k)
  }

  target <- fam$family$linkinv(attr(lambda, "target") %||% rep(0, k))
  X_padded <- rbind(X, lambda)
  y_padded <- if(is.matrix(y)){
    rbind(y, cbind(target, 1 - target))
  }else{
    c(y, target)
  }
  offset_padded <- c(offset, rep(0, k))

  if(is.matrix(y_padded)){
    # The regularization is noticed as non-integer counts.
    suppressWarnings({
      fit_list <- glm.fit(y = y_padded, x = X_padded, offset = offset_padded, family = fam$family)
    })
  }else{
    fit_list <- glm.fit(y = y_padded, x = X_padded, offset = offset_padded, family = fam$family)
  }
  fit <- list(coefficients = fit_list$coefficients,
       qr = fit_list$qr)
  # fit <- tryCatch({
  #   fit_list <- glm.fit(y = y_padded, x =X_padded, offset = offset_padded, family = fam$family)
  #   list(coefficients = fit_list$coefficients,
  #        qr = fit_list$qr)
  # }, error = function(err){
  #   stop(err)
  # })

  eta <- drop(X %*% fit$coefficients) + offset
  mu.eta.val <- fam$family$mu.eta(eta)
  mu <- fam$family$linkinv(eta)

  fit$weights <- mu.eta.val^2/fam$family$variance(mu)
  fit$residuals <- (y - mu)/mu.eta.val
  fit$df.residual <- nrow(X) - qr(X)$rank
  fit$fitted.values <- mu
  fit$formula <- design$design_formula
  fit$design_matrix <- design$design_matrix
  fit$allow_underdispersion <- allow_underdispersion
  fit$family <- fam
  class(fit) <- "modern_glm"
  fit
}

handle_family_arg <- function(family){
  if(inherits(family, "modern_glm_family")){
    return(family)
  }
  if(is.null(family)){
    warning("'family' parameter is NULL. Defaulting to 'gaussian'")
    family <- "gaussian"
  }
  if(is.character(family)){
    family <- rlang::as_function(family[1])
  }

  if(is.function(family)) family <- family()
  if(is.null(family$family)) {
    print(family)
    stop("'family' not recognized")
  }
  family_name <- family$family
  is_quasi <- grepl("quasi", family_name) || family_name == "gaussian"

  structure(list(family = family, family_name = family_name, is_quasi = is_quasi),
            class = c("modern_glm_family"))
}



test_diff <- function(fit, contrast){

  fam <- handle_family_arg(fit$family %||% family(fit))


  if(is.null(fit$design_matrix)){
    fit$design_matrix <- model.matrix(fit)
  }
  if(is.null(fit$qr)){
    fit$qr <- qr(fit)
  }
  if(is.null(fit$weights)){
    fit$weights <- 1
  }
  if(is.null(fit$residuals)){
    fit$residuals <- residuals(fit)
  }
  if(is.null(fit$df.residual)){
    fit$df.residual <- df.residual(fit)
  }
  if(is.null(fit$formula)){
    tryCatch({
      fit$formula <- formula(fit)
    }, error = function(err){
      # Do nothing
    })
  }
  if(! is.null(fit$formula) && ! is.null(fit$xlevels)){
    attr(fit$formula, "xlevels") <- fit$xlevels
  }
  if(! is.null(fit$formula) && ! is.null(fit$contrasts)){
    attr(fit$formula, "contrasts") <- fit$contrasts
  }


  cntrst <- parse_contrast({{contrast}}, coefficient_names = colnames(fit$design_matrix), formula = fit$formula)
  stopifnot(length(cntrst) == ncol(fit$design_matrix))
  df_resid <- fit$df.residual
  dispersion <- if(fam$is_quasi){
    x <- sum(fit$weights * fit$residuals^2)/df_resid
    default_allow_underdispersion <- fam$family_name == "gaussian"
    if(isTRUE(fit$allow_underdispersion) || default_allow_underdispersion){
      x
    }else{
      pmax(1, x)
    }
  }else{
    1
  }

  Qr <- fit$qr
  p1 <- seq_len(Qr$rank)
  coef.p <- fit$coefficients[Qr$pivot[p1]]
  covmat <- dispersion *  chol2inv(Qr$qr[p1, p1, drop = FALSE])

  lfc <- drop(coef.p %*% cntrst[Qr$pivot[p1]])
  lfc_se <- drop(sqrt(cntrst[Qr$pivot[p1]] %*% covmat %*% cntrst[Qr$pivot[p1]]))
  if(! is.na(lfc) && ! is.na(lfc_se) && (lfc_se + 0.5) / (abs(lfc) + 0.5) > 200){
    # The error is so large, let's just set it to Inf
    lfc_se <- Inf
  }
  pval <- if(fam$is_quasi){
    2 * pmin(pt(lfc / lfc_se, df = df_resid, lower.tail = TRUE),
             pt(lfc / lfc_se, df = df_resid, lower.tail = FALSE))
  }else{
    2 * pmin(pnorm(lfc / lfc_se, lower.tail = TRUE),
             pnorm(lfc / lfc_se, lower.tail = FALSE))
  }

  list(delta = lfc, delta_se = lfc_se, pval = pval, dispersion = dispersion)
}


parse_contrast <- function(contrast, coefficient_names, formula = NULL) {
  if(missing(contrast)){
    stop("No contrast argument was provided! The option is any linear combination of:\n",
         paste0(coefficient_names, collapse = ", "))
  }
  cnt_capture <- rlang::enquo(contrast)

  coefficient_names <- coefficient_names %||% character(0L)
  if(is.factor(coefficient_names)){
    coefficient_names <- levels(coefficient_names)
  }else if(! is.character(coefficient_names)){
    stop("levels must be either a character vector or a factor")
  }

  indicators <- diag(nrow=length(coefficient_names))
  rownames(indicators) <- coefficient_names
  colnames(indicators) <- coefficient_names

  covar_indicators <- list()
  for(lvl in coefficient_names){
    if(lvl != ""){
      ind <- indicators[, lvl]
      names(ind) <- coefficient_names
      covar_indicators[[lvl]] <- ind
    }
  }
  top <- rlang::new_environment(list(
    cond = function(...){
      .cond(formula, rlang::dots_list(...))
    }))
  bottom <- rlang::new_environment(covar_indicators, parent = top)
  data_mask <- rlang::new_data_mask(bottom = bottom, top = top)
  data_mask$.cntrst <- rlang::as_data_pronoun(bottom)
  tryCatch({
    res <- rlang::eval_tidy(cnt_capture, data = data_mask)
    if(! is.numeric(res)){
      if(is.character(res)){
        # If contrast was a string, eval will just spit it out the same way
        res <- rlang::eval_tidy(rlang::parse_expr(res), data = data_mask)
      }
    }
  }, error = function(e){
    # Try to extract text from error message
    match <- regmatches(e$message, regexec("object '(.+)' not found", e$message))[[1]]
    if(length(match) == 2){
      stop("Object '", match[2], "' not found. Allowed variables in contrast are:\n",
           paste0(coefficient_names, collapse = ", "), call. = FALSE)
    }else{
      stop(e$message)
    }
  })
  res
}



.cond <- function(formula, level_sets = list()){
  if(is.null(formula)){
    stop("You called 'cond()' inside the contrast, however the original model ",
         "was not specified with a formula. Thus 'cond()' doesn't work and you ",
         "need to specify the contrast using the column names of the design matrix.")
  }
  if(any(names(level_sets) == "")){
    stop("All arguments to 'cond()' must be named.")
  }
  if(any(duplicated(names(level_sets)))){
    stop("All arguments to 'cond()' must be unique.")
  }
  xlevels <- attr(formula, "xlevels")
  if(is.null(xlevels)){
    stop("No 'xlevels' available. The fit object must contain xlevels for 'cond()' to be used")
  }
  formula <- update(formula, NULL ~ .)

  covar <- all.vars(formula)
  new_dat <- as.list(rep(0, length(covar)))
  names(new_dat) <- covar
  for(n in covar){
    if(n %in% names(xlevels)){
      new_dat[[n]] <- factor(xlevels[[n]][1], levels = xlevels[[n]])
      if(nlevels(new_dat[[n]]) == 1){
        attr(new_dat[[n]], "contrasts") <- matrix(0, nrow = 1, ncol = 1)
      }
    }
  }
  for(n in names(level_sets)){
    if(! n %in% names(new_dat)){
      stop("Setting the level of '", n, "' failed. You can only set the level of the following variables: ", paste0(covar, collapse = ", "))
    }
    if(length(level_sets[[n]]) != 1){
      stop("Each argument to 'cond()' must be length one. '", n, "' has length ", length(level_sets[[n]]))
    }
    if(n %in% names(xlevels)){
      if(! level_sets[[n]] %in% xlevels[[n]]){
        stop("You are trying to set '", n, "=", level_sets[[n]], "'. However only the following values for ", n,
             " are valid: ", toString(xlevels[[n]], width = 80))
      }
      new_dat[[n]] <- factor(level_sets[[n]], levels = xlevels[[n]])
      if(nlevels(new_dat[[n]]) == 1){
        attr(new_dat[[n]], "contrasts") <- matrix(0, nrow = 1, ncol = 1)
      }
    }else{
      new_dat[[n]] <- level_sets[[n]]
    }
  }
  res <- drop(model.matrix(formula, new_dat, contrasts.arg = attr(formula, "contrasts")))
  attr(res, "assign") <- NULL
  attr(res, "contrasts") <- NULL
  res
}




handle_design_parameter <- function(design, data, col_data, verbose = FALSE){
  n_samples <- ncol(data)

  ignore_degeneracy <- isTRUE(attr(design, "ignore_degeneracy"))

  # Handle the design parameter
  if(is.matrix(design)){
    design_matrix <- design
    design_formula <- NULL
    offset <- 0
  }else if((is.vector(design) || is.factor(design))){
    if(length(design) != n_samples){
      if(length(design) == 1 && design == 1){
        stop("The specified design vector length (", length(design), ") does not match ",
             "the number of samples: ", n_samples, "\n",
             "Did you maybe mean: `design = ~ 1`?")
      }else{
        stop("The specified design vector length (", length(design), ") does not match ",
             "the number of samples: ", n_samples)
      }
    }
    tmp <- glmGamPoi:::convert_chr_vec_to_model_matrix(design, NULL)
    design_matrix <- tmp$model_matrix
    design_formula <- NULL
  }else if(inherits(design,"formula")){
    tmp <- convert_formula_to_design_matrix(design, col_data)
    design_matrix <- tmp$design_matrix
    design_formula <- tmp$formula
    offset <- tmp$offset
    col_data <- add_global_variables_to_col_data(design, col_data)
    attr(design_formula, "constructed_from") <- "formula"
  }else{
    stop("design argment of class ", class(design), " is not supported. Please ",
         "specify a `design_matrix`, a `character vector`, or a `formula`.")
  }

  if(nrow(design_matrix) != ncol(data)) stop("Number of rows in col_data does not match number of columns of data.")
  if(! is.null(rownames(design_matrix)) &&
     ! all(rownames(design_matrix) == as.character(seq_len(nrow(design_matrix)))) && # That's the default rownames
     ! is.null(colnames(data))){
    if(! all(rownames(design_matrix) == colnames(data))){
      if(setequal(rownames(design_matrix), colnames(data))){
        # Rearrange the rows to match the columns of data
        design_matrix <- design_matrix[colnames(data), ,drop=FALSE]
      }else{
        stop("The rownames of the design_matrix / col_data do not match the column names of data.")
      }
    }
  }

  if(any(matrixStats::rowAnyNAs(design_matrix))){
    stop("The design matrix contains 'NA's for sample ",
         paste0(head(which(matrixStats::rowAnyNAs(design_matrix))), collapse = ", "),
         ". Please remove them before you call 'lemur()'.")
  }

  if(ncol(design_matrix) >= n_samples && ! ignore_degeneracy){
    stop("The design_matrix has more columns (", ncol(design_matrix),
         ") than the there are samples in the data matrix (", n_samples, " columns).\n",
         "Too few replicates / too many coefficients to fit model.\n",
         "The head of the design matrix: \n", glmGamPoi:::format_matrix(head(design_matrix, n = 3)))
  }

  if(verbose && is.null(design_formula)){
    message("The 'design' was not specified with a formula. This means that you cannot use 'cond(...)' in 'test_de(...)'.")
  }

  # Check rank of design_matrix
  qr_mm <- qr(design_matrix)
  if(qr_mm$rank < ncol(design_matrix) && n_samples > 0  && ! ignore_degeneracy){
    is_zero_column <- matrixStats::colCounts(design_matrix, value = 0) == nrow(design_matrix)
    if(any(is_zero_column)){
      stop("The model matrix seems degenerate ('matrix_rank(design_matrix) < ncol(design_matrix)'). ",
           "Column ", paste0(head(which(is_zero_column), n=10), collapse = ", "), " contains only zeros. \n",
           "The head of the design matrix: \n", glmGamPoi:::format_matrix(head(design_matrix, n = 3)))
    }else{
      stop("The model matrix seems degenerate ('matrix_rank(design_matrix) < ncol(design_matrix)'). ",
           "Some columns are perfectly collinear. Did you maybe include the same coefficient twice?\n",
           "The head of the design matrix: \n", glmGamPoi:::format_matrix(head(design_matrix, n = 3)))
    }
  }

  rownames(design_matrix) <- colnames(data)
  validate_design_matrix(design_matrix, data)
  list(design_matrix = design_matrix, design_formula = design_formula, col_data = col_data, offset = offset)
}



convert_formula_to_design_matrix <- function(formula, col_data){
  attr(col_data, "na.action") <- "na.pass"
  for(col in seq_len(ncol(col_data))){
    if(is.character(col_data[[col]]) && nlevels(as.factor(col_data[[col]])) == 1){
      col_data[[col]] <- as.factor(col_data[[col]])
      attr(col_data[[col]], "contrasts") <- matrix(0, nrow = nrow(col_data))
    }else if(is.factor(col_data[[col]]) && nlevels(as.factor(col_data[[col]])) == 1){
      attr(col_data[[col]], "contrasts") <- matrix(0, nrow = nrow(col_data))
    }
  }

  tryCatch({
    mf <- model.frame(formula, data = col_data, drop.unused.levels = FALSE)
    offset <- as.vector(model.offset(mf))
    terms <- attr(mf, "terms")
    # xlevels is used for reconstructing the model matrix
    attr(terms, "xlevels") <- stats::.getXlevels(terms, mf)
    # vars_xlevels is used to check input to cond(...)
    attr(terms, "vars_xlevels") <- xlevels_for_formula_vars(terms, col_data)
    mm <- stats::model.matrix.default(terms, mf)
    attr(terms, "contrasts") <- attr(mm, "contrasts")
  }, error = function(e){
    # Try to extract text from error message
    match <- regmatches(e$message, regexec("object '(.+)' not found", e$message))[[1]]
    if(length(match) == 2){
      stop("Problem parsing the formula (", formula, ").\n",
           "Variable '", match[2], "' not found in col_data or global environment. Possible variables are:\n",
           paste0(colnames(col_data), collapse = ", "), call. = FALSE)
    }else{
      stop(e$message)
    }
  })

  # Otherwise every copy of the model stores the whole global environment!
  attr(terms, ".Environment") <- c()
  colnames(mm)[colnames(mm) == "(Intercept)"] <- "Intercept"
  list(formula = terms, design_matrix = mm, offset = offset)
}

add_global_variables_to_col_data <- function(formula, col_data){
  # Check if var is global and put it into col_data
  formula_env <- attr(formula, ".Environment")
  if(is.null(formula_env)) formula_env <- rlang::empty_env()
  for(gv in setdiff(all.vars(formula), colnames(col_data))){
    value <- rlang::eval_tidy(rlang::sym(gv), data = NULL, env = formula_env)
    is_vector_type <- (is(col_data, "DFrame") && is(value, "Vector")) || vctrs::obj_is_vector(value)
    if(is_vector_type){
      has_correct_length <- NROW(value) == 1 || NROW(value) == nrow(col_data)
      if(! has_correct_length){
        stop("Trying store global variables from formula in colData, however '", gv, "' ",
             "has length ", NROW(value), ", but it needs to be 1 or nrow(col_data) (", nrow(col_data), ").")
      }
    }else{
      stop("Trying store global variables from formula in colData, however '", gv, "' ",
           "is of type ",  class(value)[1]," and not a vector-type.")
    }
    col_data[[gv]] <- value
  }
  col_data
}

xlevels_for_formula_vars <- function(formula, data){
  # if(! is.null( attr(formula, "xlevels"))){
  #   attr(formula, "xlevels")
  if(! is.null( attr(formula, "vars_xlevels"))){
    attr(formula, "vars_xlevels")
  }else{
    # For all character / factor vars get xlevel
    all_vars <- all.vars(formula)
    formula_env <- attr(formula, ".Environment")
    if(is.null(formula_env)) formula_env <- rlang::empty_env()
    xlev <- lapply(all_vars, \(v){
      value <- rlang::eval_tidy(rlang::sym(v), data = as.data.frame(data), env = formula_env)
      if(is.character(value)){
        levels(as.factor(value))
      }else if(is.factor(value)){
        levels(value)
      }else{
        NULL
      }
    })
    names(xlev) <- all_vars
    xlev[!vapply(xlev, is.null, TRUE)]
  }
}

validate_design_matrix <- function(matrix, data){
  stopifnot(is.matrix(matrix))
  stopifnot(nrow(matrix) == ncol(data))
}

