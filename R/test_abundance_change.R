
#' Make `vars` available
#'
#' @param ... Variables or expression are automatically quoted.
#'
#' See `ggplot2::vars` or `rlang::quos` for more information
#'
#' @export
vars <- function(...){
  rlang::quos(...)
}


#' Test if the abundance of a label changes between conditions
#'
#' @param data an object that can be converted to a `data.frame`, which contains one row for each cell
#'   with meta data:
#'   * one or more columns with the condition information (used in `design`),
#'   * one or more columns with the sample information which describes our replicates (used in `aggregate_by`),
#'   * one or more `treelabel` columns with the cell type annotation (used in `treelabels`).
#' @param design a formula or design matrix that describes the experimental design (e.g., `design = ~ status`).
#' @param aggregate_by a <[`tidy-select`][dplyr_tidy_select]> expression which defines the independent unit of replication for this
#'   experiment (e.g., `aggregate_by = c(sample_id)`).
#' @param contrast a contrast expression that defines the conditions across which we compare the abundance
#'   changes. This can be a contrast vector (i.e., `c(0,1,-1,0)`) or a contrast expression that can refer
#'   to levels of covariates mentioned in the `design` (`cond(status = "treated") - cond(status = "control")`).
#'   Default: `NULL` in which case the first two levels of the first factor level from the design are compared.
#' @param treelabels a <[`tidy-select`][dplyr_tidy_select]> expression that selects the column or columns with the cell type annotation.
#'   Default: `where(is_treelabel)` which selects all column of class `treelabel`.
#' @param targets a <[`data-masking`][rlang::args_data_masking]> expression vector (wrapped with `vars`) or a character vector that defines which
#'   cell type labels are counted. If the `treelabel` contains confidence scores, you should convert them to
#'   booleans either before calling `test_abundance_changes` or using `targets = vars(Macrophage > 0.7, T_Cell > 0.6)`.
#'   Default: `NULL` in which case all labels below the `reference` label are tested.
#' @param reference a single <[`data-masking`][rlang::args_data_masking]> expression (not wrapped with `vars`!) or a character vector that defines
#'   which cell label is used as a reference. For example, if the `treelabel` describes immune cells with two annotation levels, where
#'   the first level is _Myeloid_ and _Lymphoid_ cells and the second level for the Myeloid cells is _Monocytes_ and _Macrophages_,
#'   we could count the abundance change of _Macrophages_ as a fraction of all cells (i.e., `reference = "root"`) or as a fraction of
#'   _Myeloid_ cells (i.e., `reference = Myeloid > 0.7`).
#'
#'   Default: `NULL` in which case the root of the `treelabel` is used as a reference.
#' @param model the statistical model that is used to test for abundance changes. I recommend using a quasi-poisson or quasi-binomial
#'   model to account for the count overdispersion. Default: `"quasipoisson"`.
#' @param ridge_penalty a small penalty added to the fit to aid convergence. Default: `0.001`.
#' @param return_aggregated_data see _Returns_ section.
#'
#' @return By default, the function returns a `tibble` with one row per statistical test that was conducted with the following columns
#'   * `treelabel`: the name of the selected `treelabel` column.
#'   * `target`: the name of the cell type label for which we test the abundance change.
#'   * `LFC`/`LogOdds`/`Delta`: the magnitude of the change. The name and interpretation of the column depends on the model family.
#'   * `LFC_se`/`LogOdds_se`/`Delta_se`: the standard error of the estimate.
#'   * `dispersion`: the variance of the residuals around the estimate.
#'   * `pval`: the significance of the estimate.
#'   * `adj_pval`: Benjamini-Hochberg adjusted p-values (i.e., the false discovery rate (FDR))
#'
#'  If `return_aggregated_data = TRUE`, the function returns a tibble that contains the aggregated data that would usually
#'  be used to fit the statistical models.
#'
#' @examples
#'   tree <- igraph::graph_from_literal(
#'     Animal - Bird : Mammal,
#'     Bird - Parrot : Eagle,
#'     Mammal - Dog : Cat
#'   )
#'   animal <- c("Parrot", "Eagle", "Dog", "Cat")
#'   df <- rbind(
#'   data.frame(group = "A",
#'              sample = sample(1:4, size = 1000, replace = TRUE),
#'              animal = sample(animal, size = 1000, prob = c(0.4, 0.25, 0.25, 0.1), replace = TRUE)),
#'   data.frame(group = "B",
#'              sample = sample(1:4, size = 1000, prob = c(0.25, 0.25, 0.25, 0.25), replace = TRUE),
#'              animal = sample(animal, size = 1000, replace = TRUE))
#'   )
#'
#'   df$tl <- treelabel(df$animal, tree = tree, tree_root = "Animal")
#'   res <- test_abundance_changes(df, design = ~ group, aggregate_by = c(group, sample),
#'                                 targets = vars(Dog, Cat), reference = Animal, model = "poisson")
#'   res
#'
#'   # The underlying data: This can be useful for plotting or sanity checks.
#'   test_abundance_changes(df, design = ~ group, aggregate_by = c(group, sample),
#'                          targets = vars(Dog, Cat), reference = Animal,
#'                          return_aggregated_data = TRUE)
#'
#' @importFrom tibble tibble
#' @export
test_abundance_changes <- function(data, design, aggregate_by, contrast = NULL,
                                  treelabels = tidyselect::where(is_treelabel), targets = NULL, reference = NULL,
                                  model = c("quasipoisson", "poisson", "quasibinomial", "binomial"),
                                  ridge_penalty = 1e-3, return_aggregated_data = FALSE){
  # I am using dplyr stuff, so it needs to be compatible
  data <- as.data.frame(data)

  # Throw some helpful errors
  if(missing(design)){
    stop("You must specify the 'design' argument. This should be a formula that defines what you are testing.\n",
         "For example, if want to know if a cell type is more prevalent in the treated condition than the control, ",
        "you would say 'design = ~ treatment', where 'treatment' is a column in 'data' which contains the treatment ",
        "status for each cell.")
  }
  if(missing(aggregate_by)){
    stop("You must specify the 'aggregate_by' argument. This is a <tidyselect> expression that defines ",
         "which columns are used to for aggregation.\n",
        "For example, if you want to compare compare the abundance changes of a cell type and you have data for ",
        "ten patients, you would say 'aggregate_by = patient_id' where 'patient_id' is a column in 'data' that ",
        "contains the identifier of the patient each cell orginates from.")
  }

  # Handle family argument
  fam <- handle_family_arg(model)
  model_name <- fam$family_name
  if(model_name %in% c("poisson", "quasipoisson")){
    output_name <- "LFC"
  }else if(model_name %in% c("binomial", "quasibinomial")){
    output_name <- "LogOdds"
  }else{
    output_name <- "Delta"
  }

  additional_vars <- intersect(all.vars(design), colnames(data))
  aggr_dat <- sum_treelabels_in_dataframe(data, aggregate_by = c({{aggregate_by}}, tidyselect::all_of(additional_vars)),
                                          treelabels = {{treelabels}}, targets = {{targets}}, reference = {{reference}})


  has_warned <- FALSE
  printed_default_contrast <- FALSE
  tl_sel <- tidyselect::eval_select(rlang::enquo(treelabels), aggr_dat)
  if(length(tl_sel) == 0){
    warning("No `treelabel` column selected. Please check `data` and `treelabels` arguments.")
  }
  res <- list()
  for(tl in tl_sel){
    tl_name <- names(tl_sel)[which(tl_sel == tl)[1]]
    tl_vec <- aggr_dat[[tl]]
    ..ref.. <- tl_get(tl_vec, tl_tree_root(tl_vec))
    working_dat <- aggr_dat[..ref.. > 0,]
    ..ref.. <- ..ref..[..ref.. > 0]
    label_names <- setdiff(colnames(tl_score_matrix(tl_vec)), tl_tree_root(tl_vec))
    failed_labs <- c()
    for(labs in label_names){
      if(nrow(working_dat) == 0){
        if(! return_aggregated_data){
          res <- append(res, list(tibble(treelabel = tl_name, target = labs,
                                         "{output_name}" := NA_real_, "{output_name}_se" := NA_real_,
                                         dispersion = NA_real_, pval = NA_real_)))
        }
        next;
      }
      ..lab.. <- tl_get(working_dat[[tl]], labs)
      if(return_aggregated_data){
        working_dat$reference <- tl_name
        working_dat$reference_count <- ..ref..
        working_dat$target <- labs
        working_dat$target_count <- ..lab..
        res <- append(res, list(tibble::as_tibble(working_dat)))
        next
      }
      if(all(..lab.. == 0)){
        res <- append(res, list(tibble(treelabel = tl_name, target = labs,
                                       "{output_name}" := NA_real_, "{output_name}_se" := NA_real_,
                                       dispersion = NA_real_, pval = NA_real_)))
        next;
      }
      if(! has_warned && any(..lab.. > ..ref..)){
        problematic_elem <- which(..lab.. > ..ref..)[1]
        warning("Element ", problematic_elem, " of `", labs, "` from treelabel vector `", tl_name,
                "` is larger than the reference element (", ..lab..[problematic_elem], " vs. ", ..ref..[problematic_elem], "). ",
                "This can indicate a serious problem.\n")
        has_warned <- TRUE
      }

      if(model_name %in% c("poisson", "quasipoisson")){
        y <- ..lab..
        offset <- log(..ref..)
      }else if(model_name %in% c("binomial", "quasibinomial")){
        y <- cbind(..lab.., ..ref.. - ..lab..)
        offset <- rep(0, length(..ref..))
      }else{
        y <- ..lab.. / ..ref..
        offset <- 0
      }

      failed <- FALSE
      tryCatch({
        fit <- modern_glm(y, design = design, offset = offset, family = model,
                          col_data = aggr_dat, ridge_penalty = ridge_penalty)
      }, error = function(err){
        failed_labs <<- c(failed_labs, labs)
        failed <<- TRUE
      })
      if(failed){
          res <- append(res, list(tibble(treelabel = tl_name, target = labs,
                                         "{output_name}" := NA_real_ , "{output_name}_se" := NA_real_,
                                         dispersion = NA_real_, pval = NA_real_)))
          next
      }
      contrast_capture <- rlang::enquo(contrast)
      if(rlang::quo_is_null(contrast_capture)){
        if(is.null(fit$formula)) stop("Cannot automatically infer the contrast. Please specify it explicitly.")
        xlevels <- attr(fit$formula, "xlevels")
        if(length(xlevels) == 0 || length(xlevels[[1]]) <= 1){
          stop("Could not automatically construct contrast. Please specify it explicitly.")
        }
        first_lvl <- names(xlevels)[1]
        if(! printed_default_contrast){
          message("Default contrast is: `cond(", first_lvl, " = '", xlevels[[1]][2], "') - cond(", first_lvl, " = '",  xlevels[[1]][1], "')`")
          printed_default_contrast <- TRUE
        }
        contrast_capture <- rlang::quo(
          cond(!!first_lvl := xlevels[[1]][2]) - cond(!!first_lvl := xlevels[[1]][1])
        )
      }
      de <- test_diff(fit, !!contrast_capture)
      res <- append(res, list(tibble(treelabel = tl_name, target = labs,
                                     "{output_name}" := de$delta , "{output_name}_se" := de$delta_se,
                                     dispersion = de$dispersion, pval = de$pval)))
    }
    if(length(failed_labs) > 0){
      message("Fit failed in ", tl_name,  " for targets: ", toString(failed_labs, width=60))
    }
  }
  res <- vctrs::vec_rbind(!!! res)
  if(! return_aggregated_data){
    res$adj_pval <- p.adjust(res$pval, method = "BH")
  }
  res
}


sum_treelabels_in_dataframe <- function(data, aggregate_by, treelabels, targets = NULL, reference = NULL){
  stopifnot(is.data.frame(data))
  if(! rlang::quo_is_null(rlang::enquo(targets))){
    if(rlang::is_quosures(targets)){
      quo_targets <- targets
    }else if(is.character(targets)){
      quo_targets <- lapply(targets, rlang::sym)
    }else{
      stop("Cannot handle targets of type ", rlang::type_of(labels))
    }
    quo_targets <- rlang::exprs_auto_name(quo_targets)
  }else{
    quo_targets <- NULL
  }

  quo_ref <- rlang::enquo(reference)
  aggr_dat <- data |>
    dplyr::mutate(dplyr::across({{treelabels}}, \(x){
      if(! is_treelabel(x)){
        stop("Column selected by 'treelabels' argument (",dplyr::cur_column(), ") is not a treelabel vector.")
      }
      if(rlang::quo_is_null(quo_ref)){
        ref_name <- tl_tree_root(x)
        ref_vec <- tl_get(x, ref_name)
      }else{
        ref_name <- unique(all.vars(quo_ref))
        ref_vec <- tl_eval(x, !!quo_ref)
      }
      if(is.null(quo_targets)){
        if(length(ref_name) != 1){
          stop("`reference` must contain exactly one reference. It contains: ", toString(ref_name, width = 60))
        }
        tl_tree_cut(x, ref_name)
      }else{
        lab_mat <- tl_eval(x, cbind(!!! quo_targets), check_bounds = FALSE)

        new_tree <- igraph::graph_from_edgelist(cbind("ref", names(quo_targets)))
        treelabel(cbind(ref = ref_vec, lab_mat), new_tree, "ref")
      }
    })) |>
    dplyr::summarize(dplyr::across({{treelabels}}, \(x) sum(x, na.rm=TRUE)), .by = {{aggregate_by}})
  aggr_dat
}



