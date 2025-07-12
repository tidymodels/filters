#' Construct a subclassed score object for cross tabulation p-values with additional metadata
#'
#' Introduce new properties `neg_log10` and `fdr`.
#' Output a new score object that contains associated metadata, such as `range`,
#' `fallback_value`, `score_type`, `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#'
#' @param outcome_type A character string indicating the outcome type. One of:
#'  - `"factor"`
#' @param predictor_type A character string indicating the predictor type. One of:
#'  - `"factor"`
#' @param fallback_value A numeric scalar used as a fallback value. One of:
#'    - `0`
#'   - `Inf` (default)
#'
#' For p-values,
#' since the default applies a negative log10 transformation to p-values,
#' the `fallback_value` is `"Inf"`.
#'
#' @param score_type A character string indicating the type of scoring metric to compute.
#' One of:
#'    - `"pval_chisq"` (default)
#'    - `"pval_fisher"`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#'
#' For p-values,
#' since the default applies a negative log10 transformation to p-values,
#' the `direction` is `"maximize"`.
#'
#' @param neg_log10 A logical value indicating whether to apply a negative log10
#' transformation to p-values. One of:
#'  - `TRUE` (default)
#'  - `FALSE`
#'
#'  If `TRUE`, p-values are transformed as `-log10(pval)`. In this case:
#'  - The `fallback_value` is `Inf` (default)
#'  - The `direction` is `"maximize"` (default)
#'
#'  If `FALSE`, raw p-values are used. In this case:
#'  - The `fallback_value` needs to be set to `0`
#'  - The `direction` needs to be set to `"minimize"`
#'
#' @param fdr NULL
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type`, `direction`, and other relevant attributes.
#' @export
#'
#' @examples
#' # Create a score object
#' new_score_obj_cross_tab()
new_score_obj_cross_tab <- S7::new_class(
  "new_score_obj_cross_tab",
  parent = new_score_obj,
  properties = list(
    neg_log10 = S7::new_property(S7::class_logical, default = TRUE),
    fdr = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' Create a score object for cross tabulation p-values
#'
#' Construct a score object containing metadata for univariate feature scoring using
#' Fisher’s exact test and the chi-squared test.
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"pval_chisq"` or `"pval_fisher"`), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj_cross_tab
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"pval_chisq"` or `"pval_fisher"`), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples
#' # Create a score object
#' score_cross_tab()
#' # Change score type to use -log10(p-values) fisher's
#' score_cross_tab(score_type = "pval_fisher")
#' # Change score type to use raw p-values fisher's
#' score_cross_tab(
#'   score_type = "pval_fisher",
#'   neg_log10 = FALSE,
#'   direction = "minimize",
#'   fallback_value = 0
#' )
score_cross_tab <- function(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "pval_chisq",
  direction = "maximize",
  neg_log10 = TRUE,
  fdr = FALSE
) {
  new_score_obj_cross_tab(
    outcome_type = "factor",
    predictor_type = "factor",
    case_weights = FALSE,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = fallback_value,
    score_type = score_type,
    #trans = # Cannot set NULL. Otherwise S7 complains
    #sorts =
    direction = direction,
    deterministic = TRUE,
    tuning = FALSE,
    #ties =
    calculating_fn = function(x) {}, # Otherwise S7 complains
    neg_log10 = neg_log10,
    fdr = fdr,
    label = c(score_crosstab = "Cross tabulation p-values")
  )
}

get_single_chisq <- function(predictor, outcome) {
  tab <- table(predictor, outcome) # TODO sample(predictor); Check Slack 06-23
  res <- suppressWarnings(stats::chisq.test(tab)$p.value)
  return(res)
}

get_single_fisher <- function(predictor, outcome) {
  tab <- table(predictor, outcome) # TODO sample(predictor); Check Slack 06-23
  res <- suppressWarnings(stats::fisher.test(tab)$p.value)
  return(res)
}

map_score_cross_tab <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.numeric(outcome_col) || is.numeric(predictor_col)) {
    return(NA_real_)
  }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

make_scores_cross_tab <- function(score_type, score, outcome, predictors) {
  res <- tibble::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' Compute cross tabulation p-value scores using Fisher’s exact test and the chi-squared test
#'
#' Evaluate the relationship between a categorical outcome and a categorical predictor
#' by computing the cross tabulation p-value.
#' Output a tibble result with with one row per predictor, and four columns:
#' `name`, `score`, `predictor`, and `outcome`.
#'
#' @param score_obj A score object. See [score_cross_tab()] for details.
#'
#' @param data A data frame or tibble containing the outcome and predictor variables.
#' @param outcome A character string specifying the name of the outcome variable.
#' @param ... NULL
#'
#' @return A tibble of result with one row per predictor, and four columns:
#' - `name`: the name of scoring metric.
#' - `score`: the score for the predictor-outcome pair.
#' - `predictor`: the name of the predictor.
#' - `outcome`: the name of the outcome.
#'
#' @export
#'
#' @examples
#' library(titanic)
#' titanic_train <- titanic_train |>
#'   dplyr::mutate(dplyr::across(c(Survived, Pclass, Sex, Embarked), as.factor))
#' titanic_subset <- titanic_train |>
#'   dplyr::select(Survived, Pclass, Sex, Age, Fare, Embarked)
#' # Return score as -log10(p-values) for chi-squared
#' score_obj <- score_cross_tab()
#' score_res <- get_scores_cross_tab(
#'   score_obj,
#'   data = titanic_subset,
#'   outcome = "Survived"
#' )
#' score_res
#' # Return score as -log10(p-values) for fisher's
#' score_obj <- score_cross_tab(score_type = "pval_fisher")
#' score_res <- get_scores_cross_tab(
#'   score_obj,
#'   data = titanic_subset,
#'   outcome = "Survived"
#' )
#' score_res
#' # Return raw p-values instead of -log10(p-values) for chi-squared
#' score_obj <- score_cross_tab(
#'   score_type = "pval_chisq",
#'   neg_log10 = FALSE,
#'   direction = "minimize",
#'   fallback_value = 0
#' )
#' score_res <- get_scores_cross_tab(
#'   score_obj,
#'   data = titanic_subset,
#'   outcome = "Survived"
#' )
#' score_res
get_scores_cross_tab <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$fdr
) {
  if (score_obj@score_type == "pval_chisq") {
    score_obj@calculating_fn <- get_single_chisq
  } else if (score_obj@score_type == "pval_fisher") {
    score_obj@calculating_fn <- get_single_fisher
  }
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_cross_tab(data, x, outcome, score_obj@calculating_fn)
  )

  if (score_obj@fdr == TRUE) {
    score <- stats::p.adjust(score)
  }

  if (is.null(score_obj@neg_log10) || isTRUE(score_obj@neg_log10)) {
    score <- -log10(score)
  }

  res <- make_scores_cross_tab(score_obj@score_type, score, outcome, predictors)
  res
}
