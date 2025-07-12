#' Construct a subclassed score object for ANOVA F-test F-statistics and p-values with additional metadata
#'
#' Introduce a new properties `neg_log10`.
#' Output a new score object that contains associated metadata, such as `range`,
#' `fallback_value`, `score_type`, `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#'
#' @param fallback_value A numeric scalar used as a fallback value. One of:
#'    - `0`
#'   - `Inf` (default)
#'
#' For F-statistics, the `fallback_value` is `"Inf"`. For p-values,
#' since the default applies a negative log10 transformation to p-values,
#' the `fallback_value` is `"Inf"`.
#'
#' @param score_type A character string indicating the type of scoring metric to compute.
#' One of:
#'    - `"fstat"` (default)
#'    - `"pval"`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#'
#' For F-statistics, the `direction` is `"maximize"`. For p-values,
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
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type`, `direction`, and other relevant attributes.
#' @export
#'
#' @examples
#' # Create a score object
#' new_score_obj_aov()
new_score_obj_aov <- S7::new_class(
  "new_score_obj_aov",
  parent = new_score_obj,
  properties = list(
    neg_log10 = S7::new_property(S7::class_logical, default = TRUE)
  )
)

#' Create a score object for ANOVA F-test F-statistics and p-values
#'
#' Construct a score object containing metadata for univariate feature scoring using the
#' ANOVA F-test.
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"fstat"` or `"pval"`), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj_aov
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"fstat"` or `"pval"`), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples
#' # Create a score object
#' score_aov()
#' # Change score type to use -log10(p-values)
#' score_aov(score_type = "pval")
#' # Change score type to use raw p-values
#' score_aov(
#'   score_type = "pval",
#'   neg_log10 = FALSE,
#'   direction = "minimize",
#'   fallback_value = 0
#' )
score_aov <- function(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "fstat",
  direction = "maximize",
  neg_log10 = TRUE
) {
  new_score_obj_aov(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
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
    label = c(score_aov = "ANOVA F-test F-statistics and p-values")
  )
}

flip_if_needed_aov <- function(predictor, outcome) {
  # TODO Move to utilities.R
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = outcome, outcome = predictor)
  } else {
    list(predictor = predictor, outcome = outcome)
  }
}

get_single_f_stat <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  fit <- stats::lm(outcome ~ predictor)
  res <- stats::anova(fit)$`F value`[1] # summary(fit)$fstatistic[1] |> as.numeric()
  return(res)
}

get_single_p_val <- function(predictor, outcome) {
  flipped <- flip_if_needed_aov(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor

  fit <- stats::lm(outcome ~ predictor)
  res <- stats::anova(fit)$`Pr(>F)`[1]
  return(res)
}

map_score_aov <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) && !is.numeric(predictor_col)) {
    return(NA_real_)
  }

  if (is.numeric(outcome_col) && !is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

make_scores_aov <- function(score_type, score, outcome, predictors) {
  res <- tibble::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' Compute F-statistic and p-value scores using ANOVA F-test
#'
#' Evaluate the relationship between a numeric outcome and a categorical predictor,
#' or vice versa, by computing the ANOVA F-statistic or p-value.
#' Output a tibble result with with one row per predictor, and four columns:
#' `name`, `score`, `predictor`, and `outcome`.
#'
#' @param score_obj A score object. See [score_aov()] for details.
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
#' data(ames, package = "modeldata")
#' ames_subset <- modeldata::ames |>
#'   dplyr::select(
#'     Sale_Price,
#'     MS_SubClass,
#'     MS_Zoning,
#'     Lot_Frontage,
#'     Lot_Area,
#'     Street
#'   )
#' # Return score as F-statistics
#' score_obj <- score_aov()
#' score_res <- get_scores_aov(
#'   score_obj,
#'   data = ames_subset,
#'   outcome = "Sale_Price"
#' )
#' score_res
#' # Return score as p-values
#' score_obj <- score_aov(score_type = "pval")
#' score_res <- get_scores_aov(
#'   score_obj,
#'   data = ames_subset,
#'   outcome = "Sale_Price"
#' )
#' score_res
#' # Return raw p-values instead of -log10(p-values)
#' score_obj <- score_aov(
#'   score_type = "pval",
#'   neg_log10 = FALSE,
#'   direction = "minimize",
#'   fallback_value = 0
#' )
#' score_res <- get_scores_aov(
#'   score_obj,
#'   data = ames_subset,
#'   outcome = "Sale_Price"
#' )
#' score_res
get_scores_aov <- function(score_obj, data, outcome, ...) {
  if (score_obj@score_type == "fstat") {
    score_obj@calculating_fn <- get_single_f_stat
  } else if (score_obj@score_type == "pval") {
    score_obj@calculating_fn <- get_single_p_val
  }

  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_aov(data, x, outcome, score_obj@calculating_fn)
  )

  # Do we need score <- stats::p.adjust(score) here too?

  if (
    score_obj@score_type == "pval" &&
      (is.null(score_obj@neg_log10) || isTRUE(score_obj@neg_log10))
  ) {
    score <- -log10(score)
  }

  res <- make_scores_aov(score_obj@score_type, score, outcome, predictors)
  res
}
