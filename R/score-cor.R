#' Create a score object for correlation coefficients
#'
#' Construct a score object containing metadata for univariate feature scoring using the
#' correlation coefficients.
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"pearson"` or `"spearman"`), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#' @param fallback_value A numeric scalar used as a fallback value. One of:
#'    - `1` (default)
#' @param score_type A character string indicating the type of scoring metric to compute.
#' One of:
#'    - `"pearson"` (default)
#'    - `"spearman"`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (`"pearson"` or `"spearman"`), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples
#' # Create a score object
#' score_cor()
#' # Change score type
#' score_cor(score_type = "spearman")
score_cor <- function(
  range = c(-1, 1),
  fallback_value = 1,
  score_type = "pearson",
  direction = "maximize"
) {
  new_score_obj(
    outcome_type = "numeric",
    predictor_type = "numeric",
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
    label = c(score_cor = "Correlation scores")
  )
}

get_single_pearson <- function(predictor, outcome) {
  res <- stats::cor(predictor, outcome, method = "pearson")
  return(res)
}

get_single_spearman <- function(predictor, outcome) {
  res <- stats::cor(predictor, outcome, method = "spearman")
  return(res)
}

map_score_cor <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.factor(outcome_col) || is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

make_scores_cor <- function(score_type, score, outcome, predictors) {
  res <- tibble::tibble(
    name = score_type,
    score = unname(score),
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' Compute Pearson or Spearman correlation coefficients
#'
#' Evaluate the relationship between a numeric outcome and a numeric predictor,
#' by computing the Pearson or Spearman correlation coefficients.
#' Output a tibble result with with one row per predictor, and four columns:
#' `name`, `score`, `predictor`, and `outcome`.
#'
#' @param score_obj A score object. See [score_cor()] for details.
#'
#' @param data A data frame or tibble containing the outcome and predictor variables.
#' @param outcome A character string specifying the name of the outcome variable.
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
#' # Return score as pearson correlation
#' score_obj <- score_cor()
#' score_res <- get_scores_cor(
#'   score_obj,
#'   data = ames_subset,
#'   outcome = "Sale_Price"
#' )
#' score_res
#' # Return score as spearman correlation
#' score_obj <- score_cor(score_type = "spearman")
#' score_res <- get_scores_cor(
#'   score_obj,
#'   data = ames_subset,
#'   outcome = "Sale_Price"
#' )
#' score_res
get_scores_cor <- function(score_obj, data, outcome) {
  if (score_obj@score_type == "pearson") {
    score_obj@calculating_fn <- get_single_pearson
  } else if (score_obj@score_type == "spearman") {
    score_obj@calculating_fn <- get_single_spearman
  }
  predictors <- setdiff(names(data), outcome)

  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) map_score_cor(data, x, outcome, score_obj@calculating_fn)
  )

  res <- make_scores_cor(score_obj@score_type, score, outcome, predictors)
  res
}
