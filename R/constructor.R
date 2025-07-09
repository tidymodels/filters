#' Construct a new score object
#'
#' Create a new score object that contains associated metadata, such as `range`,
#' `fallback_value`, `score_type`, `direction`, and other relevant attributes.
#'
#' @param subclass A character string indicating the type of predictor-outcome combination
#' the scoring method supports. One of:
#'  - `"cat_num"`
#'  - `"cat_cat"`
#'  - `"num_num"`
#'  - `"any"`
#' @param outcome_type A character string indicating the outcome type. One of:
#'  - `"numeric"`
#'  - `"factor"`
#' @param predictor_type A character string indicating the predictor type. One of:
#'  - `"numeric"`
#'  - `"factor"`
#' @param case_weights A logical value, indicating whether the model accepts
#' case weights (`TRUE`) or not (`FALSE`).
#' @param range A numeric vector of length two, specifying the minimum and maximum
#' possible values, respectively.
#' @param inclusive A logical vector of length two, indicating whether the lower and
#' upper bounds of the range are inclusive (`TRUE`) or exclusive (`FALSE`).
#' @param fallback_value A numeric scalar used as a fallback value. Typical values
#' include:
#'   - `0`
#'   - `1`
#'   - `Inf`
#' @param score_type A character string indicating the type of scoring metric to compute.
#' Available options include:
#'    - ANOVA F-Test: `"fstat"`, `"pval"`
#'    - Correlation: `"pearson"`, `"spearman"`
#'    - Cross Tabulation: `"pval_chisq"`, `"pval_fisher"`
#'    - Random Forest:`"imp_rf"`, `"imp_rf_conditional"`, `"imp_rf_oblique"`
#'    - Information Gain: `"infogain"`, `"gainratio"`, `"symuncert"`
#'    - ROC AUC: `"roc_auc"`
#' @param trans Currently not used.
#' @param sorts An optional function used to sort the scores. Common options include:
#'  - `identity`
#'  - `abs`
#'  - `function(score) max(score, 1 - score)`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"`
#'  - `"minimize"`
#'  - `"target"`
#' @param deterministic A logical value, indicating whether the score is
#' deterministic (`TRUE`) or not (`FALSE`).
#' @param tuning A logical value, indicating whether the model should be tuned
#' (`TRUE`) or not (`FALSE`).
#' @param ties An optional logical value indicating whether ties in score can occur (`TRUE`)
#' or not (`FALSE`).
#' @param calculating_fn An optional function used to compute the score. A default function
#' is selected based on the `score_type`.
#' @param label A named character string that can be used for printing and plotting.
#' @param ... Currently not used.
#'
#' @returns A score object.
#' @export
#'
#' @examples
#' # Create a score object
#' new_score_obj()
new_score_obj <- function(
  subclass = c("cat_num", "cat_cat", "num_num", "any"), # TODO Rename subclass
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = NULL,
  range = NULL,
  inclusive = NULL,
  fallback_value = NULL,
  score_type = NULL,
  trans = NULL,
  sorts = NULL,
  direction = NULL,
  deterministic = NULL,
  tuning = NULL,
  ties = NULL,
  calculating_fn = NULL,
  label = NULL,
  ...
) {
  # TODO Include validators here
  # TODO Add a validator to make sure subclass has to be in num_num, cat_num, cat_cat, any

  res <- list(
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    case_weights = case_weights,
    range = range,
    inclusive = inclusive,
    fallback_value = fallback_value,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = direction,
    deterministic = deterministic,
    tuning = tuning,
    ties = ties,
    calculating_fn = calculating_fn,
    label = label
  )
  class(res) <- c(subclass, "score_obj")

  res
}
