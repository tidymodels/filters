#' Construct a new score object
#'
#' Output a new score object that contains associated metadata, such as `range`,
#' `fallback_value`, `score_type`, `direction`, and other relevant attributes.
#'
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
#' @param trans NULL
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
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type`, `direction`, and other relevant attributes.
#' @export
#'
#' @examples
#' # Create a score object
#' new_score_obj()
new_score_obj <- S7::new_class(
  "new_score_obj",
  properties = list(
    outcome_type = S7::new_property(
      S7::class_character,
      default = c("numeric", "factor")
    ),
    predictor_type = S7::new_property(
      S7::class_character,
      default = c("numeric", "factor")
    ),
    case_weights = S7::class_logical,
    range = S7::class_numeric,
    inclusive = S7::class_logical,
    fallback_value = S7::class_numeric,
    score_type = S7::class_character,
    trans = S7::class_function,
    sorts = S7::class_function,
    direction = S7::class_character,
    deterministic = S7::class_logical,
    tuning = S7::class_logical,
    ties = S7::class_logical,
    calculating_fn = S7::class_function,
    label = S7::class_character
  )
)
