#' Construct a subclassed score object for information gain with additional metadata
#'
#' Introduce a new properties `is_reg`.
#' Output a new score object that contains associated metadata, such as `range`,
#' `fallback_value`, `score_type`, `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#'
#' @param fallback_value A numeric scalar used as a fallback value. One of:
#'    - `1`
#'   - `Inf` (default)
#' @param score_type A character string indicating the type of scoring metric to compute.
#' One of:
#'    - `"infogain"` (default)
#'    - `"gainratio"`
#'    - `"symuncert"`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#' @param is_reg NULL
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type`, `direction`, and other relevant attributes.
#' @export
#'
#' @examples
#' # Create a score object
#' new_score_obj_info_gain()
new_score_obj_info_gain <- S7::new_class(
  "new_score_obj_info_gain",
  parent = new_score_obj,
  properties = list(
    is_reg = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' Create a score object for information gain
#'
#' Construct a score object containing metadata for feature scoring using
#' information gain.
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj_info_gain
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples
#' score_info_gain()
score_info_gain <- function(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "infogain",
  direction = "maximize",
  is_reg = FALSE
) {
  new_score_obj_info_gain(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = FALSE,
    range = range,
    inclusive = c(TRUE, TRUE),
    fallback_value = Inf,
    score_type = score_type, # c("infogain", "gainratio", "symuncert"),
    #trans = # Cannot set NULL. Otherwise S7 complains
    #sorts =
    direction = direction,
    deterministic = TRUE,
    tuning = FALSE,
    #ties =
    calculating_fn = function(x) {}, # Otherwise S7 complains
    is_reg = is_reg,
    label = c(score_infogain = "Information Gain scores")
  )
}

make_scores_info_gain <- function(
  score_type,
  fit,
  outcome
) {
  res <- tibble::tibble(
    name = score_type,
    score = fit$importance,
    outcome = outcome,
    predictor = fit$attributes
  )
  res
}

#' Compute information gain
#'
#' @param score_obj NULL
#'
#' @param data NULL
#' @param outcome NULL
#' @param ... NULL
#'
#' @export
get_scores_info_gain <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$is_reg
) {
  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]

  fit <- FSelectorRcpp::information_gain(
    x = X,
    y = y,
    type = "infogain", # TODO
    equal = score_obj@is_reg # Set = TRUE for numeric outcome
  )
  res <- make_scores_info_gain(score_obj@score_type, fit, outcome)
  res
}
