#' Construct a subclassed score object for feature importance scores with additional metadata
#'
#' Introduce new properties `engine`, `trees`, `mtry`, `min_n` and `is_reg`.
#' Output a new score object that contains associated metadata, such as `range`,
#' `fallback_value`, `score_type`, `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj
#'
#' @param fallback_value A numeric scalar used as a fallback value. One of:
#'   - `Inf` (default)
#' @param score_type A character string indicating the type of scoring metric to compute.
#' One of:
#'    - `"imp_rf"` (default)
#'    - `"imp_rf_conditional"`
#'    - `"imp_rf_oblique"`
#' @param direction A character string indicating the optimization direction. One of:
#'  - `"maximize"` (default)
#'  - `"minimize"`
#'  - `"target"`
#' @param engine NULL
#' @param trees NULL
#' @param mtry NULL
#' @param min_n NULL
#' @param is_reg NULL
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type`, `direction`, and other relevant attributes.
#' @export
#'
#' @examples
#' # Create a score object
#' new_score_obj_forest_imp()
new_score_obj_forest_imp <- S7::new_class(
  "new_score_obj_forest_imp",
  parent = new_score_obj,
  properties = list(
    engine = S7::new_property(S7::class_character, default = "ranger"),
    trees = S7::new_property(S7::class_numeric, default = 10), # TODO May need to set other default
    mtry = S7::new_property(S7::class_numeric, default = 2),
    min_n = S7::new_property(S7::class_numeric, default = 1),
    is_reg = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' Create a score object for feature importance scores
#'
#' Construct a score object containing metadata for feature scoring using a
#' random forest, a conditional random forest or an oblique random forest
#' Output a score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (), `direction`, and other relevant attributes.
#'
#' @inheritParams new_score_obj_forest_imp
#'
#' @returns A score object containing associated metadata such as `range`, `fallback_value`,
#' `score_type` (), `direction`, and other relevant attributes.
#'
#' @export
#'
#' @examples NULL
score_forest_imp <- function(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "imp_rf",
  direction = "maximize",
  engine = "ranger",
  trees = 10,
  mtry = 2,
  min_n = 1,
  is_reg = FALSE
) {
  new_score_obj_forest_imp(
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
    deterministic = FALSE,
    tuning = TRUE,
    #ties =
    calculating_fn = function(x) {}, # Otherwise S7 complains
    engine = engine,
    trees = trees,
    mtry = mtry,
    min_n = min_n,
    is_reg = is_reg,
    label = c(score_rfimp = "Random Forest importance scores")
  )
}

get_imp_rf_ranger <- function(score_obj, data, outcome) {
  if (score_obj@score_type == "imp_rf") {
    importance_type = "permutation"
  } # TODO Allow option for importance = c("impurity")

  y <- data[[outcome]]
  X <- data[setdiff(names(data), outcome)]
  fit <- ranger::ranger(
    x = X,
    y = y,
    num.trees = score_obj@trees,
    mtry = score_obj@mtry,
    importance = importance_type,
    min.node.size = score_obj@min_n,
    classification = !score_obj@is_reg, # TODO Might scream at me.
    seed = 42 # TODO Add this to pass tests. Remove later.
  )
  imp <- fit$variable.importance
  imp
}

get_imp_rf_partykit <- function(score_obj, data, formula) {
  fit <- partykit::cforest(
    formula = formula,
    data = data,
    control = partykit::ctree_control(minsplit = score_obj@min_n), # TODO Eventually have user pass in ctree_control()
    ntree = score_obj@trees,
    mtry = score_obj@mtry,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  imp
}

get_imp_rf_aorsf <- function(score_obj, data, formula) {
  if (score_obj@score_type == "imp_rf_oblique") {
    importance_type = "permute"
  } # TODO Allow option for importance = c("none", "anova", "negate")

  fit <- aorsf::orsf(
    formula = formula,
    data = data,
    n_tree = score_obj@trees,
    n_retry = score_obj@mtry,
    importance = importance_type
  )
  imp <- fit$importance # orsf_vi_permute(fit)
  imp
}

make_scores_forest_importance <- function(
  score_type,
  imp,
  outcome,
  predictors
) {
  score <- imp[predictors] |> unname()
  score[is.na(score)] <- 0

  res <- tibble::tibble(
    name = score_type,
    score = score,
    outcome = outcome,
    predictor = predictors
  )
  res
}

#' Compute feature importance scores using a random forest, a conditional random forest, or
#' an oblique random forest
#'
#' @param score_obj NULL
#'
#' @param data NULL
#' @param outcome NULL
#' @param ... NULL
#'
#' @export
get_scores_forest_importance <- function(
  score_obj,
  data,
  outcome,
  ... # i.e., score_obj$engine, score_obj$trees, score_obj$mtry, score_obj$min_n, score_obj$is_reg
) {
  outcome_name <- outcome |> as.name()
  formula <- stats::as.formula(paste(outcome_name, "~ ."))
  predictors <- setdiff(names(data), outcome)

  if (score_obj@engine == "ranger") {
    score_obj@score_type <- "imp_rf"
    imp <- get_imp_rf_ranger(score_obj, data, outcome)
  } else if (score_obj@engine == "partykit") {
    score_obj@score_type <- "imp_rf_conditional"
    imp <- get_imp_rf_partykit(score_obj, data, formula)
  } else if (score_obj@engine == "aorsf") {
    score_obj@score_type <- "imp_rf_oblique"
    imp <- get_imp_rf_aorsf(score_obj, data, formula)
  }
  res <- make_scores_forest_importance(
    score_obj@score_type,
    imp,
    outcome,
    predictors
  )
  res
}
