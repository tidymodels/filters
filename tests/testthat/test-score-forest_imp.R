test_that("get_score_forest_importance() is working for ranger for classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- modeldata::cells |>
    dplyr::select(
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  score_obj <- score_forest_imp()
  # score_obj$engine <- "ranger"
  # score_obj$trees <- 10
  # score_obj$mtry <- 2
  # score_obj$min_n <- 1
  # score_obj$class <- TRUE # TODO
  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  outcome <- "class"
  y <- cells_subset[[outcome]]
  X <- cells_subset[setdiff(names(cells_subset), outcome)]
  set.seed(42)
  fit <- ranger::ranger(
    y = y,
    x = X,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = TRUE,
    seed = 42 # TODO Add this to pass tests. Remove later. set.seed(42) alone does not seem to work.
  )
  exp.res <- unname(fit$variable.importance)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(cells_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "imp_rf")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_score_forest_importance() is working for ranger regression", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_forest_imp(is_reg = TRUE)
  set.seed(42)
  score_res <- get_scores_forest_importance(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  outcome <- "Sale_Price"
  y <- ames_subset[[outcome]]
  X <- ames_subset[setdiff(names(ames_subset), outcome)]
  set.seed(42)
  fit <- ranger::ranger(
    x = X,
    y = y,
    num.trees = 10,
    mtry = 2,
    importance = "permutation",
    min.node.size = 1,
    classification = FALSE,
    seed = 42 # TODO Add this to pass tests. Remove later. set.seed(42) alone does not seem to work.
  )
  exp.res <- unname(fit$variable.importance)

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "imp_rf")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

skip()

test_that("get_score_forest_importance() is working for partykit classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- modeldata::cells |>
    dplyr::select(
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )

  score_obj <- score_forest_imp()
  score_obj$engine <- "partykit"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  score_obj$min_n <- 1
  set.seed(42)
  score_res <- get_scores_forest_importance(score_obj, data, outcome)

  set.seed(42)
  fit <- partykit::cforest(
    formula = class ~ .,
    data = data,
    control = partykit::ctree_control(minsplit = 1), # TODO Eventually have user pass in ctree_control()
    ntree = 10,
    mtry = 2,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  predictors <- setdiff(names(data), outcome)
  exp.imp <- as.numeric(imp[predictors])
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_conditional")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_score_forest_importance() is working for partykit regression", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  data <- modeldata::ames |>
    dplyr::select(
      Sale_Price,
      MS_SubClass,
      MS_Zoning,
      Lot_Frontage,
      Lot_Area,
      Street
    )
  outcome <- "Sale_Price"
  score_obj <- score_forest_imp()
  score_obj$engine <- "partykit"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  score_obj$min_n <- 1
  set.seed(42)
  score_res <- get_scores_forest_importance(score_obj, data, outcome)

  set.seed(42)
  fit <- partykit::cforest(
    formula = Sale_Price ~ .,
    data = data,
    control = partykit::ctree_control(minsplit = 1), # TODO Eventually have user pass in ctree_control()
    ntree = 10,
    mtry = 2,
  )
  imp <- partykit::varimp(fit, conditional = TRUE)
  predictors <- setdiff(names(data), outcome)
  exp.imp <- as.numeric(imp[predictors])
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_conditional")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test aorsf::orsf classification

test_that("get_score_forest_importance() is working for aorsf regression", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata")
  data <- modeldata::ames |>
    dplyr::select(
      Sale_Price,
      MS_SubClass,
      MS_Zoning,
      Lot_Frontage,
      Lot_Area,
      Street
    )
  outcome <- "Sale_Price"
  score_obj <- score_forest_imp()
  score_obj$engine <- "aorsf"
  score_obj$trees <- 10
  score_obj$mtry <- 2
  set.seed(42)
  score_res <- get_scores_forest_importance(score_obj, data, outcome)

  set.seed(42)
  fit <- aorsf::orsf(
    formula = Sale_Price ~ .,
    data = data,
    n_tree = 10,
    n_retry = 2,
    importance = "permute"
  )
  imp <- fit$importance
  predictors <- setdiff(names(data), outcome)
  exp.imp <- as.numeric(imp[predictors])
  exp.imp[is.na(exp.imp)] <- 0

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.imp)

  expect_equal(unique(score_res$name), "imp_rf_oblique")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test more after we add validators
