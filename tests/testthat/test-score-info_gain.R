test_that("get_scores_info_gain() is working for classification", {
  skip_if_not_installed("modeldata")

  cells_subset <- modeldata::cells |>
    dplyr::select(
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )

  score_obj <- score_info_gain()
  score_res <- get_scores_info_gain(
    score_obj,
    data = cells_subset,
    outcome = "class"
  )

  outcome <- "class"
  y <- cells_subset[[outcome]]
  X <- cells_subset[setdiff(names(cells_subset), outcome)]
  fit <- FSelectorRcpp::information_gain(x = X, y = y)
  exp.res <- fit$importance

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(cells_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "infogain")

  expect_equal(unique(score_res$outcome), "class")
})

test_that("get_scores_info_gain() is working for regression", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_info_gain(is_reg = TRUE)
  score_res <- get_scores_info_gain(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  outcome <- "Sale_Price"
  y <- ames_subset[[outcome]]
  X <- ames_subset[setdiff(names(ames_subset), outcome)]
  fit <- FSelectorRcpp::information_gain(x = X, y = y, equal = TRUE)
  exp.res <- fit$importance

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(score_res$score, exp.res)

  expect_equal(unique(score_res$name), "infogain")

  expect_equal(unique(score_res$outcome), "Sale_Price")
})

# TODO Test more after we add validators
