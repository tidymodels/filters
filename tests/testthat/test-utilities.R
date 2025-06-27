test_that("attach_score() is working for roc auc", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- modeldata::cells |>
    dplyr::select(
      case,
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  outcome <- "class"
  score_obj = score_roc_auc()
  res <- get_scores_roc_auc(score_obj, data, outcome)

  ex.score_obj <- score_obj |> attach_score(res)

  expect_equal(ex.score_obj$res, res)
})

test_that("attach_score() is working for aov", {
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
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  ex.score_obj <- score_obj |> attach_score(res)

  expect_equal(ex.score_obj$res, res)
})

test_that("arrange_score() is working for roc auc", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- modeldata::cells |>
    dplyr::select(
      case,
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  outcome <- "class"
  score_obj = score_roc_auc()
  res <- get_scores_roc_auc(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res) # TODO User have to run this for arrange_score() to work

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> arrange_score()

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> arrange_score()

  score_obj$direction <- "target"
  ex.target <- score_obj |> arrange_score(target = 0.760)

  score_obj$direction <- "target"
  ex.target2 <- score_obj |> arrange_score(target = 0.502)

  expect_equal(ex.max, res |> dplyr::arrange(desc(score)))

  expect_equal(ex.min, res |> dplyr::arrange(score))

  expect_equal(ex.target, res |> dplyr::arrange(abs(score - 0.760)))

  expect_equal(ex.target2, res |> dplyr::arrange(abs(score - 0.502)))
})

test_that("arrange_score() is working for aov", {
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
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res)

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> arrange_score()

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> arrange_score()

  score_obj$direction <- "target"
  ex.target <- score_obj |> arrange_score(target = 63.8)

  score_obj$direction <- "target"
  ex.target2 <- score_obj |> arrange_score(target = 10.4)

  expect_equal(ex.max, res |> dplyr::arrange(desc(score)))

  expect_equal(ex.min, res |> dplyr::arrange(score))

  expect_equal(ex.target, res |> dplyr::arrange(abs(score - 63.8)))

  expect_equal(ex.target2, res |> dplyr::arrange(abs(score - 10.4)))
})

test_that("trans_score() is working for roc auc", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- modeldata::cells |>
    dplyr::select(
      case,
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  outcome <- "class"
  score_obj = score_roc_auc()
  res <- get_scores_roc_auc(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res)

  score_obj$trans <- NULL # Default
  ex.identity <- score_obj |> trans_score()

  score_obj$trans <- scales::transform_log()
  ex.log <- score_obj |> trans_score()

  # TODO Add a couple more scales::transform_*()

  expect_equal(ex.identity, res)

  expect_equal(ex.log, res |> dplyr::mutate(score = log(score)))
})

test_that("trans_score() is working for aov", {
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
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res)

  score_obj$trans <- NULL # Default
  ex.identity <- score_obj |> trans_score()

  score_obj$trans <- scales::transform_log()
  ex.log <- score_obj |> trans_score()

  # TODO Add a couple more scales::transform_*()

  expect_equal(ex.identity, res)

  expect_equal(ex.log, res |> dplyr::mutate(score = log(score)))
})

test_that("filter_score_num() is working for roc auc", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- modeldata::cells |>
    dplyr::select(
      case,
      class,
      angle_ch_1,
      area_ch_1,
      avg_inten_ch_1,
      avg_inten_ch_2
    )
  outcome <- "class"
  score_obj = score_roc_auc()
  res <- get_scores_roc_auc(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res)

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> filter_score_num(num_terms = 2)

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> filter_score_num(num_terms = 2)

  score_obj$direction <- "target"
  ex.target <- score_obj |>
    filter_score_num(score_obj, num_terms = 2, target = 0.760)

  score_obj$direction <- "target"
  ex.target2 <- score_obj |>
    filter_score_num(score_obj, num_terms = 2, target = 0.591)

  expect_equal(ex.max, res |> dplyr::slice_max(score, n = 2))

  expect_equal(ex.min, res |> dplyr::slice_min(score, n = 2))

  expect_equal(
    ex.target,
    res |>
      dplyr::arrange(abs(score - 0.760)) |>
      dplyr::slice_head(n = 2)
  )

  expect_equal(
    ex.target2,
    res |>
      dplyr::arrange(abs(score - 0.591)) |>
      dplyr::slice_head(n = 2)
  )
})

test_that("filter_score_num() is working for aov", {
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
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res)

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> filter_score_num(num_terms = 2)

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> filter_score_num(num_terms = 2)

  score_obj$direction <- "target"
  ex.target <- score_obj |>
    filter_score_num(score_obj, num_terms = 2, target = 63.8)

  score_obj$direction <- "target"
  ex.target2 <- score_obj |>
    filter_score_num(score_obj, num_terms = 2, target = 10.4)

  expect_equal(ex.max, res |> dplyr::slice_max(score, n = 2))

  expect_equal(ex.min, res |> dplyr::slice_min(score, n = 2))

  expect_equal(
    ex.target,
    res |>
      dplyr::arrange(abs(score - 63.8)) |>
      dplyr::slice_head(n = 2)
  )

  expect_equal(
    ex.target2,
    res |>
      dplyr::arrange(abs(score - 10.4)) |>
      dplyr::slice_head(n = 2)
  )
})

# TODO Test for roc auc

test_that("filter_score_prop() is working for aov", {
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
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res)

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small

  score_obj$direction <- "target"
  ex.target <- score_obj |>
    filter_score_prop(score_obj, prop_terms = 0.2, target = 63.8) # TODO Can return NULL for prop = 0.1 if # of predictor is small

  score_obj$direction <- "target"
  ex.target2 <- score_obj |>
    filter_score_prop(score_obj, prop_terms = 0.2, target = 10.4)

  expect_equal(ex.max, res |> dplyr::slice_max(score, prop = 0.2))

  expect_equal(ex.min, res |> dplyr::slice_min(score, prop = 0.2))

  expect_equal(
    ex.target,
    res |>
      dplyr::arrange(abs(score - 63.8)) |>
      dplyr::slice_head(prop = 0.2)
  )

  expect_equal(
    ex.target2,
    res |>
      dplyr::arrange(abs(score - 10.4)) |>
      dplyr::slice_head(prop = 0.2)
  )
})

# TODO Test for roc auc

test_that("filter_score_cutoff() is working for aov", {
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
  score_obj = score_aov()
  res <- get_scores_aov(score_obj, data, outcome)

  score_obj <- score_obj |> attach_score(res)

  score_obj$direction <- "maximize" # Default
  ex.max <- score_obj |> filter_score_cutoff(cutoff = 63.8)

  score_obj$direction <- "minimize"
  ex.min <- score_obj |> filter_score_cutoff(cutoff = 63.8)

  score_obj$direction <- "target"
  ex.target <- score_obj |> filter_score_cutoff(target = 63.8, cutoff = 4)

  score_obj$direction <- "target"
  ex.target2 <- score_obj |> filter_score_cutoff(target = 10.4, cutoff = 1)

  expect_equal(
    ex.max,
    res |> dplyr::arrange(desc(score)) |> dplyr::filter(score >= 63.8)
  ) # TODO Can return more # of predictors due to floating-point precision

  expect_equal(
    ex.min,
    res |> dplyr::arrange(score) |> dplyr::filter(score <= 63.8)
  ) # TODO Can return less # of predictors due to floating-point precision

  expect_equal(
    ex.target,
    res |>
      dplyr::arrange(abs(score - 63.8)) |>
      dplyr::filter(abs(score - 63.8) <= 4)
  )

  expect_equal(
    ex.target2,
    res |>
      dplyr::arrange(abs(score - 10.4)) |>
      dplyr::filter(abs(score - 10.4) <= 1)
  )
})

# TODO Test rank_score_min()

# TODO Test rank_score_dense()

# res %>%
#   dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))

# res %>%
#   dplyr::mutate(rank = dplyr::min_rank((score)))

# res %>%
#   dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))

# res %>%
#   dplyr::mutate(rank = dplyr::dense_rank((score)))

# TODO Test as_result_obj()
