skip()

test_that("get_scores_roc_auc() is working", {
  skip_if_not_installed("modeldata")
  data(cells, package = "modeldata")
  data <- tibble::tibble(
    case = cells$case,
    class = cells$class,
    angle_ch_1 = cells$angle_ch_1,
    area_ch_1 = cells$area_ch_1,
    avg_inten_ch_1 = cells$avg_inten_ch_1,
    avg_inten_ch_2 = cells$avg_inten_ch_2,
  )
  outcome <- "class"
  score_obj = score_roc_auc()
  score_res <- get_scores_roc_auc(score_obj, data, outcome)

  exp.case <- NA

  roc <- pROC::roc(
    cells$class,
    cells$angle_ch_1,
    direction = "auto",
    quiet = TRUE
  )
  exp.angle_ch_1 <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::roc(
    cells$class,
    cells$area_ch_1,
    direction = "auto",
    quiet = TRUE
  )
  exp.area_ch_1 <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::roc(
    cells$class,
    cells$avg_inten_ch_1,
    direction = "auto",
    quiet = TRUE
  )
  exp.avg_inten_ch_1 <- pROC::auc(roc) |> as.numeric()

  roc <- pROC::roc(
    cells$class,
    cells$avg_inten_ch_2,
    direction = "auto",
    quiet = TRUE
  )
  exp.avg_inten_ch_2 <- pROC::auc(roc) |> as.numeric()

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(data) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_identical(
    score_res$score,
    c(
      exp.case,
      exp.angle_ch_1,
      exp.area_ch_1,
      exp.avg_inten_ch_1,
      exp.avg_inten_ch_2
    )
  )

  expect_equal(unique(score_res$name), "roc_auc")

  expect_equal(unique(score_res$outcome), "class")
})

# TODO Test pROC::multiclass.roc
# TODO Test more after we add validators
