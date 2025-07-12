test_that("get_scores_aov() is working for fstat", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_aov(score_type = "fstat")
  score_res <- get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(score_res$name), "fstat")

  expect_equal(unique(score_res$outcome), "Sale_Price")

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$MS_SubClass)
  exp.MS_SubClass <- stats::anova(fit)$`F value`[1]

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$MS_Zoning)
  exp.MS_Zoning <- stats::anova(fit)$`F value`[1]

  exp.Lot_Frontage <- NA

  exp.Lot_Area <- NA

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$Street)
  exp.Street <- stats::anova(fit)$`F value`[1]

  expect_identical(
    score_res$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    )
  )
})

# TODO Test Reversed stats::lm(x ~ y) Can use same data

test_that("get_scores_aov() is working for -log10(pval)", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_aov(score_type = "pval")
  score_res <- get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(score_res$name), "pval")

  expect_equal(unique(score_res$outcome), "Sale_Price")

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$MS_SubClass)
  exp.MS_SubClass <- -log10(stats::anova(fit)$`Pr(>F)`[1])

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$MS_Zoning)
  exp.MS_Zoning <- -log10(stats::anova(fit)$`Pr(>F)`[1])

  exp.Lot_Frontage <- NA

  exp.Lot_Area <- NA

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$Street)
  exp.Street <- -log10(stats::anova(fit)$`Pr(>F)`[1])

  expect_identical(
    score_res$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    )
  )
})

test_that("get_scores_aov() is working for pval", {
  skip_if_not_installed("modeldata")

  ames_subset <- helper_ames()
  score_obj <- score_aov(
    score_type = "pval",
    neg_log10 = FALSE,
    direction = "minimize",
    fallback_value = 0
  )
  score_res <- get_scores_aov(
    score_obj,
    data = ames_subset,
    outcome = "Sale_Price"
  )

  expect_true(tibble::is_tibble(score_res))

  expect_identical(nrow(score_res), ncol(ames_subset) - 1L)

  expect_named(score_res, c("name", "score", "outcome", "predictor"))

  expect_equal(unique(score_res$name), "pval")

  expect_equal(unique(score_res$outcome), "Sale_Price")

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$MS_SubClass)
  exp.MS_SubClass <- stats::anova(fit)$`Pr(>F)`[1]

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$MS_Zoning)
  exp.MS_Zoning <- stats::anova(fit)$`Pr(>F)`[1]

  exp.Lot_Frontage <- NA

  exp.Lot_Area <- NA

  fit <- stats::lm(ames_subset$Sale_Price ~ ames_subset$Street)
  exp.Street <- stats::anova(fit)$`Pr(>F)`[1]

  expect_identical(
    score_res$score,
    c(
      exp.MS_SubClass,
      exp.MS_Zoning,
      exp.Lot_Frontage,
      exp.Lot_Area,
      exp.Street
    )
  )
})

# TODO Test more after we add validators

# fallback_value

# test_that("score_aov() accepts valid score_type", {
#   expect_no_error(score_aov(score_type = "fstat"))
#   expect_no_error(score_aov(score_type = "pval"))
# })

# test_that("score_aov() rejects invalid score_type", {
#   expect_error(score_aov(score_type = "invalid"), "must be one of")
#   expect_error(score_aov(score_type = ""), "must be one of")
# })

# test_that("score_aov() accepts valid direction", {
#   expect_no_error(score_aov(direction = "maximize"))
#   expect_no_error(score_aov(direction = "minimize"))
#   expect_no_error(score_aov(direction = "target"))
# })

# test_that("score_aov() rejects invalid direction", {
#   expect_error(score_aov(direction = "invalid"), "must be one of")
#   expect_error(score_aov(direction = ""), "must be one of")
# })
