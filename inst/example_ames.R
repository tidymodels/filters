skip_if_not_installed("modeldata")
data(ames, package = "modeldata")
data <- modeldata::ames |>
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street #,
    # Alley, # ADD MORE
    # Lot_Shape,
    # Land_Contour,
    # Utilities,
    # Lot_Config,
    # Land_Slope
  )
outcome <- "Sale_Price"
score_obj = score_aov()
res <- get_scores_aov(score_obj, data, outcome)

# Attach score
score_obj <- score_obj |> attach_score(res)
score_obj$res

# Arrange score
score_obj$direction <- "maximize" # Default
score_obj |> arrange_score()

score_obj$direction <- "minimize"
score_obj |> arrange_score()

score_obj$direction <- "target"
score_obj |> arrange_score(target = 63.8)

# Transform score
score_obj$trans <- NULL # Default
score_obj |> trans_score()

score_obj$trans <- scales::transform_log()
score_obj |> trans_score()

# Filter score based on number of predictors
score_obj$direction <- "maximize" # Default
score_obj |> filter_score_num(num_terms = 2)

score_obj$direction <- "minimize"
score_obj |> filter_score_num(num_terms = 2)

score_obj$direction <- "target"
score_obj |>
  filter_score_num(score_obj, num_terms = 2, target = 63.8)

# Filter score based on proportion of predictors
score_obj$direction <- "maximize" # Default
score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small. dplyr::near()?

score_obj$direction <- "minimize"
score_obj |> filter_score_prop(prop_terms = 0.2) # TODO Can return NULL for prop = 0.1 if # of predictor is small. dplyr::near()?

score_obj$direction <- "target"
score_obj |>
  filter_score_prop(score_obj, prop_terms = 0.2, target = 63.8) # TODO Can return NULL for prop = 0.1 if # of predictor is small

# Filter score based on cutoff value
score_obj$direction <- "maximize"
score_obj |> filter_score_cutoff(cutoff = 63.8)

score_obj$direction <- "minimize"
score_obj |> filter_score_cutoff(cutoff = 63.8)

score_obj$direction <- "target"
score_obj |> filter_score_cutoff(target = 63.8, cutoff = 4) # TODO This cutoff value is based on abs(score - target). Not ideal?

# Rank score based on min_rank
score_obj$direction <- "maximize"
score_obj |> rank_score_min()

score_obj$direction <- "minimize"
score_obj |> rank_score_min()

# Rank score based on dense_rank
score_obj$direction <- "maximize"
score_obj |> rank_score_dense()

score_obj$direction <- "minimize"
score_obj |> rank_score_dense()

# Assign class result_obj to score object score_obj
res |> class()
tmp <- score_obj |> as_result_obj(res)
tmp$res |> class()

# Experiment with scores
score_obj = score_aov()
res <- get_scores_aov(score_obj, data, outcome)

score_obj_cor = score_cor()
res_cor <- get_scores_cor(score_obj_cor, data, outcome)

score_obj_imp <- score_forest_imp()
score_obj_imp$engine <- "ranger"
score_obj_imp$trees <- 10
score_obj_imp$mtry <- 2
score_obj_imp$min_n <- 1
score_obj_imp$class <- FALSE # TODO
set.seed(42)
res_imp <- get_scores_forest_importance(score_obj_imp, data, outcome)
