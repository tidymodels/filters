#' Attach score to score object
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
attach_score <- function(x, ...) {
  UseMethod("attach_score")
}

#' @noRd
#' @export
attach_score.default <- function(x, res, ...) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
attach_score.score_obj <- function(x, res, ...) {
  x$res <- res
  x
}

#' Arrange score
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
arrange_score <- function(x, ...) {
  UseMethod("arrange_score")
}

#' @noRd
#' @export
arrange_score.score_obj <- function(x, ..., target = NULL) {
  # TODO Check if direction == target, add "need a target"
  if (x$direction == "maximize") {
    x$res |> dplyr::arrange(dplyr::desc(score))
  } else if (x$direction == "minimize") {
    x$res |> dplyr::arrange(score)
  } else if (x$direction == "target") {
    x$res |> dplyr::arrange(abs(score - target))
  }
}

#' Transform score
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
trans_score <- function(x, ...) {
  UseMethod("trans_score")
}

#' @noRd
#' @export
trans_score.score_obj <- function(x, ...) {
  # TODO Figure out the basic structure first then come back for this. Have user supply direction =, if they use trans.
  if (is.null(x$trans)) {
    trans <- scales::transform_identity()
  } else {
    trans <- x$trans
  }
  x$res |>
    dplyr::mutate(score = trans$transform(score))
}

#' Filter score based on number of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_num <- function(x, ...) {
  UseMethod("filter_score_num")
}

#' @noRd
#' @export
filter_score_num.score_obj <- function(x, ..., num_terms, target = NULL) {
  # TODO Handle ties here?
  if (x$direction == "maximize") {
    x$res |> dplyr::slice_max(score, n = num_terms)
  } else if (x$direction == "minimize") {
    x$res |> dplyr::slice_min(score, n = num_terms)
  } else if (x$direction == "target") {
    x$res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(n = num_terms)
  }
}

#' Filter score based on proportion of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_prop <- function(x, ...) {
  UseMethod("filter_score_prop")
}

#' @noRd
#' @export
filter_score_prop.score_obj <- function(x, ..., prop_terms, target = NULL) {
  # TODO Handle ties here?
  if (x$direction == "maximize") {
    x$res |> dplyr::slice_max(score, prop = prop_terms)
  } else if (x$direction == "minimize") {
    x$res |> dplyr::slice_min(score, prop = prop_terms)
  } else if (x$direction == "target") {
    x$res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(prop = prop_terms)
  }
}

#' Filter score based on cutoff value
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_cutoff <- function(x, ...) {
  UseMethod("filter_score_cutoff")
}

#' @noRd
#' @export
filter_score_cutoff.score_obj <- function(x, ..., cutoff, target = NULL) {
  if (x$direction == "maximize") {
    # TODO Can return more # of predictors due to floating-point precision.
    x$res |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::filter(score >= cutoff)
  } else if (x$direction == "minimize") {
    # TODO Can return less # of predictors due to floating-point precision.
    x$res |> dplyr::arrange(score) |> dplyr::filter(score <= cutoff)
  } else if (x$direction == "target") {
    # TODO This cutoff value is based on abs(score - target). Not ideal?
    x$res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::filter(abs(score - target) <= cutoff)
  }
}

# TODO Filter score based on user input
# filter_score_<>

# COMMENT rank_score_*() below creates an additional column named rank, but can perhaps futher
# |> filter(rank = )?

#' Rank score based on min_rank (Need a better title)
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
rank_score_min <- function(x, ...) {
  UseMethod("rank_score_min")
}

#' @noRd
#' @export
rank_score_min.score_obj <- function(x, ..., target = NULL) {
  if (x$direction == "maximize") {
    x$res |> dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))
  } else if (x$direction == "minimize") {
    x$res |> dplyr::mutate(rank = dplyr::min_rank((score)))
  } # else if (x$direction == "target") { # TODO
  #   x$res |> dplyr::arrange(abs(score - target))
  # }
}

#' Rank score based on dense_rank (Need a better title)
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
rank_score_dense <- function(x, ...) {
  UseMethod("rank_score_dense")
}

#' @noRd
#' @export
rank_score_dense.score_obj <- function(x, ..., target = NULL) {
  if (x$direction == "maximize") {
    x$res |> dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))
  } else if (x$direction == "minimize") {
    x$res |> dplyr::mutate(rank = dplyr::dense_rank((score)))
  } # else if (x$direction == "target") { # TODO
  #   x$res |> dplyr::arrange(abs(score - target))
  # }
}

#' Assign class result_obj to score object score_obj
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
as_result_obj <- function(x, ...) {
  UseMethod("as_result_obj")
}

#' @noRd
#' @export
as_result_obj.score_obj <- function(x, res, ...) {
  class(res) <- c("result_obj", class(res))
  x$res <- res # COMMENT This overlaps with attach_score()
  x
}

# TODO Bind scores
# bind_score
