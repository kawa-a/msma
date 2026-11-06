# Version 5 supervision contract regression tests


make_v5_supervision_fixture <- function(
  n = 40L,
  p = 6L,
  q = 4L,
  seed = 5201L
) {
  set.seed(seed)

  X <- matrix(
    stats::rnorm(n * p),
    nrow = n,
    ncol = p
  )

  Y <- matrix(
    stats::rnorm(n * q),
    nrow = n,
    ncol = q
  )

  colnames(X) <- paste0(
    "x",
    seq_len(ncol(X))
  )

  colnames(Y) <- paste0(
    "y",
    seq_len(ncol(Y))
  )

  Z <- cbind(
    z1 = stats::rnorm(n),
    z2 = stats::rnorm(n)
  )

  list(
    X = X,
    Y = Y,
    Z = Z,
    z = Z[, 1L]
  )
}


fit_inactive_supervision <- function(
  data,
  contrast = NULL,
  comp = 2L,
  seed = 7L
) {
  suppressWarnings(
    msma(
      X = data$X,
      Z = data$Z,
      comp = comp,
      lambdaX = 0.2,
      muX = 0,
      con4spv = contrast,
      intseed = seed
    )
  )
}


expect_same_x_estimate <- function(
  observed,
  expected,
  tolerance = 0
) {
  if (identical(tolerance, 0)) {
    testthat::expect_identical(
      observed$wbX,
      expected$wbX
    )

    testthat::expect_identical(
      observed$sbX,
      expected$sbX
    )

    testthat::expect_identical(
      observed$ssX,
      expected$ssX
    )

    testthat::expect_identical(
      observed$wsX,
      expected$wsX
    )
  } else {
    testthat::expect_equal(
      observed$wbX,
      expected$wbX,
      tolerance = tolerance
    )

    testthat::expect_equal(
      observed$sbX,
      expected$sbX,
      tolerance = tolerance
    )

    testthat::expect_equal(
      observed$ssX,
      expected$ssX,
      tolerance = tolerance
    )

    testthat::expect_equal(
      observed$wsX,
      expected$wsX,
      tolerance = tolerance
    )
  }

  testthat::expect_identical(
    observed$nzwbX,
    expected$nzwbX
  )

  testthat::expect_identical(
    observed$nzwsX,
    expected$nzwsX
  )

  testthat::expect_identical(
    observed$selectXnames,
    expected$selectXnames
  )
}


testthat::test_that(
  "inactive supervision canonicalizes Z and contrast",
  {
    data <- make_v5_supervision_fixture()

    contract <- .canonicalize_msma_supervision(
      Z = data$Z,
      muX = 0,
      muY = 0,
      con4spv = c(1, -0.5),
      dmode = "PCA",
      warn_ignored = FALSE
    )

    testthat::expect_null(
      contract$Z
    )

    testthat::expect_null(
      contract$con4spv
    )

    testthat::expect_null(
      contract$dispatcher_Z
    )

    testthat::expect_null(
      contract$dispatcher_con4spv
    )

    testthat::expect_identical(
      contract$muX,
      0
    )

    testthat::expect_identical(
      contract$muY,
      0
    )

    testthat::expect_false(
      contract$metadata$active$X
    )

    testthat::expect_false(
      contract$metadata$active$Y
    )

    testthat::expect_true(
      contract$metadata$canonicalized
    )

    testthat::expect_identical(
      contract$metadata$contrast_requested,
      c(1, -0.5)
    )

    testthat::expect_null(
      contract$metadata$contrast_used
    )

    testthat::expect_identical(
      contract$metadata$reason,
      "muX_zero"
    )
  }
)


testthat::test_that(
  "inactive multivariate supervision selects the canonical legacy path",
  {
    data <- make_v5_supervision_fixture()

    contract <- .canonicalize_msma_supervision(
      Z = data$Z,
      muX = 0,
      muY = 0,
      con4spv = c(1, -0.5),
      dmode = "PCA",
      warn_ignored = FALSE
    )

    engine <- .resolve_msma_engine(
      structure.method = "none",
      gammaX = 0,
      sprmethod = "PCA",
      Z = contract$dispatcher_Z,
      con4spv = contract$dispatcher_con4spv
    )

    testthat::expect_identical(
      engine,
      "legacy"
    )
  }
)


testthat::test_that(
  "muX zero is invariant to requested contrast",
  {
    data <- make_v5_supervision_fixture()

    contrasts <- list(
      omitted = NULL,
      c10 = c(1, 0),
      c1m005 = c(1, -0.05),
      c1m05 = c(1, -0.5),
      c1m1 = c(1, -1),
      c0m1 = c(0, -1),
      zero = c(0, 0)
    )

    fits <- lapply(
      contrasts,
      function(contrast) {
        fit_inactive_supervision(
          data = data,
          contrast = contrast
        )
      }
    )

    reference <- fits$omitted

    for (name in names(fits)[-1L]) {
      fit <- fits[[name]]

      testthat::expect_identical(
        fit$preprocess$engine,
        reference$preprocess$engine,
        info = name
      )

      expect_same_x_estimate(
        observed = fit,
        expected = reference,
        tolerance = 0
      )

      testthat::expect_null(
        fit$supervision$contrast_used,
        info = name
      )

      testthat::expect_false(
        fit$supervision$active$X,
        info = name
      )

      testthat::expect_true(
        fit$supervision$canonicalized,
        info = name
      )
    }

    testthat::expect_null(
      fits$omitted$supervision$contrast_requested
    )

    testthat::expect_identical(
      fits$c1m05$supervision$contrast_requested,
      c(1, -0.5)
    )

    testthat::expect_identical(
      fits$zero$supervision$contrast_requested,
      c(0, 0)
    )
  }
)


testthat::test_that(
  "inactive supervision gives identical fitted projections",
  {
    data <- make_v5_supervision_fixture()

    reference <- fit_inactive_supervision(
      data = data,
      contrast = NULL
    )

    observed <- fit_inactive_supervision(
      data = data,
      contrast = c(1, -0.5)
    )

    reference_prediction <- predict(
      reference,
      newX = data$X,
      preprocessing = "fitted",
      reconstruction = "fitted",
      output_scale = "analysis"
    )

    observed_prediction <- predict(
      observed,
      newX = data$X,
      preprocessing = "fitted",
      reconstruction = "fitted",
      output_scale = "analysis"
    )

    testthat::expect_identical(
      observed_prediction$sbX,
      reference_prediction$sbX
    )

    testthat::expect_identical(
      observed_prediction$X,
      reference_prediction$X
    )
  }
)


testthat::test_that(
  "inactive nonzero contrast produces one warning",
  {
    data <- make_v5_supervision_fixture()

    testthat::expect_warning(
      msma(
        X = data$X,
        Z = data$Z,
        comp = 1,
        lambdaX = 0.2,
        muX = 0,
        con4spv = c(1, -0.5),
        intseed = 7
      ),
      "ignored because.*supervision term is inactive"
    )
  }
)


testthat::test_that(
  "inactive omitted or zero contrast does not warn",
  {
    data <- make_v5_supervision_fixture()

    testthat::expect_no_warning(
      msma(
        X = data$X,
        Z = data$Z,
        comp = 1,
        lambdaX = 0.2,
        muX = 0,
        con4spv = NULL,
        intseed = 7
      )
    )

    testthat::expect_no_warning(
      msma(
        X = data$X,
        Z = data$Z,
        comp = 1,
        lambdaX = 0.2,
        muX = 0,
        con4spv = c(0, 0),
        intseed = 7
      )
    )
  }
)


testthat::test_that(
  "active multivariate supervision requires a contrast",
  {
    data <- make_v5_supervision_fixture()

    testthat::expect_error(
      msma(
        X = data$X,
        Z = data$Z,
        comp = 1,
        lambdaX = 0.2,
        muX = 0.5,
        intseed = 7
      ),
      "must be supplied.*multiple columns"
    )
  }
)


testthat::test_that(
  "active multivariate supervision rejects zero contrast",
  {
    data <- make_v5_supervision_fixture()

    testthat::expect_error(
      msma(
        X = data$X,
        Z = data$Z,
        comp = 1,
        lambdaX = 0.2,
        muX = 0.5,
        con4spv = c(0, 0),
        intseed = 7
      ),
      "at least one nonzero coefficient"
    )
  }
)


testthat::test_that(
  "active contrast dimensions are validated",
  {
    data <- make_v5_supervision_fixture()

    testthat::expect_error(
      msma(
        X = data$X,
        Z = data$Z,
        comp = 1,
        lambdaX = 0.2,
        muX = 0.5,
        con4spv = 1,
        intseed = 7
      ),
      "length equal to 'ncol\\(Z\\)'"
    )

    testthat::expect_error(
      msma(
        X = data$X,
        Z = data$Z,
        comp = 1,
        lambdaX = 0.2,
        muX = 0.5,
        con4spv = c(1, 0, -1),
        intseed = 7
      ),
      "length equal to 'ncol\\(Z\\)'"
    )
  }
)


testthat::test_that(
  "active contrast must contain finite coefficients",
  {
    data <- make_v5_supervision_fixture()

invalid_contrasts <- list(
  missing_value = c(1, NA_real_),
  not_a_number = c(1, NaN),
  positive_infinity = c(1, Inf),
  negative_infinity = c(1, -Inf)
)

    for (name in names(invalid_contrasts)) {
      testthat::expect_error(
        msma(
          X = data$X,
          Z = data$Z,
          comp = 1,
          lambdaX = 0.2,
          muX = 0.5,
          con4spv = invalid_contrasts[[name]],
          intseed = 7
        ),
        "finite values",
        info = name
      )
    }
  }
)


testthat::test_that(
  "active contrast is invariant to positive scale",
  {
    data <- make_v5_supervision_fixture()

    contrasts <- list(
      original = c(1, -0.5),
      doubled = c(2, -1),
      halved = c(0.5, -0.25)
    )

    fits <- lapply(
      contrasts,
      function(contrast) {
        msma(
          X = data$X,
          Z = data$Z,
          comp = 2,
          lambdaX = 0.2,
          muX = 0.5,
          con4spv = contrast,
          intseed = 7
        )
      }
    )

    reference <- fits$original

    for (name in names(fits)[-1L]) {
      fit <- fits[[name]]

      expect_same_x_estimate(
        observed = fit,
        expected = reference,
        tolerance = 0
      )

      testthat::expect_identical(
        fit$supervision$contrast_used,
        reference$supervision$contrast_used,
        info = name
      )
    }

    testthat::expect_equal(
      reference$supervision$contrast_used,
      c(2 / 3, -1 / 3),
      tolerance = 1e-15
    )

    testthat::expect_equal(
      sum(
        abs(
          reference$supervision$contrast_used
        )
      ),
      1,
      tolerance = 1e-15
    )
  }
)


testthat::test_that(
  "single-column active supervision uses a canonical contrast",
  {
    data <- make_v5_supervision_fixture()

    fit_vector <- msma(
      X = data$X,
      Z = data$z,
      comp = 1,
      lambdaX = 0.2,
      muX = 0.5,
      intseed = 7
    )

    fit_matrix <- msma(
      X = data$X,
      Z = cbind(data$z),
      comp = 1,
      lambdaX = 0.2,
      muX = 0.5,
      intseed = 7
    )

    testthat::expect_equal(
      fit_vector$wbX,
      fit_matrix$wbX,
      tolerance = 1e-12
    )

    testthat::expect_equal(
      fit_vector$sbX,
      fit_matrix$sbX,
      tolerance = 1e-12
    )

    testthat::expect_identical(
      fit_vector$supervision$contrast_requested,
      NULL
    )

    testthat::expect_identical(
      fit_vector$supervision$contrast_used,
      1
    )

    testthat::expect_true(
      fit_vector$supervision$active$X
    )

    testthat::expect_identical(
      fit_vector$supervision$reason,
      "single_column_canonical_contrast"
    )
  }
)


testthat::test_that(
  "single-column explicit contrast is normalized",
  {
    data <- make_v5_supervision_fixture()

    fit <- msma(
      X = data$X,
      Z = data$z,
      comp = 1,
      lambdaX = 0.2,
      muX = 0.5,
      con4spv = 2,
      intseed = 7
    )

    testthat::expect_identical(
      fit$supervision$contrast_requested,
      2
    )

    testthat::expect_identical(
      fit$supervision$contrast_used,
      1
    )

    testthat::expect_identical(
      fit$supervision$reason,
      "active_l1_normalized"
    )
  }
)


testthat::test_that(
  "X-side and Y-side activity are recorded separately",
  {
    data <- make_v5_supervision_fixture()

    x_active <- msma(
      X = data$X,
      Y = data$Y,
      Z = data$Z,
      comp = 1,
      lambdaX = 0.2,
      lambdaY = 0.2,
      muX = 0.4,
      muY = 0,
      con4spv = c(1, -0.5),
      intseed = 7
    )

    y_active <- msma(
      X = data$X,
      Y = data$Y,
      Z = data$Z,
      comp = 1,
      lambdaX = 0.2,
      lambdaY = 0.2,
      muX = 0,
      muY = 0.4,
      con4spv = c(1, -0.5),
      intseed = 7
    )

    both_active <- msma(
      X = data$X,
      Y = data$Y,
      Z = data$Z,
      comp = 1,
      lambdaX = 0.2,
      lambdaY = 0.2,
      muX = 0.2,
      muY = 0.2,
      con4spv = c(1, -0.5),
      intseed = 7
    )

    testthat::expect_true(
      x_active$supervision$active$X
    )

    testthat::expect_false(
      x_active$supervision$active$Y
    )

    testthat::expect_false(
      y_active$supervision$active$X
    )

    testthat::expect_true(
      y_active$supervision$active$Y
    )

    testthat::expect_true(
      both_active$supervision$active$X
    )

    testthat::expect_true(
      both_active$supervision$active$Y
    )
  }
)


testthat::test_that(
  "supervision weights remain finite numeric scalars",
  {
    data <- make_v5_supervision_fixture()

    invalid_mu <- list(
      vector = c(0, 1),
      missing = NA_real_,
      not_a_number = NaN,
      infinity = Inf,
      character = "0"
    )

    for (name in names(invalid_mu)) {
      testthat::expect_error(
        msma(
          X = data$X,
          muX = invalid_mu[[name]]
        ),
        "'muX' must be a finite numeric scalar",
        info = name
      )
    }

    testthat::expect_error(
      msma(
        X = data$X,
        Y = data$Y,
        muY = c(0, 1)
      ),
      "'muY' must be a finite numeric scalar"
    )
  }
)


testthat::test_that(
  "combined supervision weight must not exceed one",
  {
    data <- make_v5_supervision_fixture()

    testthat::expect_error(
      msma(
        X = data$X,
        Y = data$Y,
        Z = data$Z,
        muX = 0.7,
        muY = 0.4,
        con4spv = c(1, -0.5)
      ),
      "muX.*muY.*must not exceed 1"
    )
  }
)


testthat::test_that(
  "structured PCA preserves structure while disabling supervision",
  {
    data <- make_v5_supervision_fixture()

    fit <- NULL

    testthat::expect_warning(
      fit <- msma(
        X = data$X,
        Z = data$Z,
        comp = 2,
        lambdaX = 0.1,
        muX = 0,
        con4spv = c(1, -0.5),
        structure.method = "soft",
        gammaX = 0.1,
        niterS4 = 5,
        scaling = FALSE,
        intseed = 7
      ),
      "ignored because.*supervision term is inactive"
    )

    testthat::expect_s3_class(
      fit,
      "s4pca"
    )

    testthat::expect_identical(
      fit$structure.method,
      "soft"
    )

    testthat::expect_false(
      fit$supervision$active$X
    )

    testthat::expect_false(
      fit$supervision$active$Y
    )

    testthat::expect_null(
      fit$supervision$contrast_used
    )

    testthat::expect_identical(
      fit$supervision$contrast_requested,
      c(1, -0.5)
    )

    testthat::expect_identical(
      fit$supervision$reason,
      "muX_zero"
    )
  }
)
