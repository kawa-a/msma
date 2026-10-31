
testthat::test_that(
  "legacy centering-only preprocessing round-trips",
  {
    set.seed(1001)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    fit <- msma(X = X, comp = 2, scaling = FALSE, intseed = 3)

    transformed <- .apply_msma_preprocessor(fit$preprocess, X, side = "X")
    restored <- .invert_msma_preprocessor(fit$preprocess, transformed, side = "X")

    testthat::expect_equal(restored, X, tolerance = 1e-15, ignore_attr = TRUE)
  }
)


testthat::test_that(
  "legacy centered and scaled preprocessing round-trips",
  {
    set.seed(1002)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

    transformed <- .apply_msma_preprocessor(fit$preprocess, X, side = "X")
    restored <- .invert_msma_preprocessor(fit$preprocess, transformed, side = "X")

    testthat::expect_equal(restored, X, tolerance = 1e-15, ignore_attr = TRUE)
  }
)


testthat::test_that(
  "legacy PLS Y preprocessing round-trips",
  {
    set.seed(1003)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    Y <- matrix(stats::rnorm(80), nrow = 20, ncol = 4)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    colnames(Y) <- paste0("y", seq_len(ncol(Y)))
    fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

    transformed <- .apply_msma_preprocessor(fit$preprocess, Y, side = "Y")
    restored <- .invert_msma_preprocessor(fit$preprocess, transformed, side = "Y")

    testthat::expect_equal(restored, Y, tolerance = 1e-15, ignore_attr = TRUE)
  }
)


testthat::test_that(
  "structured preprocessing round-trips for scaling TRUE",
  {
    set.seed(1004)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    Z <- as.numeric(scale(X[, 1] + stats::rnorm(20)))
    fit <- msma(
      X = X, Z = Z, comp = 2, lambdaX = 0.1, muX = 0.8,
      structure.method = "soft", gammaX = 0.1, niterS4 = 5,
      scaling = TRUE, intseed = 4
    )

    transformed <- .apply_msma_preprocessor(fit$preprocess, X, side = "X")
    restored <- .invert_msma_preprocessor(fit$preprocess, transformed, side = "X")

    testthat::expect_equal(restored, X, tolerance = 1e-15, ignore_attr = TRUE)
  }
)


testthat::test_that(
  "structured preprocessing is identity for scaling FALSE",
  {
    set.seed(1005)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    Z <- as.numeric(scale(X[, 1] + stats::rnorm(20)))
    fit <- msma(
      X = X, Z = Z, comp = 2, lambdaX = 0.1, muX = 0.8,
      structure.method = "soft", gammaX = 0.1, niterS4 = 5,
      scaling = FALSE, intseed = 4
    )

    restored <- .invert_msma_preprocessor(fit$preprocess, X, side = "X")
    testthat::expect_identical(restored, X)
  }
)


testthat::test_that(
  "multiblock X preprocessing round-trips and preserves list structure",
  {
    set.seed(1006)
    X <- list(
      block1 = matrix(stats::rnorm(80), nrow = 20, ncol = 4),
      block2 = matrix(stats::rnorm(60), nrow = 20, ncol = 3)
    )
    colnames(X[[1]]) <- paste0("a", seq_len(ncol(X[[1]])))
    colnames(X[[2]]) <- paste0("b", seq_len(ncol(X[[2]])))
    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

    transformed <- .apply_msma_preprocessor(fit$preprocess, X, side = "X")
    restored <- .invert_msma_preprocessor(fit$preprocess, transformed, side = "X")

    testthat::expect_type(restored, "list")
    testthat::expect_named(restored, names(X))
    testthat::expect_equal(restored, X, tolerance = 1e-15, ignore_attr = TRUE)
  }
)


testthat::test_that(
  "inverse preprocessing safely handles invalid scale values",
  {
    prep <- .new_msma_preprocess(engine = "legacy", scaling = TRUE)
    prep$X$center <- list(c(a = 1, b = 2, c = 3))
    prep$X$scale <- list(c(a = 0, b = Inf, c = NA_real_))
    prep$X$centered <- TRUE
    prep$X$scaled <- TRUE

    X <- matrix(c(1, 2, 3, 2, 3, 4), nrow = 2, byrow = TRUE)
    colnames(X) <- c("a", "b", "c")
    observed <- .invert_msma_preprocessor(prep, X, side = "X")
    expected <- sweep(X, 2, c(a = 1, b = 2, c = 3), "+")

    testthat::expect_equal(observed, expected, tolerance = 0, ignore_attr = TRUE)
  }
)


testthat::test_that(
  "inverse preprocessing leaves Z unchanged",
  {
    prep <- .new_msma_preprocess(engine = "soft", scaling = TRUE)
    Z <- stats::rnorm(20)
    testthat::expect_identical(
      .invert_msma_preprocessor(prep, Z, side = "Z"),
      Z
    )
  }
)


testthat::test_that(
  "inverse preprocessing validates column order",
  {
    set.seed(1007)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
    transformed <- .apply_msma_preprocessor(fit$preprocess, X, side = "X")
    transformed <- transformed[, rev(seq_len(ncol(transformed))), drop = FALSE]

    testthat::expect_error(
      .invert_msma_preprocessor(fit$preprocess, transformed, side = "X"),
      "Column names or column order"
    )
  }
)


testthat::test_that(
  "M2-F reconstruction remains equal to the M2-E baseline",
  {
    set.seed(901)
    X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    Z <- as.numeric(scale(0.8 * X[, 1] - 0.4 * X[, 4] + stats::rnorm(30)))
    Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
    colnames(Xnew) <- colnames(X)
    fit <- msma(
      X = X, Z = Z, comp = 2, lambdaX = 0.1, muX = 0.8,
      structure.method = "soft", gammaX = 0.1, niterS4 = 10,
      scaling = TRUE, intseed = 4
    )

    expected <- readRDS(testthat::test_path("reference", "m2e-reconstruction-baseline.rds"))
    observed <- list(
      training_analysis = s4pca_reconstruct(fit, scaled = TRUE),
      training_original = s4pca_reconstruct(fit, scaled = FALSE),
      new_analysis = s4pca_reconstruct(fit, newdata = Xnew, scaled = TRUE),
      new_original = s4pca_reconstruct(fit, newdata = Xnew, scaled = FALSE)
    )

    for (result_name in names(expected)) {
      testthat::expect_equal(
        observed[[result_name]], expected[[result_name]],
        tolerance = 1e-12, ignore_attr = TRUE, info = result_name
      )
    }
  }
)


testthat::test_that(
  "M2-F keeps the Version 4-like reconstruction fallback",
  {
    set.seed(1008)
    X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    Z <- as.numeric(scale(X[, 1] + stats::rnorm(30)))
    Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
    colnames(Xnew) <- colnames(X)
    fit_v5 <- msma(
      X = X, Z = Z, comp = 2, lambdaX = 0.1, muX = 0.8,
      structure.method = "soft", gammaX = 0.1, niterS4 = 5,
      scaling = TRUE, intseed = 4
    )
    fit_v4_like <- fit_v5
    fit_v4_like$preprocess <- NULL

    for (scaled_value in c(TRUE, FALSE)) {
      testthat::expect_equal(
        s4pca_reconstruct(fit_v5, Xnew, scaled = scaled_value),
        s4pca_reconstruct(fit_v4_like, Xnew, scaled = scaled_value),
        tolerance = 1e-12,
        ignore_attr = TRUE
      )
    }
  }
)
