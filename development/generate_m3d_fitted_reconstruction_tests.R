# Generate M3-D fitted reconstruction tests.
# Run after integrate_m3d_fitted_reconstruction.R.

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))
test_file <- file.path("tests", "testthat", "test-v5-prediction-reconstruction.R")

test_code <- r'{
testthat::test_that("prediction reconstruction metadata has a stable contract", {
  set.seed(1501)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_s3_class(
    fit$prediction_reconstruction,
    "msma_prediction_reconstruction"
  )
  testthat::expect_named(
    fit$prediction_reconstruction,
    c("X", "Y", "scale", "method", "version")
  )
  testthat::expect_identical(fit$prediction_reconstruction$scale, "analysis")
  testthat::expect_identical(
    fit$prediction_reconstruction$method,
    "training-score-projection"
  )
  testthat::expect_identical(fit$prediction_reconstruction$version, 1L)
  testthat::expect_null(fit$prediction_reconstruction$Y)
  testthat::expect_identical(dim(fit$prediction_reconstruction$X[[1]]), c(2L, 6L))
})


testthat::test_that("PCA fitted coefficients equal training-score projection", {
  set.seed(1502)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
  analysis_x <- .apply_msma_preprocessor(fit$preprocess, X, side = "X")
  expected <- project(analysis_x, fit$sbX[[1]])

  testthat::expect_equal(
    fit$prediction_reconstruction$X[[1]],
    expected,
    tolerance = 1e-15
  )
})


testthat::test_that("PLS fitted coefficients are stored for X and Y", {
  set.seed(1503)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  Y <- matrix(stats::rnorm(160), nrow = 40, ncol = 4)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_identical(dim(fit$prediction_reconstruction$X[[1]]), c(2L, 6L))
  testthat::expect_identical(dim(fit$prediction_reconstruction$Y[[1]]), c(2L, 4L))
  testthat::expect_true(all(is.finite(fit$prediction_reconstruction$X[[1]])))
  testthat::expect_true(all(is.finite(fit$prediction_reconstruction$Y[[1]])))
})


testthat::test_that("multiblock fitted coefficients preserve block names", {
  set.seed(1504)
  X <- list(
    block1 = matrix(stats::rnorm(160), nrow = 40, ncol = 4),
    block2 = matrix(stats::rnorm(120), nrow = 40, ncol = 3)
  )
  colnames(X[[1]]) <- paste0("a", seq_len(ncol(X[[1]])))
  colnames(X[[2]]) <- paste0("b", seq_len(ncol(X[[2]])))
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_named(fit$prediction_reconstruction$X, names(X))
  testthat::expect_identical(dim(fit$prediction_reconstruction$X[[1]]), c(2L, 4L))
  testthat::expect_identical(dim(fit$prediction_reconstruction$X[[2]]), c(2L, 3L))
})


testthat::test_that("fitted reconstruction is independent of batch composition", {
  set.seed(1505)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  target <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
  colnames(target) <- colnames(X)
  batch_a <- rbind(target, matrix(stats::rnorm(54), nrow = 9, ncol = 6))
  batch_b <- rbind(target, matrix(stats::rnorm(54, mean = 20, sd = 5), nrow = 9, ncol = 6))
  colnames(batch_a) <- colnames(X)
  colnames(batch_b) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  pred_a <- predict(
    fit, newX = batch_a,
    preprocessing = "fitted", reconstruction = "fitted"
  )
  pred_b <- predict(
    fit, newX = batch_b,
    preprocessing = "fitted", reconstruction = "fitted"
  )

  testthat::expect_equal(pred_a$sbX[[1]][1, ], pred_b$sbX[[1]][1, ], tolerance = 1e-15)
  testthat::expect_equal(pred_a$X[[1]][1, ], pred_b$X[[1]][1, ], tolerance = 1e-15)
})


testthat::test_that("fitted reconstruction equals score times saved coefficients", {
  set.seed(1506)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted"
  )
  expected <- observed$sbX[[1]] %*% fit$prediction_reconstruction$X[[1]]
  testthat::expect_equal(observed$X[[1]], expected, tolerance = 1e-15)
})


testthat::test_that("batch reconstruction remains the M3-C default", {
  set.seed(1507)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  default_result <- predict(fit, newX = Xnew, preprocessing = "fitted")
  batch_result <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "batch"
  )
  fitted_result <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted"
  )

  testthat::expect_identical(default_result, batch_result)
  testthat::expect_false(isTRUE(all.equal(batch_result$X, fitted_result$X)))
  testthat::expect_identical(batch_result$sbX, fitted_result$sbX)
})


testthat::test_that("legacy preprocessing rejects fitted reconstruction", {
  set.seed(1508)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_error(
    predict(fit, newX = Xnew, reconstruction = "fitted"),
    "preprocessing.*fitted"
  )
})


testthat::test_that("Version 4-like fits reject fitted reconstruction clearly", {
  set.seed(1509)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
  fit$prediction_reconstruction <- NULL

  testthat::expect_error(
    predict(
      fit, newX = Xnew,
      preprocessing = "fitted", reconstruction = "fitted"
    ),
    "reconstruction metadata"
  )
})


testthat::test_that("fitted reconstruction survives RDS serialization", {
  set.seed(1510)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(fit, path)
  restored <- readRDS(path)

  expected <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted"
  )
  observed <- predict(
    restored, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted"
  )
  testthat::expect_identical(observed, expected)
  testthat::expect_identical(
    restored$prediction_reconstruction,
    fit$prediction_reconstruction
  )
})


testthat::test_that("PLS fitted reconstruction uses saved X and Y coefficients", {
  set.seed(1511)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  Y <- matrix(stats::rnorm(160), nrow = 40, ncol = 4)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  Ynew <- matrix(stats::rnorm(40), nrow = 10, ncol = 4)
  colnames(Xnew) <- colnames(X)
  colnames(Ynew) <- colnames(Y)
  fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(
    fit, newX = Xnew, newY = Ynew,
    preprocessing = "fitted", reconstruction = "fitted"
  )
  expected_x <- observed$sbX[[1]] %*% fit$prediction_reconstruction$X[[1]]
  expected_y <- observed$sbY[[1]] %*% fit$prediction_reconstruction$Y[[1]]

  testthat::expect_equal(observed$X[[1]], expected_x, tolerance = 1e-15)
  testthat::expect_equal(observed$Y[[1]], expected_y, tolerance = 1e-15)
})


testthat::test_that("reconstruction mode is validated", {
  set.seed(1512)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_error(
    predict(
      fit, newX = Xnew,
      preprocessing = "fitted", reconstruction = "unknown"
    ),
    "arg"
  )
})
}'

writeLines(strsplit(test_code, "\n", fixed = TRUE)[[1]], test_file, useBytes = TRUE)
syntax <- try(parse(file = test_file), silent = TRUE)
if (inherits(syntax, "try-error")) stop(as.character(syntax), call. = FALSE)
cat("Generated: ", normalizePath(test_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
