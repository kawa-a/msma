# Generate M3-E prediction output-scale tests.
# Run after integrate_m3e_prediction_output_scale.R.

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))
test_file <- file.path("tests", "testthat", "test-v5-prediction-output-scale.R")

test_code <- r'{
testthat::test_that("analysis scale remains the fitted prediction default", {
  set.seed(1601)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  default_result <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted"
  )
  explicit_result <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "analysis"
  )
  testthat::expect_identical(default_result, explicit_result)
})


testthat::test_that("PCA original-scale output equals inverse preprocessing", {
  set.seed(1602)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  analysis <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "analysis"
  )
  original <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )
  expected <- .invert_msma_preprocessor(
    fit$preprocess, analysis$X, side = "X"
  )

  testthat::expect_equal(original$X, expected, tolerance = 1e-15)
  testthat::expect_identical(original$sbX, analysis$sbX)
  testthat::expect_null(original$Y)
  testthat::expect_null(original$sbY)
})


testthat::test_that("batch reconstruction can also be returned on original scale", {
  set.seed(1603)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  analysis <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "batch",
    output_scale = "analysis"
  )
  original <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "batch",
    output_scale = "original"
  )
  expected <- .invert_msma_preprocessor(
    fit$preprocess, analysis$X, side = "X"
  )

  testthat::expect_equal(original$X, expected, tolerance = 1e-15)
  testthat::expect_identical(original$sbX, analysis$sbX)
})


testthat::test_that("PLS original-scale output inverts X and Y independently", {
  set.seed(1604)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  Y <- matrix(stats::rnorm(160), nrow = 40, ncol = 4)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  Ynew <- matrix(stats::rnorm(40), nrow = 10, ncol = 4)
  colnames(Xnew) <- colnames(X)
  colnames(Ynew) <- colnames(Y)
  fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

  analysis <- predict(
    fit, newX = Xnew, newY = Ynew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "analysis"
  )
  original <- predict(
    fit, newX = Xnew, newY = Ynew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )

  expected_x <- .invert_msma_preprocessor(
    fit$preprocess, analysis$X, side = "X"
  )
  expected_y <- .invert_msma_preprocessor(
    fit$preprocess, analysis$Y, side = "Y"
  )

  testthat::expect_equal(original$X, expected_x, tolerance = 1e-15)
  testthat::expect_equal(original$Y, expected_y, tolerance = 1e-15)
  testthat::expect_identical(original$sbX, analysis$sbX)
  testthat::expect_identical(original$sbY, analysis$sbY)
})


testthat::test_that("multiblock original-scale output preserves block names", {
  set.seed(1605)
  X <- list(
    block1 = matrix(stats::rnorm(160), nrow = 40, ncol = 4),
    block2 = matrix(stats::rnorm(120), nrow = 40, ncol = 3)
  )
  colnames(X[[1]]) <- paste0("a", seq_len(ncol(X[[1]])))
  colnames(X[[2]]) <- paste0("b", seq_len(ncol(X[[2]])))
  Xnew <- lapply(X, function(x) x[1:10, , drop = FALSE])
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )

  testthat::expect_named(observed$X, names(Xnew))
  testthat::expect_named(observed$sbX, names(Xnew))
  testthat::expect_named(observed$X[[1]], NULL, ignore.order = FALSE)
  testthat::expect_identical(colnames(observed$X[[1]]), colnames(X[[1]]))
  testthat::expect_identical(colnames(observed$X[[2]]), colnames(X[[2]]))
})


testthat::test_that("original-scale fitted reconstruction remains batch independent", {
  set.seed(1606)
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
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )
  pred_b <- predict(
    fit, newX = batch_b,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )

  testthat::expect_equal(pred_a$sbX[[1]][1, ], pred_b$sbX[[1]][1, ], tolerance = 1e-15)
  testthat::expect_equal(pred_a$X[[1]][1, ], pred_b$X[[1]][1, ], tolerance = 1e-15)
})


testthat::test_that("scaling FALSE original output applies centering inverse only", {
  set.seed(1607)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = FALSE, intseed = 3)

  analysis <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "analysis"
  )
  original <- predict(
    fit, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )
  expected <- sweep(
    analysis$X[[1]], 2,
    fit$preprocess$X$center[[1]], "+"
  )

  testthat::expect_equal(original$X[[1]], expected, tolerance = 1e-15)
})


testthat::test_that("legacy preprocessing rejects original-scale output", {
  set.seed(1608)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_error(
    predict(fit, newX = Xnew, output_scale = "original"),
    "Original-scale output.*preprocessing.*fitted"
  )
})


testthat::test_that("output scale is validated", {
  set.seed(1609)
  X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_error(
    predict(
      fit, newX = Xnew,
      preprocessing = "fitted", output_scale = "unknown"
    ),
    "arg"
  )
})


testthat::test_that("original-scale output survives RDS serialization", {
  set.seed(1610)
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
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )
  observed <- predict(
    restored, newX = Xnew,
    preprocessing = "fitted", reconstruction = "fitted",
    output_scale = "original"
  )
  testthat::expect_identical(observed, expected)
})
}'

writeLines(strsplit(test_code, "\n", fixed = TRUE)[[1]], test_file, useBytes = TRUE)
syntax <- try(parse(file = test_file), silent = TRUE)
if (inherits(syntax, "try-error")) stop(as.character(syntax), call. = FALSE)
cat("Generated: ", normalizePath(test_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
