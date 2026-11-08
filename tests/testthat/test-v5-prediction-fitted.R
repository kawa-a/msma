
read_m3b_fixture_for_m3c <- function() {
  readRDS(testthat::test_path("reference", "m3b-predict-msma-legacy-baseline.rds"))
}


testthat::test_that("default prediction remains the legacy M3-B baseline", {
  expected <- read_m3b_fixture_for_m3c()$pca_scaling_true
  set.seed(1301)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_equal(predict(fit, newX = Xnew), expected, tolerance = 1e-12)
  testthat::expect_equal(
    predict(
      fit,
      newX = Xnew,
      preprocessing = "legacy"
    ),
    expected,
    tolerance = 1e-12
  )
})


testthat::test_that("fitted PCA prediction uses saved preprocessing", {
  set.seed(1401)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = Xnew, preprocessing = "fitted")
  processed <- .apply_msma_preprocessor(fit$preprocess, Xnew, side = "X")
  processed[is.na(processed)] <- 0
  expected_score <- processed %*% fit$wbX[[1]]
  expected_x <- cbind(expected_score) %*% project(processed, expected_score)

  testthat::expect_equal(observed$sbX[[1]], expected_score, tolerance = 1e-15)
  testthat::expect_equal(observed$X[[1]], expected_x, tolerance = 1e-15)
  testthat::expect_null(observed$Y)
  testthat::expect_null(observed$sbY)
})


testthat::test_that("fitted single-row prediction uses fitted scale", {
  set.seed(1402)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  target <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
  colnames(target) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = target, preprocessing = "fitted")
  processed <- .apply_msma_preprocessor(fit$preprocess, target, side = "X")
  expected_score <- processed %*% fit$wbX[[1]]

  testthat::expect_equal(observed$sbX[[1]], expected_score, tolerance = 1e-15)
  testthat::expect_false(all(as.numeric(processed) %in% c(-1, 1)))
})


testthat::test_that("fitted preprocessing makes component scores independent of batch composition", {
  set.seed(1403)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  target <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
  colnames(target) <- colnames(X)
  batch_a <- rbind(target, matrix(stats::rnorm(54), nrow = 9, ncol = 6))
  batch_b <- rbind(target, matrix(stats::rnorm(54, mean = 20, sd = 5), nrow = 9, ncol = 6))
  colnames(batch_a) <- colnames(X)
  colnames(batch_b) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  pred_a <- predict(fit, newX = batch_a, preprocessing = "fitted")
  pred_b <- predict(fit, newX = batch_b, preprocessing = "fitted")

  testthat::expect_equal(pred_a$sbX[[1]][1, ], pred_b$sbX[[1]][1, ], tolerance = 1e-15)

  processed_a <- .apply_msma_preprocessor(
    fit$preprocess,
    batch_a,
    side = "X"
  )

  processed_b <- .apply_msma_preprocessor(
    fit$preprocess,
    batch_b,
    side = "X"
  )

  expected_x_a <- cbind(pred_a$sbX[[1]]) %*%
    project(processed_a, pred_a$sbX[[1]])

  expected_x_b <- cbind(pred_b$sbX[[1]]) %*%
    project(processed_b, pred_b$sbX[[1]])

  testthat::expect_equal(
    pred_a$X[[1]],
    expected_x_a,
    tolerance = 1e-15
  )

  testthat::expect_equal(
    pred_b$X[[1]],
    expected_x_b,
    tolerance = 1e-15
  )

  testthat::expect_false(
    isTRUE(
      all.equal(
        pred_a$X[[1]][1, ],
        pred_b$X[[1]][1, ],
        tolerance = 1e-15
      )
    )
  )
})


testthat::test_that("fitted prediction rejects reordered columns", {
  set.seed(1404)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
  reordered <- Xnew[, rev(seq_len(ncol(Xnew))), drop = FALSE]

  testthat::expect_error(
    predict(fit, newX = reordered, preprocessing = "fitted"),
    "Column names or column order"
  )
})


testthat::test_that("fitted multiblock prediction preserves block names", {
  set.seed(1405)
  X <- list(
    block1 = matrix(stats::rnorm(120), nrow = 30, ncol = 4),
    block2 = matrix(stats::rnorm(90), nrow = 30, ncol = 3)
  )
  colnames(X[[1]]) <- paste0("a", seq_len(ncol(X[[1]])))
  colnames(X[[2]]) <- paste0("b", seq_len(ncol(X[[2]])))
  Xnew <- lapply(X, function(x) x[1:7, , drop = FALSE])
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = Xnew, preprocessing = "fitted")
  testthat::expect_named(observed$X, names(Xnew))
  testthat::expect_named(observed$sbX, names(Xnew))
})


testthat::test_that("fitted PLS prediction uses saved X and Y preprocessing", {
  set.seed(1406)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  Y <- matrix(stats::rnorm(120), nrow = 30, ncol = 4)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  Ynew <- matrix(stats::rnorm(32), nrow = 8, ncol = 4)
  colnames(Xnew) <- colnames(X)
  colnames(Ynew) <- colnames(Y)
  fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = Xnew, newY = Ynew, preprocessing = "fitted")
  px <- .apply_msma_preprocessor(fit$preprocess, Xnew, side = "X")
  py <- .apply_msma_preprocessor(fit$preprocess, Ynew, side = "Y")
  sx <- px %*% fit$wbX[[1]]
  sy <- py %*% fit$wbY[[1]]

  testthat::expect_equal(observed$sbX[[1]], sx, tolerance = 1e-15)
  testthat::expect_equal(observed$sbY[[1]], sy, tolerance = 1e-15)
})


testthat::test_that("fitted PLS requires newY explicitly", {
  set.seed(1407)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  Y <- matrix(stats::rnorm(120), nrow = 30, ncol = 4)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_error(
    predict(fit, newX = Xnew, preprocessing = "fitted"),
    "newY.*must be supplied"
  )
})


testthat::test_that("fitted mode rejects Version 4-like fits without metadata", {
  set.seed(1408)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
  fit$preprocess <- NULL

  testthat::expect_error(
    predict(fit, newX = Xnew, preprocessing = "fitted"),
    "Version 5 fit"
  )
})


testthat::test_that("prediction preprocessing mode is validated", {
  set.seed(1409)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  testthat::expect_error(
    predict(fit, newX = Xnew, preprocessing = "unknown"),
    "arg"
  )
})
