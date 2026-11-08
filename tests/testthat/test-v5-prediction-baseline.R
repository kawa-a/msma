
read_m3b_fixture <- function() {
  readRDS(testthat::test_path("reference", "m3b-predict-msma-legacy-baseline.rds"))
}


expect_prediction_equal <- function(
  object,
  expected,
  tolerance = 1e-12
) {
  testthat::expect_equal(
    object,
    expected,
    tolerance = tolerance
  )
}

testthat::test_that("legacy PCA prediction with scaling TRUE matches baseline", {
  expected <- read_m3b_fixture()$pca_scaling_true
  set.seed(1301)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = Xnew)
  expect_prediction_equal(observed, expected)
  testthat::expect_named(observed, c("X", "sbX", "Y", "sbY"))
  testthat::expect_null(observed$Y)
  testthat::expect_null(observed$sbY)
})


testthat::test_that("legacy PCA prediction with scaling FALSE matches baseline", {
  expected <- read_m3b_fixture()$pca_scaling_false
  set.seed(1302)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = FALSE, intseed = 3)

  observed <- predict(fit, newX = Xnew)
  expect_prediction_equal(observed, expected)

  current <- scale(Xnew, center = fit$Xscale[[1]]$`scaled:center`)
  current <- apply(current, 2, function(x) { x[is.na(x)] <- 0; x })
  fitted <- .apply_msma_preprocessor(fit$preprocess, Xnew, side = "X")
  testthat::expect_false(isTRUE(all.equal(current, fitted, check.attributes = FALSE)))
})


testthat::test_that("legacy PLS X and Y prediction matches baseline", {
  expected <- read_m3b_fixture()$pls_xy
  set.seed(1303)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  Y <- matrix(stats::rnorm(120), nrow = 30, ncol = 4)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  Ynew <- matrix(stats::rnorm(32), nrow = 8, ncol = 4)
  colnames(Xnew) <- colnames(X)
  colnames(Ynew) <- colnames(Y)
  fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = Xnew, newY = Ynew)
  expect_prediction_equal(observed, expected)
  testthat::expect_length(observed$X, 1L)
  testthat::expect_length(observed$Y, 1L)
})


testthat::test_that("legacy PLS without newY keeps the indirect center error", {
  set.seed(1303)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  Y <- matrix(stats::rnorm(120), nrow = 30, ncol = 4)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  colnames(Y) <- paste0("y", seq_len(ncol(Y)))
  Xnew <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
  matrix(stats::rnorm(32), nrow = 8, ncol = 4)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

  observed <- try(predict(fit, newX = Xnew), silent = TRUE)
  testthat::expect_true(inherits(observed, "try-error"))
  testthat::expect_match(as.character(observed), "center", fixed = TRUE)
  testthat::expect_match(as.character(observed), "scale.default", fixed = TRUE)
})


testthat::test_that("single-row legacy prediction matches baseline and stays finite", {
  expected <- read_m3b_fixture()$single_row
  set.seed(1304)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  target <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
  colnames(target) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = target)
  testthat::expect_equal(
    observed$sbX,
    expected$sbX,
    tolerance = 1e-12
  )

  testthat::expect_identical(
    dim(observed$X[[1L]]),
    dim(expected$X[[1L]])
  )

  testthat::expect_true(
    all(is.finite(observed$X[[1L]]))
  )

  testthat::expect_true(
    all(is.finite(observed$sbX[[1L]]))
  )
  legacy_scaled <- scale(target, center = fit$Xscale[[1]]$`scaled:center`)
  testthat::expect_true(all(as.numeric(legacy_scaled) %in% c(-1, 1)))
})


testthat::test_that("legacy prediction remains batch dependent", {
  expected <- read_m3b_fixture()
  set.seed(1305)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  target <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
  colnames(target) <- colnames(X)
  batch_a <- rbind(target, matrix(stats::rnorm(54), nrow = 9, ncol = 6))
  batch_b <- rbind(target, matrix(stats::rnorm(54, mean = 20, sd = 5), nrow = 9, ncol = 6))
  colnames(batch_a) <- colnames(X)
  colnames(batch_b) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed_a <- predict(fit, newX = batch_a)
  observed_b <- predict(fit, newX = batch_b)
  expect_prediction_equal(observed_a, expected$batch_a)
  expect_prediction_equal(observed_b, expected$batch_b)
  testthat::expect_false(isTRUE(all.equal(observed_a$sbX[[1]][1, ], observed_b$sbX[[1]][1, ])))
  testthat::expect_false(isTRUE(all.equal(observed_a$X[[1]][1, ], observed_b$X[[1]][1, ])))
})


testthat::test_that("legacy prediction accepts reordered columns and changes results", {
  expected <- read_m3b_fixture()
  set.seed(1306)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
  reordered <- Xnew[, rev(seq_len(ncol(Xnew))), drop = FALSE]

  original_result <- predict(fit, newX = Xnew)
  reordered_result <- predict(fit, newX = reordered)
  expect_prediction_equal(original_result, expected$order_original)
  expect_prediction_equal(reordered_result, expected$order_reordered)
  testthat::expect_false(isTRUE(all.equal(original_result$sbX, reordered_result$sbX)))
})


testthat::test_that("legacy multiblock prediction loses block names", {
  expected <- read_m3b_fixture()$multiblock
  set.seed(1307)
  X <- list(
    block1 = matrix(stats::rnorm(120), nrow = 30, ncol = 4),
    block2 = matrix(stats::rnorm(90), nrow = 30, ncol = 3)
  )
  colnames(X[[1]]) <- paste0("a", seq_len(ncol(X[[1]])))
  colnames(X[[2]]) <- paste0("b", seq_len(ncol(X[[2]])))
  Xnew <- lapply(X, function(x) x[1:7, , drop = FALSE])
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = Xnew)
  expect_prediction_equal(observed, expected)
  testthat::expect_null(names(observed$X))
  testthat::expect_null(names(observed$sbX))
})


testthat::test_that("legacy prediction replaces transformed missing values with zero", {
  expected <- read_m3b_fixture()$missing
  set.seed(1308)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  Xnew[1, 2] <- NA_real_
  Xnew[4, 5] <- NA_real_
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

  observed <- predict(fit, newX = Xnew)
  expect_prediction_equal(observed, expected)
  testthat::expect_true(
    all(is.finite(observed$sbX[[1L]]))
  )

  testthat::expect_true(
    all(is.finite(observed$X[[1L]]))
  )
})


testthat::test_that("serialized legacy fit predicts identically", {
  expected <- read_m3b_fixture()$serialized
  set.seed(1301)
  X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
  colnames(X) <- paste0("x", seq_len(ncol(X)))
  Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
  colnames(Xnew) <- colnames(X)
  fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(fit, path)
  restored <- readRDS(path)

  observed <- predict(restored, newX = Xnew)
  expect_prediction_equal(observed, expected)
  testthat::expect_identical(observed, predict(fit, newX = Xnew))
})
