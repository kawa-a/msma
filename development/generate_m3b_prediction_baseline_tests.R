# M3-B: Generate legacy predict.msma() fixtures and regression tests.
# Run from the msma Version 5 repository root before changing predict.msma().

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))
source("src.r", local = globalenv())

out_dir <- file.path("development", "v5-prediction")
ref_dir <- file.path("tests", "testthat", "reference")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)

fixture_file <- file.path(ref_dir, "m3b-predict-msma-legacy-baseline.rds")
test_file <- file.path("tests", "testthat", "test-v5-prediction-baseline.R")
summary_file <- file.path(out_dir, "M3-B-PREDICTION-BASELINE.txt")

# PCA, scaling TRUE ----------------------------------------------------------
set.seed(1301)
X_true <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
colnames(X_true) <- paste0("x", seq_len(ncol(X_true)))
Xnew_true <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
colnames(Xnew_true) <- colnames(X_true)
fit_pca_true <- msma(X = X_true, comp = 2, scaling = TRUE, intseed = 3)
pca_true <- predict(fit_pca_true, newX = Xnew_true)

# PCA, scaling FALSE ---------------------------------------------------------
set.seed(1302)
X_false <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
colnames(X_false) <- paste0("x", seq_len(ncol(X_false)))
Xnew_false <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
colnames(Xnew_false) <- colnames(X_false)
fit_pca_false <- msma(X = X_false, comp = 2, scaling = FALSE, intseed = 3)
pca_false <- predict(fit_pca_false, newX = Xnew_false)

# PLS with X and Y -----------------------------------------------------------
set.seed(1303)
X_pls <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
Y_pls <- matrix(stats::rnorm(120), nrow = 30, ncol = 4)
colnames(X_pls) <- paste0("x", seq_len(ncol(X_pls)))
colnames(Y_pls) <- paste0("y", seq_len(ncol(Y_pls)))
Xnew_pls <- matrix(stats::rnorm(48), nrow = 8, ncol = 6)
Ynew_pls <- matrix(stats::rnorm(32), nrow = 8, ncol = 4)
colnames(Xnew_pls) <- colnames(X_pls)
colnames(Ynew_pls) <- colnames(Y_pls)
fit_pls <- msma(X = X_pls, Y = Y_pls, comp = 2, scaling = TRUE, intseed = 3)
pls_xy <- predict(fit_pls, newX = Xnew_pls, newY = Ynew_pls)
pls_without_y <- try(predict(fit_pls, newX = Xnew_pls), silent = TRUE)

# Single-row prediction ------------------------------------------------------
set.seed(1304)
X_single <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
colnames(X_single) <- paste0("x", seq_len(ncol(X_single)))
target_row <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
colnames(target_row) <- colnames(X_single)
fit_single <- msma(X = X_single, comp = 2, scaling = TRUE, intseed = 3)
single_row <- predict(fit_single, newX = target_row)

# Batch dependency -----------------------------------------------------------
set.seed(1305)
X_batch <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
colnames(X_batch) <- paste0("x", seq_len(ncol(X_batch)))
target_batch_row <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
colnames(target_batch_row) <- colnames(X_batch)
batch_a <- rbind(target_batch_row, matrix(stats::rnorm(54), nrow = 9, ncol = 6))
batch_b <- rbind(target_batch_row, matrix(stats::rnorm(54, mean = 20, sd = 5), nrow = 9, ncol = 6))
colnames(batch_a) <- colnames(X_batch)
colnames(batch_b) <- colnames(X_batch)
fit_batch <- msma(X = X_batch, comp = 2, scaling = TRUE, intseed = 3)
batch_prediction_a <- predict(fit_batch, newX = batch_a)
batch_prediction_b <- predict(fit_batch, newX = batch_b)

# Reordered columns ----------------------------------------------------------
set.seed(1306)
X_order <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
colnames(X_order) <- paste0("x", seq_len(ncol(X_order)))
Xnew_order <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
colnames(Xnew_order) <- colnames(X_order)
fit_order <- msma(X = X_order, comp = 2, scaling = TRUE, intseed = 3)
order_original <- predict(fit_order, newX = Xnew_order)
Xnew_reordered <- Xnew_order[, rev(seq_len(ncol(Xnew_order))), drop = FALSE]
order_reordered <- predict(fit_order, newX = Xnew_reordered)

# Named multiblock behavior --------------------------------------------------
set.seed(1307)
X_multi <- list(
  block1 = matrix(stats::rnorm(120), nrow = 30, ncol = 4),
  block2 = matrix(stats::rnorm(90), nrow = 30, ncol = 3)
)
colnames(X_multi[[1]]) <- paste0("a", seq_len(ncol(X_multi[[1]])))
colnames(X_multi[[2]]) <- paste0("b", seq_len(ncol(X_multi[[2]])))
Xnew_multi <- lapply(X_multi, function(x) x[1:7, , drop = FALSE])
fit_multi <- msma(X = X_multi, comp = 2, scaling = TRUE, intseed = 3)
multi_prediction <- predict(fit_multi, newX = Xnew_multi)

# Missing-value replacement -------------------------------------------------
set.seed(1308)
X_missing <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)
colnames(X_missing) <- paste0("x", seq_len(ncol(X_missing)))
Xnew_missing <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
colnames(Xnew_missing) <- colnames(X_missing)
Xnew_missing[1, 2] <- NA_real_
Xnew_missing[4, 5] <- NA_real_
fit_missing <- msma(X = X_missing, comp = 2, scaling = TRUE, intseed = 3)
missing_prediction <- predict(fit_missing, newX = Xnew_missing)

# RDS round-trip -------------------------------------------------------------
rds_fit_file <- tempfile(fileext = ".rds")
saveRDS(fit_pca_true, rds_fit_file)
fit_pca_true_restored <- readRDS(rds_fit_file)
unlink(rds_fit_file)
serialized_prediction <- predict(fit_pca_true_restored, newX = Xnew_true)

fixture <- list(
  metadata = list(
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    branch = system2("git", c("branch", "--show-current"), stdout = TRUE),
    commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    contract = "Legacy predict.msma behavior before M3-C"
  ),
  pca_scaling_true = pca_true,
  pca_scaling_false = pca_false,
  pls_xy = pls_xy,
  pls_without_y_error = as.character(pls_without_y),
  single_row = single_row,
  batch_a = batch_prediction_a,
  batch_b = batch_prediction_b,
  order_original = order_original,
  order_reordered = order_reordered,
  multiblock = multi_prediction,
  missing = missing_prediction,
  serialized = serialized_prediction
)

saveRDS(fixture, fixture_file, version = 3)

# Generate formal tests ------------------------------------------------------
test_code <- r'{
read_m3b_fixture <- function() {
  readRDS(testthat::test_path("reference", "m3b-predict-msma-legacy-baseline.rds"))
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
  testthat::expect_identical(observed, expected)
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
  testthat::expect_identical(observed, expected)

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
  testthat::expect_identical(observed, expected)
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
  testthat::expect_identical(observed, expected)
  testthat::expect_true(all(is.finite(observed$sbX[[1]])))
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
  testthat::expect_identical(observed_a, expected$batch_a)
  testthat::expect_identical(observed_b, expected$batch_b)
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
  testthat::expect_identical(original_result, expected$order_original)
  testthat::expect_identical(reordered_result, expected$order_reordered)
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
  testthat::expect_identical(observed, expected)
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
  testthat::expect_identical(observed, expected)
  testthat::expect_true(all(is.finite(observed$sbX[[1]])))
  testthat::expect_true(all(is.finite(observed$X[[1]])))
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
  testthat::expect_identical(observed, expected)
  testthat::expect_identical(observed, predict(fit, newX = Xnew))
})
}'

writeLines(strsplit(test_code, "\n", fixed = TRUE)[[1]], test_file, useBytes = TRUE)
syntax <- try(parse(file = test_file), silent = TRUE)
if (inherits(syntax, "try-error")) stop(as.character(syntax), call. = FALSE)

# Human-readable fixture summary --------------------------------------------
capture.output(
  {
    cat("msma Version 5 M3-B predict.msma legacy baseline\n")
    cat("Date:", fixture$metadata$created, "\n")
    cat("Branch:", fixture$metadata$branch, "\n")
    cat("Commit:", fixture$metadata$commit, "\n\n")
    cat("Fixture file:", fixture_file, "\n")
    cat("Test file:", test_file, "\n\n")
    cat("Stored scenarios:\n")
    cat("* PCA scaling TRUE\n")
    cat("* PCA scaling FALSE\n")
    cat("* PLS with newX and newY\n")
    cat("* PLS without newY error\n")
    cat("* single-row prediction\n")
    cat("* batch dependency\n")
    cat("* reordered columns accepted\n")
    cat("* multiblock names lost\n")
    cat("* transformed missing values replaced by zero\n")
    cat("* saveRDS/readRDS prediction identity\n\n")
    cat("No predict.msma implementation was changed.\n")
  },
  file = summary_file
)

cat("Generated fixture: ", normalizePath(fixture_file, winslash = "/"), "\n", sep = "")
cat("Generated tests: ", normalizePath(test_file, winslash = "/"), "\n", sep = "")
cat("Generated summary: ", normalizePath(summary_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
