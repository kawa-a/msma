testthat::test_that(
  "Version 5 preprocessing metadata survives RDS serialization",
  {
    set.seed(1101)

    X <- matrix(
      stats::rnorm(120),
      nrow = 20,
      ncol = 6
    )

    colnames(X) <- paste0(
      "x",
      seq_len(ncol(X))
    )

    fit <- msma(
      X = X,
      comp = 2,
      scaling = TRUE,
      intseed = 3
    )

    rds_file <- tempfile(
      fileext = ".rds"
    )

    on.exit(
      unlink(rds_file),
      add = TRUE
    )

    saveRDS(
      fit,
      rds_file
    )

    restored_fit <- readRDS(
      rds_file
    )

    testthat::expect_identical(
      restored_fit,
      fit
    )

    testthat::expect_s3_class(
      restored_fit$preprocess,
      "msma_preprocess"
    )

    testthat::expect_identical(
      restored_fit$preprocess,
      fit$preprocess
    )
  }
)


testthat::test_that(
  "serialized fit applies the same preprocessing",
  {
    set.seed(1102)

    X <- matrix(
      stats::rnorm(120),
      nrow = 20,
      ncol = 6
    )

    colnames(X) <- paste0(
      "x",
      seq_len(ncol(X))
    )

    Xnew <- matrix(
      stats::rnorm(48),
      nrow = 8,
      ncol = 6
    )

    colnames(Xnew) <- colnames(X)

    fit <- msma(
      X = X,
      comp = 2,
      scaling = TRUE,
      intseed = 3
    )

    rds_file <- tempfile(
      fileext = ".rds"
    )

    on.exit(
      unlink(rds_file),
      add = TRUE
    )

    saveRDS(fit, rds_file)
    restored_fit <- readRDS(rds_file)

    original_transformed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = Xnew,
      side = "X"
    )

    restored_transformed <- .apply_msma_preprocessor(
      preprocess = restored_fit$preprocess,
      newdata = Xnew,
      side = "X"
    )

    testthat::expect_identical(
      restored_transformed,
      original_transformed
    )
  }
)


testthat::test_that(
  "serialized structured fit reconstructs identically",
  {
    set.seed(1103)

    X <- matrix(
      stats::rnorm(180),
      nrow = 30,
      ncol = 6
    )

    colnames(X) <- paste0(
      "x",
      seq_len(ncol(X))
    )

    Z <- as.numeric(
      scale(
        X[, 1] + stats::rnorm(30)
      )
    )

    Xnew <- matrix(
      stats::rnorm(60),
      nrow = 10,
      ncol = 6
    )

    colnames(Xnew) <- colnames(X)

    fit <- msma(
      X = X,
      Z = Z,
      comp = 2,
      lambdaX = 0.1,
      muX = 0.8,
      structure.method = "soft",
      gammaX = 0.1,
      niterS4 = 5,
      scaling = TRUE,
      intseed = 4
    )

    rds_file <- tempfile(
      fileext = ".rds"
    )

    on.exit(
      unlink(rds_file),
      add = TRUE
    )

    saveRDS(fit, rds_file)
    restored_fit <- readRDS(rds_file)

    for (scaled_value in c(TRUE, FALSE)) {
      testthat::expect_identical(
        s4pca_reconstruct(
          restored_fit,
          newdata = Xnew,
          scaled = scaled_value
        ),
        s4pca_reconstruct(
          fit,
          newdata = Xnew,
          scaled = scaled_value
        )
      )
    }
  }
)


testthat::test_that(
  "serialized exclusive fit retains preprocessing metadata",
  {
    set.seed(1104)

    X <- matrix(
      stats::rnorm(180),
      nrow = 30,
      ncol = 6
    )

    colnames(X) <- paste0(
      "x",
      seq_len(ncol(X))
    )

    Z <- as.numeric(
      scale(
        X[, 1] + stats::rnorm(30)
      )
    )

    fit <- msma(
      X = X,
      Z = Z,
      comp = 2,
      lambdaX = 0.1,
      muX = 0.8,
      structure.method = "exclusive",
      gammaX = 0,
      niterS4 = 5,
      scaling = TRUE,
      intseed = 4
    )

    rds_file <- tempfile(
      fileext = ".rds"
    )

    on.exit(
      unlink(rds_file),
      add = TRUE
    )

    saveRDS(fit, rds_file)
    restored_fit <- readRDS(rds_file)

    testthat::expect_identical(
      restored_fit$preprocess$engine,
      "exclusive"
    )

    testthat::expect_identical(
      restored_fit$preprocess,
      fit$preprocess
    )
  }
)


testthat::test_that(
  "serialized multiblock fit preserves block names",
  {
    set.seed(1105)

    X <- list(
      block1 = matrix(
        stats::rnorm(80),
        nrow = 20,
        ncol = 4
      ),

      block2 = matrix(
        stats::rnorm(60),
        nrow = 20,
        ncol = 3
      )
    )

    colnames(X[[1]]) <- paste0(
      "a",
      seq_len(ncol(X[[1]]))
    )

    colnames(X[[2]]) <- paste0(
      "b",
      seq_len(ncol(X[[2]]))
    )

    fit <- msma(
      X = X,
      comp = 2,
      scaling = TRUE,
      intseed = 3
    )

    rds_file <- tempfile(
      fileext = ".rds"
    )

    on.exit(
      unlink(rds_file),
      add = TRUE
    )

    saveRDS(fit, rds_file)
    restored_fit <- readRDS(rds_file)

    transformed <- .apply_msma_preprocessor(
      preprocess = restored_fit$preprocess,
      newdata = X,
      side = "X"
    )

    inverted <- .invert_msma_preprocessor(
      preprocess = restored_fit$preprocess,
      data = transformed,
      side = "X"
    )

    testthat::expect_named(
      transformed,
      names(X)
    )

    testthat::expect_named(
      inverted,
      names(X)
    )

    testthat::expect_equal(
      inverted,
      X,
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "Version 4-like serialized fit keeps reconstruction fallback",
  {
    set.seed(1106)

    X <- matrix(
      stats::rnorm(180),
      nrow = 30,
      ncol = 6
    )

    colnames(X) <- paste0(
      "x",
      seq_len(ncol(X))
    )

    Z <- as.numeric(
      scale(
        X[, 1] + stats::rnorm(30)
      )
    )

    Xnew <- matrix(
      stats::rnorm(60),
      nrow = 10,
      ncol = 6
    )

    colnames(Xnew) <- colnames(X)

    fit_v5 <- msma(
      X = X,
      Z = Z,
      comp = 2,
      lambdaX = 0.1,
      muX = 0.8,
      structure.method = "soft",
      gammaX = 0.1,
      niterS4 = 5,
      scaling = TRUE,
      intseed = 4
    )

    fit_v4_like <- fit_v5
    fit_v4_like$preprocess <- NULL

    rds_file <- tempfile(
      fileext = ".rds"
    )

    on.exit(
      unlink(rds_file),
      add = TRUE
    )

    saveRDS(fit_v4_like, rds_file)
    restored_v4_like <- readRDS(rds_file)

    testthat::expect_null(
      restored_v4_like$preprocess
    )

    for (scaled_value in c(TRUE, FALSE)) {
      testthat::expect_equal(
        s4pca_reconstruct(
          restored_v4_like,
          newdata = Xnew,
          scaled = scaled_value
        ),
        s4pca_reconstruct(
          fit_v5,
          newdata = Xnew,
          scaled = scaled_value
        ),
        tolerance = 1e-12,
        ignore_attr = TRUE
      )
    }
  }
)
