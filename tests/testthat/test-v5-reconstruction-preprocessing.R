testthat::test_that(
  "M2-E reconstruction baseline is preserved",
  {
    set.seed(901)

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
        0.8 * X[, 1] -
          0.4 * X[, 4] +
          stats::rnorm(30)
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
      niterS4 = 10,
      scaling = TRUE,
      intseed = 4
    )

    expected <- readRDS(
      testthat::test_path("reference", "m2e-reconstruction-baseline.rds")
    )

    observed <- list(
      training_analysis = s4pca_reconstruct(
        fit,
        scaled = TRUE
      ),

      training_original = s4pca_reconstruct(
        fit,
        scaled = FALSE
      ),

      new_analysis = s4pca_reconstruct(
        fit,
        newdata = Xnew,
        scaled = TRUE
      ),

      new_original = s4pca_reconstruct(
        fit,
        newdata = Xnew,
        scaled = FALSE
      )
    )

    for (result_name in names(expected)) {
      testthat::expect_equal(
        observed[[result_name]],
        expected[[result_name]],
        tolerance = 1e-12,
        ignore_attr = TRUE,
        info = result_name
      )
    }
  }
)


testthat::test_that(
  "Version 5 and Version 4-like fits reconstruct identically",
  {
    set.seed(902)

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

    for (scaled_value in c(TRUE, FALSE)) {
      observed_v5 <- s4pca_reconstruct(
        fit_v5,
        newdata = Xnew,
        scaled = scaled_value
      )

      observed_v4 <- s4pca_reconstruct(
        fit_v4_like,
        newdata = Xnew,
        scaled = scaled_value
      )

      testthat::expect_equal(
        observed_v5,
        observed_v4,
        tolerance = 1e-12,
        ignore_attr = TRUE
      )
    }
  }
)


testthat::test_that(
  "Version 5 reconstruction rejects reordered columns",
  {
    set.seed(903)

    X <- matrix(
      stats::rnorm(180),
      nrow = 30,
      ncol = 6
    )

    colnames(X) <- paste0("x", seq_len(ncol(X)))

    Z <- as.numeric(
      scale(X[, 1] + stats::rnorm(30))
    )

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

    X_reordered <- X[
      ,
      rev(seq_len(ncol(X))),
      drop = FALSE
    ]

    testthat::expect_error(
      s4pca_reconstruct(
        fit,
        newdata = X_reordered,
        scaled = TRUE
      ),
      "Column names or column order"
    )
  }
)
