
testthat::test_that(
  "preprocessing metadata has a stable Version 5 contract",
  {
    prep <- .new_msma_preprocess(
      engine = "legacy",
      scaling = TRUE
    )

    testthat::expect_s3_class(prep, "msma_preprocess")
    testthat::expect_named(
      prep,
      c("engine", "scaling", "X", "Y", "Z")
    )
    testthat::expect_identical(prep$engine, "legacy")
    testthat::expect_identical(prep$scaling, TRUE)

    testthat::expect_named(
      prep$X,
      c("centered", "scaled", "center", "scale", "missing_method")
    )
    testthat::expect_named(
      prep$Y,
      c("centered", "scaled", "center", "scale", "missing_method")
    )
    testthat::expect_named(
      prep$Z,
      c("method", "centered", "scaled", "center", "scale")
    )
  }
)


testthat::test_that(
  "new preprocessing metadata is initially empty",
  {
    prep <- .new_msma_preprocess(
      engine = "soft",
      scaling = TRUE
    )

    testthat::expect_null(prep$X$centered)
    testthat::expect_null(prep$X$scaled)
    testthat::expect_null(prep$X$center)
    testthat::expect_null(prep$X$scale)
    testthat::expect_null(prep$X$missing_method)

    testthat::expect_null(prep$Y$centered)
    testthat::expect_null(prep$Y$scaled)
    testthat::expect_null(prep$Y$center)
    testthat::expect_null(prep$Y$scale)
    testthat::expect_null(prep$Y$missing_method)

    testthat::expect_identical(prep$Z$method, "none")
    testthat::expect_null(prep$Z$centered)
    testthat::expect_null(prep$Z$scaled)
    testthat::expect_null(prep$Z$center)
    testthat::expect_null(prep$Z$scale)
  }
)


testthat::test_that(
  "preprocessing metadata supports all dispatcher engines",
  {
    engines <- c("legacy", "extended", "soft", "exclusive")

    objects <- lapply(
      engines,
      function(engine) {
        .new_msma_preprocess(
          engine = engine,
          scaling = FALSE
        )
      }
    )

    observed <- vapply(
      objects,
      `[[`,
      character(1),
      "engine"
    )

    testthat::expect_identical(observed, engines)
  }
)


testthat::test_that(
  "scale metadata distinguishes centering from scaling",
  {
    set.seed(604)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)

    scaled_X <- scale(X, scale = TRUE)
    centered_X <- scale(X, scale = FALSE)

    scaled_metadata <- .extract_msma_scale_metadata(attributes(scaled_X))
    centered_metadata <- .extract_msma_scale_metadata(attributes(centered_X))

    testthat::expect_true(scaled_metadata$centered)
    testthat::expect_true(scaled_metadata$scaled)
    testthat::expect_true(centered_metadata$centered)
    testthat::expect_false(centered_metadata$scaled)

    testthat::expect_equal(
      as.numeric(scaled_metadata$center),
      as.numeric(colMeans(X)),
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
    testthat::expect_equal(
      as.numeric(centered_metadata$center),
      as.numeric(colMeans(X)),
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
    testthat::expect_null(centered_metadata$scale)
  }
)


testthat::test_that(
  "NULL scale metadata produces no transformation",
  {
    metadata <- .extract_msma_scale_metadata(NULL)

    testthat::expect_identical(metadata$centered, FALSE)
    testthat::expect_identical(metadata$scaled, FALSE)
    testthat::expect_null(metadata$center)
    testthat::expect_null(metadata$scale)
  }
)


testthat::test_that(
  "legacy scaling FALSE means centering without scaling",
  {
    set.seed(603)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)

    fit <- msma(
      X = X,
      comp = 2,
      scaling = FALSE,
      intseed = 3
    )

    prep <- .build_msma_preprocess(
      fit = fit,
      engine = "legacy",
      scaling = FALSE
    )

    testthat::expect_true(prep$X$centered[[1]])
    testthat::expect_false(prep$X$scaled[[1]])
    testthat::expect_equal(
      as.numeric(prep$X$center[[1]]),
      as.numeric(colMeans(X)),
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
    testthat::expect_null(prep$X$scale[[1]])
    testthat::expect_identical(
      prep$X$missing_method,
      "replace_with_zero_after_scaling"
    )
    testthat::expect_identical(prep$Y$missing_method, "not_used")
  }
)


testthat::test_that(
  "legacy scaling TRUE records centering and scaling",
  {
    set.seed(605)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)

    fit <- msma(
      X = X,
      comp = 2,
      scaling = TRUE,
      intseed = 3
    )

    prep <- .build_msma_preprocess(
      fit = fit,
      engine = "legacy",
      scaling = TRUE
    )

    expected_scale <- apply(
      sweep(X, 2, colMeans(X), "-"),
      2,
      stats::sd
    )

    testthat::expect_true(prep$X$centered[[1]])
    testthat::expect_true(prep$X$scaled[[1]])
    testthat::expect_equal(
      as.numeric(prep$X$center[[1]]),
      as.numeric(colMeans(X)),
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
    testthat::expect_equal(
      as.numeric(prep$X$scale[[1]]),
      as.numeric(expected_scale),
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
    testthat::expect_identical(
      prep$Z$method,
      "legacy_scale_if_supplied"
    )
  }
)


testthat::test_that(
  "legacy PLS metadata includes X and Y transformations",
  {
    set.seed(607)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    Y <- matrix(stats::rnorm(80), nrow = 20, ncol = 4)

    fit <- msma(
      X = X,
      Y = Y,
      comp = 2,
      scaling = TRUE,
      intseed = 3
    )

    prep <- .build_msma_preprocess(
      fit = fit,
      engine = "legacy",
      scaling = TRUE
    )

    testthat::expect_true(prep$X$centered[[1]])
    testthat::expect_true(prep$X$scaled[[1]])
    testthat::expect_true(prep$Y$centered[[1]])
    testthat::expect_true(prep$Y$scaled[[1]])
    testthat::expect_equal(
      as.numeric(prep$Y$center[[1]]),
      as.numeric(colMeans(Y)),
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
    testthat::expect_identical(
      prep$Y$missing_method,
      "replace_with_zero_after_scaling"
    )
  }
)


testthat::test_that(
  "extended scaling FALSE records centering without scaling",
  {
    set.seed(608)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)

    fit <- msma(
      X = X,
      comp = 2,
      sprmethod = "NMF",
      scaling = FALSE,
      intseed = 3
    )

    prep <- .build_msma_preprocess(
      fit = fit,
      engine = "extended",
      scaling = FALSE
    )

    testthat::expect_true(prep$X$centered[[1]])
    testthat::expect_false(prep$X$scaled[[1]])
    testthat::expect_equal(
      as.numeric(prep$X$center[[1]]),
      as.numeric(colMeans(X)),
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
    testthat::expect_null(prep$X$scale[[1]])
    testthat::expect_identical(prep$X$missing_method, "engine_specific")
    testthat::expect_identical(prep$Z$method, "engine_specific")
  }
)


testthat::test_that(
  "structured scaling TRUE uses existing fit fields",
  {
    set.seed(606)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    Z <- as.numeric(scale(X[, 1] + stats::rnorm(20)))

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

    prep <- .build_msma_preprocess(
      fit = fit,
      engine = "soft",
      scaling = TRUE
    )

    testthat::expect_identical(prep$X$center, fit$Xcenter)
    testthat::expect_identical(prep$X$scale, fit$Xscale)
    testthat::expect_true(prep$X$centered)
    testthat::expect_true(prep$X$scaled)
    testthat::expect_identical(prep$X$missing_method, "none")
    testthat::expect_identical(prep$Z$method, "unchanged")
  }
)


testthat::test_that(
  "structured scaling FALSE records no transformation",
  {
    set.seed(609)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    Z <- as.numeric(scale(X[, 1] + stats::rnorm(20)))

    fit <- msma(
      X = X,
      Z = Z,
      comp = 2,
      lambdaX = 0.1,
      muX = 0.8,
      structure.method = "soft",
      gammaX = 0.1,
      niterS4 = 5,
      scaling = FALSE,
      intseed = 4
    )

    prep <- .build_msma_preprocess(
      fit = fit,
      engine = "soft",
      scaling = FALSE
    )

    testthat::expect_false(prep$X$centered)
    testthat::expect_false(prep$X$scaled)
    testthat::expect_equal(
      prep$X$center,
      rep(0, ncol(X)),
      tolerance = 0,
      ignore_attr = TRUE
    )
    testthat::expect_equal(
      prep$X$scale,
      rep(1, ncol(X)),
      tolerance = 0,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "exclusive metadata uses structured fit fields",
  {
    set.seed(610)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    Z <- as.numeric(scale(X[, 1] + stats::rnorm(20)))

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

    prep <- .build_msma_preprocess(
      fit = fit,
      engine = "exclusive",
      scaling = TRUE
    )

    testthat::expect_identical(prep$engine, "exclusive")
    testthat::expect_identical(prep$X$center, fit$Xcenter)
    testthat::expect_identical(prep$X$scale, fit$Xscale)
    testthat::expect_true(prep$X$centered)
    testthat::expect_true(prep$X$scaled)
  }
)


testthat::test_that(
  "unknown preprocessing engine fails clearly",
  {
    testthat::expect_error(
      .build_msma_preprocess(
        fit = list(),
        engine = "unknown",
        scaling = TRUE
      ),
      "Unknown preprocessing engine"
    )
  }
)


testthat::test_that(
  "public fits receive preprocessing metadata for all engines",
  {
    set.seed(701)

    X <- matrix(
      stats::rnorm(120),
      nrow = 20,
      ncol = 6
    )

    Z <- as.numeric(
      scale(X[, 1] + stats::rnorm(20))
    )

    legacy_fit <- msma(
      X = X,
      comp = 2,
      scaling = FALSE,
      intseed = 3
    )

    extended_fit <- msma(
      X = X,
      comp = 2,
      sprmethod = "NMF",
      scaling = FALSE,
      intseed = 3
    )

    soft_fit <- msma(
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

    exclusive_fit <- msma(
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

    fits <- list(
      legacy = legacy_fit,
      extended = extended_fit,
      soft = soft_fit,
      exclusive = exclusive_fit
    )

    for (engine in names(fits)) {
      testthat::expect_s3_class(
        fits[[engine]]$preprocess,
        "msma_preprocess"
      )

      testthat::expect_identical(
        fits[[engine]]$preprocess$engine,
        engine
      )

      testthat::expect_identical(
        tail(names(fits[[engine]]), 1),
        "preprocess"
      )
    }
  }
)


testthat::test_that(
  "preprocessing metadata is additive to legacy results",
  {
    set.seed(702)

    X <- matrix(
      stats::rnorm(100),
      nrow = 20,
      ncol = 5
    )

    direct_fit <- .msma_default_legacy(
      X = X,
      comp = 2,
      scaling = FALSE,
      intseed = 3
    )

    public_fit <- msma(
      X = X,
      comp = 2,
      scaling = FALSE,
      intseed = 3
    )

    old_names <- names(direct_fit)

    testthat::expect_identical(
      names(public_fit)[seq_along(old_names)],
      old_names
    )

    comparable_fields <- setdiff(old_names, "call")

    for (field in comparable_fields) {
      testthat::expect_identical(
        public_fit[[field]],
        direct_fit[[field]],
        info = field
      )
    }

    testthat::expect_true(
      is.call(direct_fit$call)
    )

    testthat::expect_true(
      is.call(public_fit$call)
    )

    testthat::expect_false(
      identical(
        direct_fit$call,
        public_fit$call
      )
    )

    testthat::expect_identical(
      tail(names(public_fit), 1),
      "preprocess"
    )

    testthat::expect_s3_class(
      public_fit$preprocess,
      "msma_preprocess"
    )
  }
)
