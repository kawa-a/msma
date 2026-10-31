testthat::test_that(
  "preprocessing metadata has a stable initial contract",
  {
    prep <- .new_msma_preprocess(
      engine = "legacy",
      scaling = TRUE
    )

    testthat::expect_s3_class(
      prep,
      "msma_preprocess"
    )

    testthat::expect_identical(
      prep$engine,
      "legacy"
    )

    testthat::expect_identical(
      prep$scaling,
      TRUE
    )

    testthat::expect_named(
      prep,
      c("engine", "scaling", "X", "Y", "Z")
    )

    testthat::expect_named(
      prep$X,
      c("center", "scale", "missing_method")
    )

    testthat::expect_named(
      prep$Y,
      c("center", "scale", "missing_method")
    )

    testthat::expect_named(
      prep$Z,
      c("method", "center", "scale")
    )
  }
)

testthat::test_that(
  "preprocessing metadata supports all dispatcher engines",
  {
    engines <- c(
      "legacy",
      "extended",
      "soft",
      "exclusive"
    )

    prep <- lapply(
      engines,
      function(engine) {
        .new_msma_preprocess(
          engine = engine,
          scaling = FALSE
        )
      }
    )

    observed <- vapply(
      prep,
      `[[`,
      character(1),
      "engine"
    )

    testthat::expect_identical(
      observed,
      engines
    )
  }
)

testthat::test_that(
  "preprocessing metadata is initially empty",
  {
    prep <- .new_msma_preprocess(
      engine = "soft",
      scaling = TRUE
    )

    testthat::expect_null(prep$X$center)
    testthat::expect_null(prep$X$scale)
    testthat::expect_null(prep$X$missing_method)

    testthat::expect_null(prep$Y$center)
    testthat::expect_null(prep$Y$scale)
    testthat::expect_null(prep$Y$missing_method)

    testthat::expect_identical(
      prep$Z$method,
      "none"
    )

    testthat::expect_null(prep$Z$center)
    testthat::expect_null(prep$Z$scale)
  }
)
