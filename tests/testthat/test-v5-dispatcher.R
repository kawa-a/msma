testthat::test_that(
  "Version 5 resolver preserves Version 4 engine policy",
  {
    testthat::expect_identical(.resolve_msma_engine(), "legacy")

    testthat::expect_identical(
      .resolve_msma_engine(
        structure.method = "none",
        gammaX = 0
      ),
      "legacy"
    )

    testthat::expect_identical(
      .resolve_msma_engine(
        structure.method = "soft",
        gammaX = 0
      ),
      "soft"
    )

    testthat::expect_identical(
      .resolve_msma_engine(
        structure.method = "none",
        gammaX = 0.1
      ),
      "soft"
    )

    testthat::expect_identical(
      .resolve_msma_engine(
        structure.method = "exclusive",
        gammaX = 0
      ),
      "exclusive"
    )
  }
)

testthat::test_that(
  "Version 5 resolver identifies the extended engine",
  {
    Z_multi <- matrix(1:20, nrow = 10, ncol = 2)

    testthat::expect_identical(
      .resolve_msma_engine(sprmethod = "NMF"),
      "extended"
    )

    testthat::expect_identical(
      .resolve_msma_engine(Z = Z_multi),
      "extended"
    )

    testthat::expect_identical(
      .resolve_msma_engine(con4spv = list()),
      "extended"
    )
  }
)

testthat::test_that(
  "structured engines take precedence over extended inputs",
  {
    Z_multi <- matrix(1:20, nrow = 10, ncol = 2)

    testthat::expect_identical(
      .resolve_msma_engine(
        structure.method = "soft",
        Z = Z_multi
      ),
      "soft"
    )

    testthat::expect_identical(
      .resolve_msma_engine(
        structure.method = "exclusive",
        sprmethod = "NMF"
      ),
      "exclusive"
    )
  }
)

testthat::test_that(
  "Version 5 resolver rejects unknown structure methods",
  {
    testthat::expect_error(
      .resolve_msma_engine(
        structure.method = "unknown"
      ),
      "arg"
    )
  }
)

testthat::test_that(
  "invalid gammaX does not activate soft structure",
  {
    testthat::expect_identical(
      .resolve_msma_engine(gammaX = c(0.1, 0.2)),
      "legacy"
    )

    testthat::expect_identical(
      .resolve_msma_engine(gammaX = NA_real_),
      "legacy"
    )

    testthat::expect_identical(
      .resolve_msma_engine(gammaX = Inf),
      "legacy"
    )
  }
)
