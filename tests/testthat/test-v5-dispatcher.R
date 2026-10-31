testthat::test_that(
  "Version 5 dispatcher preserves Version 4 engine policy",
  {
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
