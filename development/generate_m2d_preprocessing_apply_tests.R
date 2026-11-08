# Generate formal M2-D preprocessing application tests.
# Run from the msma Version 5 repository root.

stopifnot(
  file.exists("src.r"),
  dir.exists("tests/testthat")
)

test_file <- file.path(
  "tests",
  "testthat",
  "test-v5-preprocessing-apply.R"
)

backup_dir <- file.path(
  "development",
  "v5-preprocessing"
)

dir.create(
  backup_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

backup_file <- file.path(
  backup_dir,
  "test-v5-preprocessing-apply-before-m2d.R"
)

if (file.exists(test_file)) {
  file.copy(
    test_file,
    backup_file,
    overwrite = TRUE
  )
}

test_code <- r'{
testthat::test_that(
  "legacy fitted centering is applied to new X data",
  {
    set.seed(801)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    colnames(X) <- paste0("x", seq_len(ncol(X)))

    fit <- msma(X = X, comp = 2, scaling = FALSE, intseed = 3)

    observed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = X,
      side = "X"
    )

    expected <- sweep(X, 2, colMeans(X), "-")

    testthat::expect_equal(
      observed,
      expected,
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "legacy fitted centering and scaling are applied to new X data",
  {
    set.seed(802)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    colnames(X) <- paste0("x", seq_len(ncol(X)))

    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

    observed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = X,
      side = "X"
    )

    expected <- scale(
      X,
      center = colMeans(X),
      scale = apply(X, 2, stats::sd)
    )

    testthat::expect_equal(
      observed,
      expected,
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "legacy PLS Y preprocessing is applied from fitted metadata",
  {
    set.seed(803)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    Y <- matrix(stats::rnorm(80), nrow = 20, ncol = 4)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    colnames(Y) <- paste0("y", seq_len(ncol(Y)))

    fit <- msma(
      X = X,
      Y = Y,
      comp = 2,
      scaling = TRUE,
      intseed = 3
    )

    observed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = Y,
      side = "Y"
    )

    expected <- scale(
      Y,
      center = colMeans(Y),
      scale = apply(Y, 2, stats::sd)
    )

    testthat::expect_equal(
      observed,
      expected,
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "structured scaling TRUE applies the saved X transformation",
  {
    set.seed(804)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
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

    observed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = X,
      side = "X"
    )

    expected <- sweep(X, 2, fit$Xcenter, "-")
    expected <- sweep(expected, 2, fit$Xscale, "/")

    testthat::expect_equal(
      observed,
      expected,
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "structured scaling FALSE leaves X unchanged",
  {
    set.seed(805)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
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

    observed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = X,
      side = "X"
    )

    testthat::expect_identical(observed, X)
  }
)


testthat::test_that(
  "Z is returned unchanged in the initial M2-D implementation",
  {
    set.seed(806)
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

    observed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = Z,
      side = "Z"
    )

    testthat::expect_identical(observed, Z)
  }
)


testthat::test_that(
  "multiblock X data retain list structure and block transformations",
  {
    set.seed(807)
    X <- list(
      block1 = matrix(stats::rnorm(80), nrow = 20, ncol = 4),
      block2 = matrix(stats::rnorm(60), nrow = 20, ncol = 3)
    )
    colnames(X[[1]]) <- paste0("a", seq_len(ncol(X[[1]])))
    colnames(X[[2]]) <- paste0("b", seq_len(ncol(X[[2]])))

    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

    observed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = X,
      side = "X"
    )

    expected <- Map(
      function(block, metadata) {
        result <- sweep(block, 2, metadata[["scaled:center"]], "-")
        sweep(result, 2, metadata[["scaled:scale"]], "/")
      },
      X,
      fit$Xscale
    )

    testthat::expect_type(observed, "list")
    testthat::expect_length(observed, 2L)
    testthat::expect_equal(
      observed,
      expected,
      tolerance = 1e-15,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "preprocessor safely replaces invalid scale values with one",
  {
    preprocess <- .new_msma_preprocess(
      engine = "legacy",
      scaling = TRUE
    )
    preprocess$X$center <- list(c(a = 1, b = 2, c = 3))
    preprocess$X$scale <- list(c(a = 0, b = Inf, c = NA_real_))
    preprocess$X$centered <- TRUE
    preprocess$X$scaled <- TRUE

    X <- matrix(c(2, 4, 6, 3, 5, 7), nrow = 2, byrow = TRUE)
    colnames(X) <- c("a", "b", "c")

    observed <- .apply_msma_preprocessor(
      preprocess = preprocess,
      newdata = X,
      side = "X"
    )

    expected <- sweep(X, 2, c(a = 1, b = 2, c = 3), "-")

    testthat::expect_equal(
      observed,
      expected,
      tolerance = 0,
      ignore_attr = TRUE
    )
  }
)


testthat::test_that(
  "validator rejects a non-preprocessing object",
  {
    testthat::expect_error(
      .apply_msma_preprocessor(
        preprocess = list(),
        newdata = matrix(1:6, nrow = 2),
        side = "X"
      ),
      "must inherit"
    )
  }
)


testthat::test_that(
  "validator rejects non-matrix X input",
  {
    prep <- .new_msma_preprocess(engine = "legacy", scaling = FALSE)
    prep$X$center <- list(c(a = 0, b = 0))
    prep$X$scale <- list(NULL)

    testthat::expect_error(
      .apply_msma_preprocessor(
        preprocess = prep,
        newdata = data.frame(a = 1:2, b = 3:4),
        side = "X"
      ),
      "numeric matrix"
    )
  }
)


testthat::test_that(
  "validator rejects the wrong number of blocks",
  {
    set.seed(808)
    X <- list(
      matrix(stats::rnorm(80), nrow = 20, ncol = 4),
      matrix(stats::rnorm(60), nrow = 20, ncol = 3)
    )
    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

    testthat::expect_error(
      .apply_msma_preprocessor(
        preprocess = fit$preprocess,
        newdata = X[[1]],
        side = "X"
      ),
      "block"
    )
  }
)


testthat::test_that(
  "validator rejects the wrong number of columns",
  {
    set.seed(809)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
    X_wrong <- matrix(stats::rnorm(80), nrow = 20, ncol = 4)

    testthat::expect_error(
      .apply_msma_preprocessor(
        preprocess = fit$preprocess,
        newdata = X_wrong,
        side = "X"
      ),
      "column"
    )
  }
)


testthat::test_that(
  "validator rejects reordered named columns",
  {
    set.seed(810)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
    X_reordered <- X[, rev(seq_len(ncol(X))), drop = FALSE]

    testthat::expect_error(
      .apply_msma_preprocessor(
        preprocess = fit$preprocess,
        newdata = X_reordered,
        side = "X"
      ),
      "Column names or column order"
    )
  }
)


testthat::test_that(
  "preprocessor does not modify its input",
  {
    set.seed(811)
    X <- matrix(stats::rnorm(100), nrow = 20, ncol = 5)
    X_original <- X
    fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)

    transformed <- .apply_msma_preprocessor(
      preprocess = fit$preprocess,
      newdata = X,
      side = "X"
    )

    testthat::expect_identical(X, X_original)
    testthat::expect_false(identical(transformed, X_original))
  }
)
}'

writeLines(
  strsplit(test_code, "\n", fixed = TRUE)[[1]],
  test_file,
  useBytes = TRUE
)

syntax_check <- try(parse(file = test_file), silent = TRUE)
if (inherits(syntax_check, "try-error")) {
  stop(as.character(syntax_check), call. = FALSE)
}

cat("Generated: ", normalizePath(test_file, winslash = "/"), "\n", sep = "")
if (file.exists(backup_file)) {
  cat("Backup: ", normalizePath(backup_file, winslash = "/"), "\n", sep = "")
}
cat("Syntax: OK\n")
