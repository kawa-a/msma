# Regression tests for msma 4.0 S4PCA
#
# Primary rule: the refactored implementation must reproduce the original
# manuscript-generating implementation before it is connected to msma().

reference_file <- testthat::test_path("reference", "S3PCA_reference.R")
if (!file.exists(reference_file)) {
  stop("Missing reference implementation: ", reference_file)
}
source(reference_file, local = TRUE)

make_s4pca_fixture <- function(seed = 20260806L, n = 75L, p = 40L) {
  set.seed(seed)
  score <- matrix(stats::rnorm(n * 4L), nrow = n, ncol = 4L)
  loading <- matrix(0, nrow = p, ncol = 4L)
  support_template <- list(1:10, 8:17, 18:27, 25:34)
  support <- lapply(support_template, function(idx) idx[idx <= p])
  for (k in seq_along(support)) {
    idx <- support[[k]]
    if (!length(idx)) next
    loading[idx, k] <- stats::runif(length(idx), 0.5, 1.0) *
      sample(c(-1, 1), length(idx), replace = TRUE)
  }
  X <- score %*% t(loading) + matrix(stats::rnorm(n * p, sd = 0.5), n, p)
  X <- scale(X)
  Z <- as.numeric(scale(score[, 1] + 0.8 * score[, 2] + stats::rnorm(n)))
  colnames(X) <- paste0("x", seq_len(p))
  list(X = X, Z = Z, score = score, loading = loading)
}

run_reference <- function(X, Z = NULL, comp = 6L, lambda = 0.20,
                          gamma = 0.10, mu = 1.0, str = FALSE,
                          niter = 40L, seed = 3L) {
  set.seed(seed)
  S3PCA_reference(
    Xs = X, Zs = Z, comp = comp, lambda = lambda, gamma = gamma,
    excl_mode = "sumabs", mu = mu, str = str, niter = niter,
    verbose = FALSE
  )
}

run_candidate <- function(X, Z = NULL, comp = 6L, lambda = 0.20,
                          gamma = 0.10, mu = 1.0, str = FALSE,
                          niter = 40L, seed = 3L) {
  candidate <- getFromNamespace(".s4pca_fit_reference", "msma")
  set.seed(seed)
  candidate(
    X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
    history_method = "sumabs", mu = mu, hard_exclusion = str,
    niter = niter, seed = seed, verbose = FALSE
  )
}

expect_reference_match <- function(reference, candidate, tolerance = 1e-12) {
  testthat::expect_equal(candidate$W, reference$W, tolerance = tolerance)
  testthat::expect_equal(candidate$S, reference$S, tolerance = tolerance)
  testthat::expect_equal(candidate$rss, reference$rss, tolerance = tolerance)
  testthat::expect_equal(candidate$AIC, reference$AIC, tolerance = tolerance)
  testthat::expect_equal(candidate$BIC, reference$BIC, tolerance = tolerance)
  testthat::expect_equal(candidate$EBIC, reference$EBIC, tolerance = tolerance)
  testthat::expect_identical(candidate$used_idx, reference$used_idx)
  testthat::expect_identical(ncol(candidate$W), ncol(reference$W))
  testthat::expect_identical(
    abs(candidate$W) <= 1e-8,
    abs(reference$W) <= 1e-8
  )
  testthat::expect_equal(
    candidate$overlap_all,
    reference$overlap,
    tolerance = tolerance
  )
}

testthat::test_that("candidate S4PCA reproduces the manuscript algorithm", {
  testthat::skip_if_not(
    exists(".s4pca_fit_reference", envir = asNamespace("msma"), inherits = FALSE),
    "The Version 4.0 candidate engine has not been implemented yet"
  )
  dat <- make_s4pca_fixture()
  ref <- run_reference(dat$X, dat$Z)
  new <- run_candidate(dat$X, dat$Z)
  expect_reference_match(ref, new)
})

scenarios <- list(
  S4PCA = list(mu = 1.0, lambda = 0.20, gamma = 0.10, str = FALSE),
  S2PCA = list(mu = 0.8, lambda = 0.20, gamma = 0.00, str = FALSE),
  SStSPCA = list(mu = 0.0, lambda = 0.20, gamma = 0.10, str = FALSE),
  sparse_PCA = list(mu = 0.0, lambda = 0.20, gamma = 0.00, str = FALSE),
  hard_exclusive_S3PCA = list(mu = 1.0, lambda = 0.05, gamma = 0.00, str = TRUE),
  no_sparsity = list(mu = 0.0, lambda = 0.00, gamma = 0.00, str = FALSE)
)

for (scenario_name in names(scenarios)) {
  local({
    nm <- scenario_name
    pars <- scenarios[[scenario_name]]
    testthat::test_that(paste("reference agreement:", nm), {
      testthat::skip_if_not(
        exists(".s4pca_fit_reference", envir = asNamespace("msma"), inherits = FALSE),
        "The Version 4.0 candidate engine has not been implemented yet"
      )
      dat <- make_s4pca_fixture()
      ref <- do.call(run_reference, c(list(X = dat$X, Z = dat$Z), pars))
      new <- do.call(run_candidate, c(list(X = dat$X, Z = dat$Z), pars))
      expect_reference_match(ref, new)
    })
  })
}

testthat::test_that("reference agreement holds without supervision", {
  testthat::skip_if_not(
    exists(".s4pca_fit_reference", envir = asNamespace("msma"), inherits = FALSE),
    "The Version 4.0 candidate engine has not been implemented yet"
  )
  dat <- make_s4pca_fixture()
  ref <- run_reference(dat$X, Z = NULL, mu = 0, lambda = 0.15, gamma = 0.10)
  new <- run_candidate(dat$X, Z = NULL, mu = 0, lambda = 0.15, gamma = 0.10)
  expect_reference_match(ref, new)
})

testthat::test_that("reference agreement holds across component counts", {
  testthat::skip_if_not(
    exists(".s4pca_fit_reference", envir = asNamespace("msma"), inherits = FALSE),
    "The Version 4.0 candidate engine has not been implemented yet"
  )
  dat <- make_s4pca_fixture()
  for (k in c(1L, 2L, 4L, 6L)) {
    ref <- run_reference(dat$X, dat$Z, comp = k)
    new <- run_candidate(dat$X, dat$Z, comp = k)
    expect_reference_match(ref, new)
  }
})

testthat::test_that("candidate is reproducible for a fixed seed", {
  testthat::skip_if_not(
    exists(".s4pca_fit_reference", envir = asNamespace("msma"), inherits = FALSE),
    "The Version 4.0 candidate engine has not been implemented yet"
  )
  dat <- make_s4pca_fixture()
  fit1 <- run_candidate(dat$X, dat$Z, seed = 19L)
  fit2 <- run_candidate(dat$X, dat$Z, seed = 19L)
  testthat::expect_identical(fit1, fit2)
})

testthat::test_that("both overlap definitions are internally consistent", {
  testthat::skip_if_not(
    exists(".s4pca_fit_reference", envir = asNamespace("msma"), inherits = FALSE),
    "The Version 4.0 candidate engine has not been implemented yet"
  )
  dat <- make_s4pca_fixture()
  fit <- run_candidate(dat$X, dat$Z)
  count <- rowSums(abs(fit$W) > 1e-8)
  expected_all <- mean(count >= 2L)
  expected_selected <- if (any(count >= 1L)) {
    sum(count >= 2L) / sum(count >= 1L)
  } else {
    0
  }
  testthat::expect_equal(fit$overlap_all, expected_all, tolerance = 1e-12)
  testthat::expect_equal(
    fit$overlap_selected,
    expected_selected,
    tolerance = 1e-12
  )
})


