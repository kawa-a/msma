source(testthat::test_path("test-s4pca-reference-regression.R"), local = TRUE)

test_that("validated candidate preserves reference numerical outputs", {
  dat <- make_s4pca_fixture()
  ref <- run_reference(dat$X, dat$Z)
  candidate <- getFromNamespace(".s4pca_fit", "msma")
  fit <- candidate(dat$X, dat$Z, comp = 6, lambda = 0.20, gamma = 0.10,
                   mu = 1, niter = 40, seed = 3)
  expect_equal(fit$W, ref$W, tolerance = 1e-12)
  expect_equal(fit$S, ref$S, tolerance = 1e-12)
  expect_equal(fit$rss, ref$rss, tolerance = 1e-12)
  expect_equal(fit$AIC, ref$AIC, tolerance = 1e-12)
  expect_equal(fit$BIC, ref$BIC, tolerance = 1e-12)
  expect_equal(fit$EBIC, ref$EBIC, tolerance = 1e-12)
  expect_identical(fit$used_idx, ref$used_idx)
  expect_equal(fit$overlap_all, ref$overlap, tolerance = 1e-12)
})

test_that("one-column matrix Z is normalized to a vector", {
  dat <- make_s4pca_fixture(n = 30, p = 20)
  candidate <- getFromNamespace(".s4pca_fit", "msma")
  a <- candidate(dat$X, dat$Z, comp = 2, seed = 7)
  b <- candidate(dat$X, cbind(dat$Z), comp = 2, seed = 7)
  expect_equal(a$W, b$W, tolerance = 1e-12)
  expect_equal(a$S, b$S, tolerance = 1e-12)
})

test_that("invalid X and Z inputs are rejected", {
  candidate <- getFromNamespace(".s4pca_fit", "msma")
  expect_error(candidate(matrix(c(1, NA, 2, 3), 2), seed = 1), "X must not contain")
  expect_error(candidate(matrix(1:6, 3, 2), Z = 1:2, seed = 1), "length\\(Z\\)")
  expect_error(candidate(matrix(1:6, 3, 2), Z = cbind(1:3, 4:6), seed = 1), "Z must be")
})

test_that("invalid tuning and control values are rejected", {
  candidate <- getFromNamespace(".s4pca_fit", "msma")
  X <- matrix(rnorm(30), 10, 3)
  expect_error(candidate(X, comp = 0), "comp")
  expect_error(candidate(X, comp = 1.5), "comp")
  expect_error(candidate(X, lambda = -0.1), "lambda")
  expect_error(candidate(X, gamma = Inf), "gamma")
  expect_error(candidate(X, mu = 1.1), "mu")
  expect_error(candidate(X, niter = 0), "niter")
  expect_error(candidate(X, hard_exclusion = NA), "hard_exclusion")
})

test_that("diagnostics are internally consistent", {
  dat <- make_s4pca_fixture(n = 30, p = 20)
  candidate <- getFromNamespace(".s4pca_fit", "msma")
  fit <- candidate(dat$X, dat$Z, comp = 3, lambda = 0.15,
                   gamma = 0.10, mu = 0.8, niter = 20, seed = 9)
  d <- fit$diagnostics
  expect_identical(d$engine, "reference-compatible")
  expect_true(d$reference_compatible)
  expect_identical(d$extracted_components, ncol(fit$W))
  expect_identical(d$requested_components, 3L)
  expect_equal(d$selected_per_component, colSums(abs(fit$W) > 1e-8))
  expect_equal(d$selected_variables, sum(rowSums(abs(fit$W) > 1e-8) >= 1L))
  expect_equal(d$repeated_variables, sum(rowSums(abs(fit$W) > 1e-8) >= 2L))
})

test_that("simulation fixture supports reduced variable counts", {
  dat <- make_s4pca_fixture(n = 30, p = 20)
  expect_identical(dim(dat$X), c(30L, 20L))
  expect_identical(length(dat$Z), 30L)
  expect_identical(dim(dat$loading), c(20L, 4L))
})


