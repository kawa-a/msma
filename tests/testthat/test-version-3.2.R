test_that("default method remains PCA", {
  dat <- simdata(n = 20, Xps = c(3, 4), Yps = 3, seed = 1)
  fit_default <- msma(dat$X, comp = c(2, 1), intseed = 1)
  fit_explicit <- msma(dat$X, comp = c(2, 1), sprmethod = "PCA", intseed = 1)
  expect_equal(fit_default$wbX, fit_explicit$wbX)
  expect_equal(fit_default$sbX, fit_explicit$sbX)
})

test_that("matrix Z with one column agrees with vector Z", {
  dat <- simdata(n = 20, Xps = 4, Yps = 3, seed = 2)
  z <- rep(c(0, 1), 10)
  fit_vec <- msma(dat$X[[1]], Z = z, comp = 1, muX = 0.2, intseed = 1)
  fit_mat <- msma(dat$X[[1]], Z = cbind(z), comp = 1, muX = 0.2, intseed = 1)
  expect_equal(fit_vec$wbX, fit_mat$wbX)
})

test_that("sNMF is reproducible and nonnegative", {
  dat <- simdata(n = 20, Xps = c(3, 4), Yps = 3, seed = 3)
  fit1 <- msma(dat$X, comp = c(2, 2), sprmethod = "sNMF",
               lambdaXsup = 0.05, intseed = 11)
  fit2 <- msma(dat$X, comp = c(2, 2), sprmethod = "sNMF",
               lambdaXsup = 0.05, intseed = 11)
  expect_equal(fit1$ssX, fit2$ssX)
  expect_true(all(unlist(fit1$ssX) >= 0))
  expect_true(all(unlist(fit1$wsX) >= 0))
})

test_that("one-column matrix Z is identical to vector Z", {
  dat <- simdata(n = 20, Xps = c(4, 3), Yps = 3, seed = 101)
  z <- rep(c(0, 1), 10)

  set.seed(77)
  fit_vector <- msma(dat$X, Z = z, comp = 1, muX = 0.2, intseed = 11)
  set.seed(77)
  fit_matrix <- msma(dat$X, Z = cbind(z), comp = 1, muX = 0.2, intseed = 11)

  fit_vector$call <- NULL
  fit_matrix$call <- NULL
  expect_equal(fit_matrix, fit_vector, tolerance = 1e-8)
})

test_that("only the public default S3 method has an S3-style name", {
  expect_true(exists("msma.default", mode = "function"))
  expect_false(exists("msma.default_legacy", mode = "function"))
  expect_false(exists("msma.default_extended", mode = "function"))
})


