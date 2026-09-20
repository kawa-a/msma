source(testthat::test_path("test-s4pca-reference-regression.R"), local = TRUE)

test_that("training and new-data reconstruction have valid dimensions", {
  d <- make_s4pca_fixture(n = 36, p = 20)
  fit <- msma(d$X, Z = d$Z, comp = 3, lambdaX = .1, muX = .8,
              structure.method = "soft", gammaX = .1,
              scaling = FALSE, intseed = 3)
  expect_identical(dim(s4pca_reconstruct(fit)), dim(d$X))
  expect_identical(dim(s4pca_reconstruct(fit, d$X[1:7, , drop=FALSE])), c(7L,20L))
})

test_that("reconstruction respects original scale", {
  d <- make_s4pca_fixture(n = 36, p = 20)
  X <- sweep(sweep(d$X, 2, seq_len(20), "*"), 2, seq_len(20), "+")
  fit <- msma(X, Z=d$Z, comp=2, lambdaX=.1, muX=.8,
              structure.method="soft", gammaX=.1, scaling=TRUE, intseed=4)
  a <- s4pca_reconstruct(fit, scaled=FALSE)
  b <- s4pca_reconstruct(fit, scaled=TRUE)
  expect_identical(dim(a), dim(X)); expect_identical(dim(b), dim(X))
  expect_false(isTRUE(all.equal(a,b)))
})

test_that("repeated split selection is reproducible", {
  d <- make_s4pca_fixture(n=32,p=16)
  args <- list(X=d$X,Z=d$Z,lambdaX=c(.05,.1),gammaX=c(0,.1),comp=2:3,
               repeats=2,calibration_fraction=.25,muX=.8,niterS4=10,
               scaling=FALSE,intseed=9)
  a <- do.call(s4pca_conformal_select,args)
  b <- do.call(s4pca_conformal_select,args)
  expect_identical(a$selected,b$selected)
  expect_equal(a$summary,b$summary,tolerance=1e-12)
  expect_s3_class(a$fit,"s4pca")
})

test_that("selected candidate belongs to the requested grid", {
  d <- make_s4pca_fixture(n=30,p=16)
  z <- s4pca_conformal_select(d$X,d$Z,lambdaX=c(.05,.2),gammaX=c(0,.1),
       comp=c(2,3),repeats=1,niterS4=8,scaling=FALSE,intseed=5)
  expect_true(z$selected$lambdaX %in% c(.05,.2))
  expect_true(z$selected$gammaX %in% c(0,.1))
  expect_true(z$selected$comp %in% c(2,3))
  expect_equal(nrow(z$summary),8)
})

test_that("model-selection inputs are validated", {
  X <- matrix(rnorm(80),20,4)
  expect_error(s4pca_conformal_select(X,alpha=0),"alpha")
  expect_error(s4pca_conformal_select(X,repeats=0),"repeats")
  expect_error(s4pca_conformal_select(X,calibration_fraction=1),"calibration")
  expect_error(s4pca_conformal_select(X,max_overlap=2),"max_overlap")
  expect_error(s4pca_conformal_select(X,lambdaX=-1),"lambdaX")
})

test_that("split details retain the repeat identifier", {
  d <- make_s4pca_fixture(n = 24, p = 12)
  z <- s4pca_conformal_select(
    d$X, d$Z,
    lambdaX = 0.1,
    gammaX = 0.1,
    comp = 2,
    repeats = 2,
    niterS4 = 5,
    scaling = FALSE,
    intseed = 2,
    refit = FALSE
  )
  expect_true("repeat" %in% names(z$details))
  expect_identical(sort(unique(z$details[["repeat"]])), c(1L, 2L))
})


