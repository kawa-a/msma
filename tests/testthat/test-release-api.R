test_that("Version 4.0 public S4PCA helpers are exported", {
  exports <- getNamespaceExports("msma")
  expect_true("s4pca_reconstruct" %in% exports)
  expect_true("s4pca_conformal_select" %in% exports)
})

test_that("default structured arguments preserve the ordinary engine", {
  X <- matrix(rnorm(120), 30, 4)
  a <- msma(X, comp = 2, scaling = FALSE, intseed = 3)
  b <- msma(X, comp = 2, scaling = FALSE, intseed = 3,
            structure.method = "none", gammaX = 0)
  expect_equal(a, b, tolerance = 1e-12)
})


