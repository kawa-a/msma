source(testthat::test_path("test-s4pca-reference-regression.R"), local = TRUE)

test_that("structured PCA is opt-in and defaults remain legacy", {
  X <- matrix(rnorm(120), 30, 4)
  ordinary <- msma(X, comp=2, scaling=FALSE, intseed=3)
  expect_s3_class(ordinary, "msma")
  fit <- msma(X, comp=2, lambdaX=.1, structure.method="soft", gammaX=.1,
              niterS4=20, scaling=FALSE, intseed=3)
  expect_s3_class(fit, "s4pca")
})

test_that("public soft API matches validated engine", {
  d <- make_s4pca_fixture(n=40,p=20)
  a <- msma(d$X,Z=d$Z,comp=3,lambdaX=.2,muX=.8,structure.method="soft",
            gammaX=.1,niterS4=30,scaling=FALSE,intseed=7)
  b <- getFromNamespace(".s4pca_fit","msma")(d$X,d$Z,comp=3,lambda=.2,
       gamma=.1,mu=.8,niter=30,seed=7)
  expect_equal(a$W,b$W,tolerance=1e-12); expect_equal(a$S,b$S,tolerance=1e-12)
})

test_that("gammaX explicitly opts into soft structure", {
  d <- make_s4pca_fixture(n=30,p=20)
  fit <- msma(d$X,Z=d$Z,comp=2,lambdaX=.1,muX=.8,gammaX=.1,
              scaling=FALSE,intseed=5)
  expect_s3_class(fit,"s4pca"); expect_identical(fit$structure.method,"soft")
})

test_that("exclusive API matches hard exclusion", {
  d <- make_s4pca_fixture(n=40,p=20)
  a <- msma(d$X,Z=d$Z,comp=3,lambdaX=.05,muX=1,structure.method="exclusive",
            niterS4=30,scaling=FALSE,intseed=11)
  b <- getFromNamespace(".s4pca_fit","msma")(d$X,d$Z,comp=3,lambda=.05,
       gamma=0,mu=1,hard_exclusion=TRUE,niter=30,seed=11)
  expect_equal(a$W,b$W,tolerance=1e-12); expect_equal(a$S,b$S,tolerance=1e-12)
})

test_that("unsupported structured combinations fail clearly", {
  X <- matrix(rnorm(120),30,4); Y <- matrix(rnorm(90),30,3)
  expect_error(msma(list(X,X),structure.method="soft"),"single numeric matrix")
  expect_error(msma(X,Y=Y,structure.method="soft"),"with Y")
  expect_error(msma(X,comp=c(2,2),structure.method="soft"),"single component")
  expect_error(msma(X,structure.method="soft",sprmethod="sNMF"),"cannot be combined")
  expect_error(msma(X,structure.method="soft",Z=cbind(1:30,31:60)),"only vector")
})

test_that("public structured API is reproducible", {
  d <- make_s4pca_fixture(n=30,p=20)
  a <- msma(d$X,Z=d$Z,comp=2,lambdaX=.1,muX=.8,structure.method="soft",
            gammaX=.1,scaling=FALSE,intseed=13)
  b <- msma(d$X,Z=d$Z,comp=2,lambdaX=.1,muX=.8,structure.method="soft",
            gammaX=.1,scaling=FALSE,intseed=13)
  expect_identical(a$W,b$W); expect_identical(a$S,b$S)
})


