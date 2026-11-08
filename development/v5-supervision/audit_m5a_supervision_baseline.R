# M5-A supervision baseline audit.

source("src.r", local = globalenv())

set.seed(5101)
X <- matrix(rnorm(240), nrow = 40, ncol = 6)
colnames(X) <- paste0("x", seq_len(ncol(X)))
Z <- cbind(
  z1 = rnorm(nrow(X)),
  z2 = rnorm(nrow(X))
)

fit_without_contrast <- msma(
  X = X,
  Z = Z,
  comp = 2,
  lambdaX = 0.2,
  muX = 0,
  intseed = 7
)

fit_with_contrast <- suppressWarnings(
  msma(
    X = X,
    Z = Z,
    comp = 2,
    lambdaX = 0.2,
    muX = 0,
    con4spv = c(1, -0.5),
    intseed = 7
  )
)

baseline <- list(
  no_contrast_engine = fit_without_contrast$preprocess$engine,
  contrast_engine = fit_with_contrast$preprocess$engine,
  wbX_identical = identical(
    fit_without_contrast$wbX,
    fit_with_contrast$wbX
  ),
  sbX_identical = identical(
    fit_without_contrast$sbX,
    fit_with_contrast$sbX
  ),
  contrast_zero_result = try(
    msma(
      X = X,
      Z = Z,
      comp = 1,
      lambdaX = 0.2,
      muX = 0,
      con4spv = c(0, 0),
      intseed = 7
    ),
    silent = TRUE
  )
)

saveRDS(
  baseline,
  file.path(
    "development",
    "v5-supervision",
    "m5a-supervision-baseline-result.rds"
  ),
  version = 3
)

print(baseline[c(
  "no_contrast_engine",
  "contrast_engine",
  "wbX_identical",
  "sbX_identical"
)])
