# msma 4.0.0 Release Candidate

[![R-CMD-check](https://github.com/kawa-a/msma/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/kawa-a/msma/actions/workflows/R-CMD-check.yaml)
[![Release](https://img.shields.io/badge/release-v4.0.0--rc1-orange.svg)](https://github.com/kawa-a/msma/releases/tag/v4.0.0-rc1)
[![CRAN version](https://www.r-pkg.org/badges/version/msma)](https://CRAN.R-project.org/package=msma)
[![R version](https://img.shields.io/badge/R-%3E%3D%203.5-276DC3.svg?logo=r&logoColor=white)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/license-GPL--2%2B-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![Lifecycle](https://img.shields.io/badge/lifecycle-release%20candidate-orange.svg)](#release-candidate-notice)
[![CRAN status](https://img.shields.io/badge/CRAN%20status-pre--submission-yellow.svg)](#release-candidate-notice)

`msma` is an R package for sparse multivariable analysis, including PCA, PLS,
supervised analysis, and multiblock methods.

This repository provides **msma 4.0.0 Release Candidate 1 (`v4.0.0-rc1`)**
for pre-release testing. Version 4.0 adds supervised sparse soft-structured PCA
while preserving the default computational paths of msma 3.2.

## Release candidate notice

This version is intended for evaluation by collaborators and advanced users.
It has not yet been submitted to CRAN. For production analyses that do not
require the new structured PCA functionality, the current CRAN release should
remain the default choice.

## Main additions in Version 4.0

- supervised sparse soft-structured PCA for a single numeric `X` matrix
- soft control of variable reuse across components
- hard-exclusive variable selection across components
- reconstruction of training and new data
- repeated split reconstruction-based parameter selection
- separate overlap measures for all variables and selected variables
- input validation and diagnostic information
- regression testing against the manuscript-generating implementation
- backward-compatibility testing against msma 3.2

The ordinary `msma()` interface remains unchanged unless structured PCA is
explicitly requested.

## Installation

### Tagged GitHub release candidate

```r
install.packages("remotes")
remotes::install_github("kawa-a/msma", ref = "v4.0.0-rc1")
```

### Separate test library

```r
test_library <- path.expand("~/R/msma-4.0-rc1")
dir.create(test_library, recursive = TRUE, showWarnings = FALSE)

remotes::install_github(
  "kawa-a/msma",
  ref = "v4.0.0-rc1",
  lib = test_library
)

library(msma, lib.loc = test_library)
packageVersion("msma")
find.package("msma", lib.loc = test_library)
```

## Backward compatibility

Existing analyses stay on the Version 3.2 computational path when structured
PCA is not requested.

```r
fit <- msma(
  X = X,
  Z = Z,
  comp = 2,
  lambdaX = 0.10,
  muX = 0.50
)
```

This is equivalent to explicitly specifying:

```r
fit <- msma(
  X = X,
  Z = Z,
  comp = 2,
  lambdaX = 0.10,
  muX = 0.50,
  structure.method = "none",
  gammaX = 0
)
```

## Soft-structured PCA

```r
set.seed(4)
n <- 80
p <- 30
X <- scale(matrix(rnorm(n * p), nrow = n, ncol = p))
Z <- as.numeric(scale(0.8 * X[, 1] - 0.5 * X[, 5] + rnorm(n)))
colnames(X) <- paste0("x", seq_len(p))

fit <- msma(
  X = X,
  Z = Z,
  comp = 3,
  lambdaX = 0.10,
  muX = 0.80,
  structure.method = "soft",
  gammaX = 0.10,
  history.method = "sumabs",
  niterS4 = 30,
  scaling = FALSE,
  intseed = 4
)

fit$W
fit$S
fit$overlap_all
fit$overlap_selected
fit$diagnostics
```

Conceptually, for component $k$ the variable-specific threshold is

$$
\lambda_{jk} = \lambda_X + \gamma_X h_{j,k-1},
$$

with the reference-compatible default

$$
h_{j,k-1} = \sum_{\ell < k} |w_{j\ell}|.
$$

- `lambdaX` controls ordinary sparsity.
- `gammaX` controls the additional penalty for repeated variable use.
- `muX` controls the contribution of supervision.
- `niterS4` specifies the fixed number of updates per component.

## Hard-exclusive PCA

```r
fit_exclusive <- msma(
  X = X,
  Z = Z,
  comp = 3,
  lambdaX = 0.05,
  muX = 0.80,
  structure.method = "exclusive",
  niterS4 = 30,
  scaling = FALSE,
  intseed = 4
)
```

## Reconstruction

```r
X_reconstructed <- s4pca_reconstruct(fit)
X_new_reconstructed <- s4pca_reconstruct(
  fit,
  newdata = X[1:10, , drop = FALSE]
)
```

When a model is fitted with `scaling = TRUE`, the default reconstruction is
returned on the original scale. Use `scaled = TRUE` for the standardized scale.

## Repeated split model selection

```r
selection <- s4pca_conformal_select(
  X = X,
  Z = Z,
  lambdaX = c(0.05, 0.10),
  gammaX = c(0, 0.10),
  comp = 2:3,
  alpha = 0.10,
  repeats = 3,
  calibration_fraction = 0.25,
  max_overlap = 0.50,
  muX = 0.80,
  history.method = "sumabs",
  niterS4 = 20,
  scaling = FALSE,
  intseed = 4
)

selection$selected
selection$summary
selection$details
selection$fit
```

The current criterion is reconstruction-based and conformal-style. It is not a
complete predictive interval procedure for the supervision outcome.

## Current scope

Version 4.0 structured estimation supports:

- a single numeric `X` matrix
- PCA mode with `Y = NULL`
- `Z = NULL`, a numeric vector, or a one-column matrix
- scalar `lambdaX` for an individual fit
- lasso thresholding
- fixed iterations and projection-based deflation

## Not yet supported

The following are intentionally deferred to a future version:

- multiblock soft-structured PCA
- within-block soft-structured penalties
- super-level soft-structured penalties
- structured multiblock PLS
- structured analysis with `Y`
- multiple-column `Z` in the structured-PCA path
- soft-structured NMF, sNMF, or CMP
- super-level structured parameter selection

Existing multiblock functionality remains available through the ordinary
`msma()` interface, but it cannot currently be combined with
`structure.method = "soft"` or `"exclusive"`.

## Validation completed before RC1

- `R CMD check --as-cran`: 0 errors, 0 warnings, 0 notes
- package test suite: 0 failures, 0 skips
- regression against the manuscript-generating S4PCA implementation at
  tolerance $10^{-12}$
- independent Version 3.2 versus Version 4.0 compatibility audit: `Failed: 0`
- reverse-dependency check for `mand`: `Status: OK`
- vignette reconstruction and PDF/HTML manual generation

## What collaborators should test

1. Rerun an existing msma 3.2 analysis without adding structured arguments.
2. Test `structure.method = "soft"` on a real dataset.
3. Compare soft structure with `structure.method = "exclusive"`.
4. Check training and new-data reconstruction.
5. Run a small model-selection grid and record execution time and stability.
6. Comment on argument names, error messages, output structure, and vignette clarity.

## Reproducible issue reports

Please include:

```r
sessionInfo()
packageVersion("msma")
find.package("msma")
```

Also include the complete call, full warning or error, random seed, data
dimensions, operating system, release tag or commit, and a minimal example when
possible.

https://github.com/kawa-a/msma/issues/new?template=bug_report.yml

## Known limitations of RC1

- Structured estimation is limited to a single `X` matrix.
- Multiblock and super-level structured penalties are deferred.
- S4PCA uses fixed iterations.
- Large model-selection grids may be computationally intensive.
- Parallel grid evaluation is not implemented.
- API details may change before the CRAN release following collaborator review.

## Development status

```text
v4.0.0-rc1
    -> collaborator testing
    -> v4.0.0-rc2, if corrections are needed
    -> final compatibility and reverse-dependency audit
    -> CRAN submission of msma 4.0
```

## Citation

Until Version 4.0 is formally released, cite the current CRAN version and
record the GitHub release tag or commit used in the analysis.

```r
citation("msma")
```

## License

GPL version 2 or later.

## Maintainer

Atsushi Kawaguchi




