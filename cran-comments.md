# CRAN submission comments

## Resubmission

This is a corrective update from msma 5.0 to version 5.1.

Version 5.0 produced test errors on the CRAN macOS arm64 release and
old-release platforms. The failures were caused by prediction regression
tests requiring bitwise identity with numeric reference results generated
on another BLAS/LAPACK platform.

For the ordinary prediction cases, the observed differences were
approximately 1e-16 to 1e-15. These tests now use an explicit tolerance
of 1e-12.

The single-row legacy prediction test now verifies projection scores,
output dimensions, and finite outputs separately rather than requiring
platform-specific reconstructed-X reference values.

The prediction implementation, defaults, fitted-object structure, and
public API are unchanged.


The short interval since version 5.0 is intentional.
Version 5.1 is being submitted promptly because CRAN checks for version
5.0 identified test errors on the macOS arm64 release and old-release
platforms. This corrective release addresses those failures without
changing the package implementation or public API.
## Submission

This is an update of msma from CRAN version 4.0 to version 5.0.

Package source:

* https://github.com/kawa-a/msma

Issue tracker:

* https://github.com/kawa-a/msma/issues

## Test environments

* Windows 11 x64, R 4.5.2
* Local R CMD check --as-cran
* Local R CMD check --no-manual

GitHub Actions completed successfully on:

* Windows, R-release
* macOS, R-release
* Ubuntu, R-release
* Ubuntu, R-devel

GitHub Actions run for Version 5.1: 42435066837
The successful macOS job used platform aarch64-apple-darwin23, directly
covering the architecture on which the Version 5.0 test failures occurred.

## R CMD check results

Local R CMD check --as-cran on the Version 5.1 release tarball:

* 0 errors
* 0 warnings
* 1 note

The NOTE was:

```
checking CRAN incoming feasibility ... NOTE
Days since last update: 1
```

The short interval is intentional. Version 5.1 is a corrective update
for test errors identified by CRAN on the macOS arm64 release and
old-release platforms after publication of version 5.0.

The package implementation and public API are unchanged.

## Test suite

* 1026 expectations passed
* 0 failures
* 0 errors
* 0 skipped tests
* 0 warnings

The test suite covers legacy computation, structured PCA, dispatcher
selection, preprocessing, serialization, prediction, reconstruction,
and the Version 5 supervision contract.

## Version 5.0 summary

Version 5.0 adds fitted preprocessing and fixed reconstruction metadata
for prediction, serialization-safe fitted objects, and a canonical
contract for inactive supervision.

When all corresponding supervision weights are zero, Z and con4spv are
removed from the estimation path before dispatcher resolution.
Consequently, contrast coefficients cannot affect estimation when the
supervision term is inactive.

Active multicolumn contrasts are validated for dimension, finite
coefficients, and a positive L1 norm before normalization. One-column
supervision retains backward compatibility through canonical contrast 1
when con4spv is omitted.

Requested and used supervision specifications are retained in fitted
objects through the supervision component.

This release does not introduce block-specific supervision weights or
soft-structured multiblock estimation.

## Backward compatibility

The Version 3.2 legacy paths and the Version 4.0 structured PCA paths are
covered by regression tests.

Existing calls using a nonzero con4spv with zero corresponding
supervision weights remain successful. The contrast is ignored with a
warning rather than producing an error.

For active one-column supervision, omission of con4spv continues to be
supported through canonical contrast 1.

## Reverse dependencies

mand 3.0 is listed as a reverse Suggests package.

Compatibility was checked against the msma Version 5.0 release candidate
in an isolated library. mand 3.0 built successfully, completed R CMD check
with Status: OK, rebuilt its Vignettes, and loaded successfully with
msma 5.0.
