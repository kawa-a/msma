# CRAN submission comments

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

GitHub Actions run: 42324927210

## R CMD check results

Local R CMD check --as-cran on the Version 5.0 release tarball:

* 0 errors
* 0 warnings
* 0 notes

A supplementary local R CMD check --no-manual completed with:

```
Status: OK
```

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

mand is listed as a reverse Suggests package.
Compatibility will be checked against the Version 5.0 release candidate
before submission.
