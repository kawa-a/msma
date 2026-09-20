## Test environments

* Local Windows 11 x64, R 4.5.2, `R CMD check --as-cran`
* Additional CRAN platforms will be recorded here before submission.

## R CMD check results

Local result for the Milestone 5 baseline:

    Status: OK

This release candidate must be checked again after the independent Version 3.2
compatibility and reverse-dependency audits. The final status and platforms
should replace this provisional text before CRAN submission.

## Changes in this version

Version 4.0 adds an explicitly requested supervised sparse soft-structured PCA
engine, reconstruction, and repeated split reconstruction-based model
selection. Existing calls that do not request structured PCA remain on the
Version 3.2 computational paths.

## Backward compatibility

The release process includes:

* a frozen regression test against the manuscript-generating S4PCA code;
* Version 3.2 versus Version 4.0 compatibility validation;
* package-level testthat tests; and
* a reverse-dependency check for `mand`.

## Reverse dependencies

CRAN currently reports one reverse dependency, `mand`. Its final check result
will be recorded here before submission.


