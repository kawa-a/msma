# Milestone 6: independent audit and release-candidate preparation

The package source is now arranged as a release candidate, but release remains
gated on two independent checks that cannot be inferred from the ordinary
package check: Version 3.2 versus Version 4.0 compatibility and the `mand`
reverse-dependency check.

The release scripts intentionally do not silently mark these audits as passed.
Their outputs must be reviewed and copied into `cran-comments.md` before CRAN
submission.

## Path-handling correction

The compatibility runner now resolves the development root first and passes
absolute paths to `rmarkdown::render()`. This avoids losing the Rmd path after
changing the working directory to `validation/`.

## Reverse-dependency correction

The first `mand` run completed its functional checks, but `--library` was
passed in a nonportable form, so msma was installed into the ordinary library.
The corrected script uses `--library=<path>`, verifies msma 4.0 inside the
isolated library, and omits CRAN incoming feasibility because checking the
already-published `mand` 3.0 source with `--as-cran` creates an expected
same-version warning unrelated to msma compatibility.

## Reverse-dependency log location

The isolated `mand` check itself completed with `Status: OK`. On this Windows
R setup, `mand.Rcheck` was created in the development root rather than under
`validation/mand-revdep`. The audit and finalization scripts now search both
explicit locations and select the most recently modified valid log.


