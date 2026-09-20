# Milestone 3 report: validation and diagnostics

## Implemented

- Added `Encoding: UTF-8` to DESCRIPTION.
- Removed the stale Date field.
- Qualified `utils::tail()` in the candidate engine.
- Preserved `.s4pca_fit_reference()` as the numerical oracle-compatible engine.
- Added `.s4pca_validate_inputs()`.
- Added `.s4pca_fit()` as a validated internal interface.
- Added diagnostics without changing W, S, RSS, AIC, BIC, EBIC, selected indices,
  or the original overlap statistic.

## Not yet implemented

- Public `msma()` dispatch.
- Alternative convergence rules.
- Alternative deflation.
- Conformal hyperparameter selection.
- Formal Rd documentation for a public function.

## Required local gates

1. Run the original reference-regression test with no failures or skips.
2. Run `test-s4pca-validation-diagnostics.R` with no failures.
3. Run the full package tests.
4. Run `R CMD check --as-cran`.

## Corrective revision

- The test fixture now truncates manuscript support sets to `1:p`, allowing
  reduced-dimensional test cases such as `p = 20` without out-of-bounds indices.
- `create_4.0.R` now restores the canonical DESCRIPTION before `roxygenize()`,
  so roxygen2 sees `Encoding: UTF-8` during documentation generation.


