# Contributing to msma 4.0 RC

Thank you for testing the release candidate.

## Reporting problems

Use the GitHub issue templates and include a minimal reproducible example,
`sessionInfo()`, `packageVersion("msma")`, the release tag or commit, dimensions,
random seed, and the complete warning or error.

Do not upload confidential, identifiable, or restricted research data.
Use simulated or anonymized examples.

## Pull requests

1. Create a branch from `release/4.0` or the current RC branch.
2. Keep changes focused.
3. Add tests for behavioral changes.
4. Run the complete test suite and `R CMD check --as-cran`.
5. Confirm the frozen manuscript-code S4PCA regression still passes.
6. Describe backward-compatibility implications.

Multiblock and super-level soft-structured estimation are intentionally deferred
to a future version and should not be added to the Version 4.0 release branch.


