# msma 4.0 development plan

## Scope

Version 4.0 adds opt-in soft-structured estimation while preserving every
Version 3.2 computation when no structure is supplied.

## Release gates

1. Version 3.2 versus 4.0 compatibility validation: Failed = 0.
2. A zero structural penalty reproduces the corresponding ordinary analysis.
3. Structure dimensions, names, symmetry, and finite values are validated.
4. Single-block, multiblock, and X/Y-side structured analyses are reproducible.
5. R CMD check --as-cran reports Status: OK.

## Milestones

- M0: freeze the CRAN 3.2 reference archive and validation report.
- M1: map S4PCA equations, symbols, and source functions.
- M2: implement and test one-component single-block estimation.
- M3: add sparsity, deflation, and multiple components.
- M4: add multiblock and PLS support.
- M5: connect the opt-in engine to msma.default().
- M6: add documentation, model selection, reverse-dependency tests, and release checks.

## Out of scope for 4.0.0

- Network estimation.
- Missing-data imputation.
- Automatic estimation of structural matrices.
- Combining the soft-structured engine with NMF or sNMF unless separately
  justified and validated.


