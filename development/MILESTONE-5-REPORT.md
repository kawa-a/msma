# Milestone 5: compatibility and model selection

## Added

- Public `s4pca_reconstruct()` for training and new-data reconstruction.
- Public `s4pca_conformal_select()` for repeated split reconstruction-based
  model selection over lambdaX, gammaX, and comp.
- Median aggregation across repeated splits and an overlap constraint based on
  `overlap_selected`.
- Full refit on all observations after selection.
- Model-selection tests and a vignette example.
- Corrected Version 3.2 versus Version 4.0 validation metadata.

## Compatibility gates

The frozen manuscript-code regression remains the first gate. The Version 3.2
compatibility Rmd requires a local `validation/msma_3.2.tar.gz` reference
archive and must report zero failures before release.

## Deferred

- Bayesian PPCA, WAIC, LOO, and Bayesian conformal scoring.
- Parallel grid execution.
- Multiblock and PLS structured selection.


