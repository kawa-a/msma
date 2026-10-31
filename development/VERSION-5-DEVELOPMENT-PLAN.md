# msma Version 5 Development Plan

## Baseline

Version 5 development starts from the audited Version 4.0 implementation.

Initial baseline:

- R version 4.5.2
- 569 passed expectations
- 0 failures
- 0 errors
- 0 warnings
- 0 skips

The initial requirement is numerical equivalence with Version 4.0 for all
existing public interfaces and tested computational paths.

## Milestone 0: Freeze the Version 4 baseline

- Preserve all Version 4.0 tests.
- Preserve the manuscript-reference regression tests.
- Preserve the Version 3.2 compatibility tests.
- Preserve soft and exclusive S4PCA results.
- Preserve reconstruction, overlap, and model-selection results.
- Record the baseline commit and fixed-seed behavior.

## Milestone 1: Estimation-engine dispatch

Separate the internal computational paths into explicit engines:

- legacy engine;
- extended engine;
- soft-structured engine.

The public msma() interface and Version 4.0 numerical behavior must remain
backward compatible.

## Milestone 2: Common preprocessing infrastructure

Develop internal components for:

- fitting preprocessing transformations;
- applying fitted transformations to new data;
- validating preprocessing metadata;
- storing centering and scaling consistently.

Existing public preprocessing fields must remain available.

## Milestone 3: Structured control objects

Introduce internal control structures for:

- estimation engine;
- convergence;
- structured penalties;
- supervision;
- missing modalities;
- network methods;
- random seeds.

Existing public arguments will initially be translated into internal controls.

## Milestone 4: Unified result and diagnostic structure

Add consistent internal locations for:

- parameters;
- preprocessing;
- loadings;
- scores;
- reconstruction;
- selection results;
- diagnostics.

Existing Version 4.0 result elements must remain available.

## Additional functions after stabilization

- msma_heatmap();
- explicit scaleZ handling;
- msma_select();
- cross-validation;
- calibration-quantile selection;
- information criteria;
- stability-based selection;
- parallel candidate evaluation.

## Later major releases

- Version 6: missing-modality methods.
- Version 7: network-integrated estimation.
- Future release: multiblock soft-structured estimation.
