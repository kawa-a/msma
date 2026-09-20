# Milestone 6 release checklist

## A. Frozen numerical regression

- [ ] `test-s4pca-reference-regression.R`: zero failures and zero skips.
- [ ] Tolerance remains `1e-12`.

## B. Package tests

- [ ] Validation and diagnostics tests pass.
- [ ] Public structured-PCA contract tests pass.
- [ ] Reconstruction and model-selection tests pass.
- [ ] Version 3.2 package regression tests pass.
- [ ] Full `testthat::test_dir()` run has zero failures and zero skips.

## C. Independent Version 3.2 compatibility

- [ ] Download and freeze CRAN `msma_3.2.tar.gz`.
- [ ] Render `validation/msma_3.2_vs_4.0_compatibility_test.Rmd`.
- [ ] Confirm `Failed: 0`.
- [ ] Archive the HTML report and the reference archive checksum.

## D. Vignettes and manuals

- [ ] Five vignettes are indexed after installation.
- [ ] Soft-structured vignette examples and terminology are reviewed.
- [ ] PDF and HTML manuals build successfully.

## E. Reverse dependency

- [ ] Run `validation/check_mand_reverse_dependency.R`.
- [ ] Confirm `mand` has no new ERROR, WARNING, or compatibility regression.
- [ ] Record the result in `cran-comments.md`.

## F. CRAN checks

- [ ] Windows local `R CMD check --as-cran`: Status OK.
- [ ] R-release remote check: Status OK.
- [ ] R-devel remote check: Status OK.
- [ ] R-oldrel remote check: Status OK.

## G. Release artifacts

- [ ] DESCRIPTION and NEWS are final.
- [ ] `cran-comments.md` contains actual final environments and results.
- [ ] Source tarball contains all five vignettes.
- [ ] Run `validation/create_release_manifest.R`.
- [ ] Freeze Git tag `v4.0` only after every item above passes.


