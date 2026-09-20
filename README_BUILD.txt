msma 4.0 development source

1. Place this directory in a writable location.
2. Start R in this directory.
3. Run: source("create_4.0.R")
4. Review msma.Rcheck/00check.log.
5. The CRAN source archive is msma_4.0.tar.gz.

The build script regenerates NAMESPACE and man/*.Rd from src.r using roxygen2.

Compatibility validation
------------------------
Run validation/msma_3.1_vs_3.2_compatibility_test_fixed.Rmd from its own
folder. Its default paths are msma_3.1.tar.gz and ../src.r. Place the legacy archive in the validation directory.

If Pandoc is unavailable, create_4.0.R builds and checks the package without
vignettes. This is reported as an informational message rather than a warning.

Version 4.0 development
-----------------------
The initial scaffold does not yet expose soft-structured estimation. Complete
`development/S4PCA-IMPLEMENTATION-MAP.md` and validate the isolated
one-component implementation before changing the public dispatcher.


