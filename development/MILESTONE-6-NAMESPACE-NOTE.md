# Post-build namespace refresh

`roxygen2::roxygenize()` loads the source-tree package before the regenerated
NAMESPACE is installed. In the same R session, `getNamespaceExports("msma")`
can therefore inspect that stale namespace even though the source archive and
installed package contain the correct exports. `R CMD check` runs in a fresh
process and was unaffected.

The build script now unloads the source-tree namespace and reloads the newly
installed package after the check. This makes a subsequent interactive
`testthat::test_dir()` inspect the current installed namespace.

The additional source-tree NAMESPACE inspection was removed because the root
NAMESPACE is created only inside the temporary package-skeleton directory.
The installed-namespace export test is authoritative and runs without skips.


