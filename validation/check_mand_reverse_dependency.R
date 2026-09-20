# Reverse-dependency audit for mand against the locally built msma 4.0.
# Run from the package development root or from validation/.

start_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (file.exists(file.path(start_dir, "msma_4.0.tar.gz"))) {
  root <- start_dir
} else if (file.exists(file.path(start_dir, "..", "msma_4.0.tar.gz"))) {
  root <- normalizePath(file.path(start_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  stop("Build msma_4.0.tar.gz and run from the development root or validation directory")
}

msma_tar <- normalizePath(file.path(root, "msma_4.0.tar.gz"), winslash = "/", mustWork = TRUE)
work <- file.path(root, "validation", "mand-revdep")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
lib <- file.path(work, "library")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
lib <- normalizePath(lib, winslash = "/", mustWork = TRUE)
rbin <- file.path(R.home("bin"), "R.exe")
if (!file.exists(rbin)) rbin <- file.path(R.home("bin"), "R")

# R CMD INSTALL uses -l/--library=<path>; passing --library as a separate token
# is not portable on Windows.
install_status <- system2(
  rbin,
  c("CMD", "INSTALL", paste0("--library=", shQuote(lib)), shQuote(msma_tar))
)
if (install_status != 0L) stop("Failed to install msma 4.0 into the isolated library")

installed_desc <- read.dcf(file.path(lib, "msma", "DESCRIPTION"))
if (installed_desc[1, "Version"] != "4.0") stop("Isolated library does not contain msma 4.0")
cat("Isolated msma:", file.path(lib, "msma"), "version", installed_desc[1, "Version"], "\n")

old <- setwd(work)
on.exit(setwd(old), add = TRUE)
existing <- list.files(".", pattern = "^mand_.*[.]tar[.]gz$", full.names = TRUE)
if (!length(existing)) {
  utils::download.packages("mand", destdir = ".", type = "source", repos = "https://cloud.r-project.org")
}
mand_tar <- list.files(".", pattern = "^mand_.*[.]tar[.]gz$", full.names = TRUE)
if (length(mand_tar) != 1L) stop("Could not uniquely identify the mand source archive")
mand_tar <- normalizePath(mand_tar, winslash = "/", mustWork = TRUE)

# Put the isolated library first while retaining the standard libraries for
# mand's other dependencies.
libs <- paste(c(lib, .libPaths()), collapse = .Platform$path.sep)
env <- c(paste0("R_LIBS=", libs), paste0("R_LIBS_USER=", libs))
check_status <- system2(
  rbin,
  c("CMD", "check", "--no-manual", "--no-vignettes", shQuote(mand_tar)),
  env = env
)

# Depending on the Windows R front end, R CMD check may place mand.Rcheck in
# the requested working directory or in the development root. Search both
# deterministic locations and require exactly one valid log.
candidates <- unique(c(
  file.path(work, "mand.Rcheck"),
  file.path(root, "mand.Rcheck"),
  file.path(getwd(), "mand.Rcheck")
))
logs <- file.path(candidates, "00check.log")
logs <- logs[file.exists(logs)]
if (!length(logs)) {
  stop("Could not locate mand.Rcheck/00check.log in: ",
       paste(candidates, collapse = ", "))
}
if (length(logs) > 1L) {
  info <- file.info(logs)
  logs <- logs[which.max(info$mtime)]
}
log <- logs[1L]
log_lines <- readLines(log, warn = FALSE)
cat(log_lines, sep = "\n")

# Reverse-dependency compatibility is judged on installation, loading,
# examples, and tests. CRAN incoming feasibility is intentionally excluded:
# checking the already-published mand 3.0 source with --as-cran produces the
# expected same-version submission warning, which is unrelated to msma.
bad <- grep("^Status:.*(ERROR|WARNING)", log_lines, value = TRUE)
if (check_status != 0L || length(bad)) {
  stop("mand reverse-dependency compatibility check failed")
}
cat("mand reverse-dependency compatibility check passed\n")
cat("Check log:", normalizePath(log, winslash = "/"), "\n")


