# Run from the package development root or from validation/.
# All paths passed to rmarkdown are absolute so rendering remains valid after
# changing the working directory.

start_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
if (file.exists(file.path(start_dir, "src.r"))) {
  root <- start_dir
} else if (file.exists(file.path(start_dir, "..", "src.r"))) {
  root <- normalizePath(file.path(start_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package development root or validation directory")
}

validation_dir <- normalizePath(file.path(root, "validation"), winslash = "/", mustWork = TRUE)
input_rmd <- normalizePath(
  file.path(validation_dir, "msma_3.2_vs_4.0_compatibility_test.Rmd"),
  winslash = "/", mustWork = TRUE
)
latest_source <- normalizePath(file.path(root, "src.r"), winslash = "/", mustWork = TRUE)
reference <- file.path(validation_dir, "msma_3.2.tar.gz")
output_file <- file.path(validation_dir, "msma_3.2_vs_4.0_compatibility_test.html")

if (!file.exists(reference)) {
  urls <- c(
    "https://cran.r-project.org/src/contrib/msma_3.2.tar.gz",
    "https://cran.r-project.org/src/contrib/Archive/msma/msma_3.2.tar.gz",
    "https://mirrors.tuna.tsinghua.edu.cn/CRAN/src/contrib/msma_3.2.tar.gz"
  )
  downloaded <- FALSE
  for (u in urls) {
    status <- try(utils::download.file(u, reference, mode = "wb", quiet = FALSE), silent = TRUE)
    if (!inherits(status, "try-error") && file.exists(reference) && file.info(reference)$size > 0) {
      downloaded <- TRUE
      break
    }
    if (file.exists(reference)) unlink(reference)
  }
  if (!downloaded) stop("Could not download the msma 3.2 source archive")
}

if (!requireNamespace("rmarkdown", quietly = TRUE)) stop("Install rmarkdown")
output <- rmarkdown::render(
  input = input_rmd,
  output_file = basename(output_file),
  output_dir = validation_dir,
  knit_root_dir = validation_dir,
  params = list(
    legacy_source = normalizePath(reference, winslash = "/", mustWork = TRUE),
    latest_source = latest_source,
    tolerance = 1e-8,
    strict = TRUE,
    run_cv = TRUE,
    run_search = FALSE
  ),
  envir = new.env(parent = globalenv()),
  quiet = FALSE
)
cat("Compatibility report:", normalizePath(output, winslash = "/"), "\n")


