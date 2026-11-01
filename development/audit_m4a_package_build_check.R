# M4-A: Package-wide build/check baseline audit for msma Version 5.
# Run from the repository root. This script does not modify tracked source files.

required <- c("src.r", "DESCRIPTION", "NEWS", "README.md", "create_4.0.R", "tests/testthat")
stopifnot(all(file.exists(required) | dir.exists(required)))

source("src.r", local = globalenv())

out_dir <- file.path("development", "v5-build-check")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
report_file <- file.path(out_dir, "M4-A-PACKAGE-BUILD-CHECK-AUDIT.txt")
rds_file <- file.path(out_dir, "m4a-package-build-check-audit.rds")
log_dir <- file.path(out_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

capture_command <- function(
  command,
  args,
  log_file,
  wd = getwd()
) {
  original_wd <- getwd()

  log_file <- normalizePath(
    log_file,
    winslash = "/",
    mustWork = FALSE
  )

  dir.create(
    dirname(log_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  on.exit(
    setwd(original_wd),
    add = TRUE
  )

  setwd(wd)

  output <- tryCatch(
    system2(
      command,
      args,
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(e) {
      structure(
        conditionMessage(e),
        status = 127L
      )
    }
  )

  status <- attr(
    output,
    "status"
  )

  if (is.null(status)) {
    status <- 0L
  }

  writeLines(
    c(
      paste(
        "Command:",
        command,
        paste(args, collapse = " ")
      ),
      paste0(
        "Working directory: ",
        normalizePath(
          wd,
          winslash = "/",
          mustWork = FALSE
        )
      ),
      paste0(
        "Exit status: ",
        status
      ),
      "",
      as.character(output)
    ),
    log_file,
    useBytes = TRUE
  )

  list(
    status = as.integer(status),
    log = log_file
  )
}

summarize_test_results <- function(result) {
  x <- as.data.frame(result)
  c(
    failed = sum(x$failed, na.rm = TRUE),
    error = sum(x$error, na.rm = TRUE),
    skipped = sum(x$skipped, na.rm = TRUE),
    warning = sum(x$warning, na.rm = TRUE),
    passed = sum(x$passed, na.rm = TRUE)
  )
}

metadata <- list(
  date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
  branch = system2("git", c("branch", "--show-current"), stdout = TRUE),
  commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
  r_version = R.version.string,
  platform = R.version$platform,
  pandoc = if (requireNamespace("rmarkdown", quietly = TRUE)) {
    as.character(rmarkdown::pandoc_version())
  } else {
    NA_character_
  }
)

# 1. Source and DESCRIPTION baseline -----------------------------------------
source_parse <- try(parse(file = "src.r"), silent = TRUE)
description <- read.dcf("DESCRIPTION")
description_status <- c(
  package = identical(unname(description[1, "Package"]), "msma"),
  version_present = nzchar(unname(description[1, "Version"])),
  encoding_utf8 = identical(unname(description[1, "Encoding"]), "UTF-8"),
  vignette_builder = identical(unname(description[1, "VignetteBuilder"]), "knitr")
)

# 2. Test baseline ------------------------------------------------------------
tests <- testthat::test_dir(
  "tests/testthat",
  reporter = "silent",
  stop_on_failure = FALSE
)
test_summary <- summarize_test_results(tests)

# 3. Temporary package skeleton and roxygen ----------------------------------
tmp_root <- file.path(tempdir(), "msma-m4a-build-check")
unlink(tmp_root, recursive = TRUE, force = TRUE)
dir.create(tmp_root, recursive = TRUE)

utils::package.skeleton(
  name = "msma",
  path = tmp_root,
  code_files = normalizePath("src.r", winslash = "/"),
  force = TRUE
)
package_dir <- file.path(tmp_root, "msma")

unlink(list.files(file.path(package_dir, "man"), pattern = "[.]Rd$", full.names = TRUE), force = TRUE)
unlink(file.path(package_dir, "NAMESPACE"), force = TRUE)
unlink(file.path(package_dir, "Read-and-delete-me"), force = TRUE)
file.copy("DESCRIPTION", file.path(package_dir, "DESCRIPTION"), overwrite = TRUE)
file.copy("NEWS", file.path(package_dir, "NEWS"), overwrite = TRUE)
if (file.exists("LICENSE")) file.copy("LICENSE", file.path(package_dir, "LICENSE"), overwrite = TRUE)
if (file.exists("CITATION")) {
  dir.create(file.path(package_dir, "inst"), showWarnings = FALSE)
  file.copy("CITATION", file.path(package_dir, "inst", "CITATION"), overwrite = TRUE)
}
if (dir.exists("tests")) file.copy("tests", package_dir, recursive = TRUE, overwrite = TRUE)
if (dir.exists("vignettes")) file.copy("vignettes", package_dir, recursive = TRUE, overwrite = TRUE)

roxygen_messages <- character()
roxygen_warnings <- character()
roxygen_error <- NULL
withCallingHandlers(
  tryCatch(
    roxygen2::roxygenize(package_dir),
    error = function(e) roxygen_error <<- conditionMessage(e)
  ),
  message = function(m) {
    roxygen_messages <<- c(roxygen_messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  },
  warning = function(w) {
    roxygen_warnings <<- c(roxygen_warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
writeLines(c(roxygen_messages, roxygen_warnings), file.path(log_dir, "roxygen.log"), useBytes = TRUE)

namespace_file <- file.path(package_dir, "NAMESPACE")
namespace_lines <- if (file.exists(namespace_file)) readLines(namespace_file, warn = FALSE) else character()
expected_s3 <- c(
  "S3method(msma,default)",
  "S3method(plot,msma)",
  "S3method(predict,msma)",
  "S3method(print,msma)",
  "S3method(print,summary.msma)",
  "S3method(summary,msma)"
)
s3_status <- setNames(vapply(expected_s3, function(x) {
  sum(grepl(x, namespace_lines, fixed = TRUE)) == 1L
}, logical(1)), expected_s3)

rd_files <- list.files(file.path(package_dir, "man"), pattern = "[.]Rd$", full.names = TRUE)
rd_check <- setNames(vapply(rd_files, function(path) {
  !inherits(try(tools::parse_Rd(path), silent = TRUE), "try-error")
}, logical(1)), basename(rd_files))

predict_rd <- file.path(package_dir, "man", "predict.msma.Rd")
predict_rd_lines <- if (file.exists(predict_rd)) readLines(predict_rd, warn = FALSE, encoding = "UTF-8") else character()
predict_rd_contract <- c(
  generated = file.exists(predict_rd),
  title = any(grepl("Project and Reconstruct New Data", predict_rd_lines, fixed = TRUE)),
  preprocessing = any(grepl("preprocessing", predict_rd_lines, fixed = TRUE)),
  reconstruction = any(grepl("reconstruction", predict_rd_lines, fixed = TRUE)),
  output_scale = any(grepl("output_scale", predict_rd_lines, fixed = TRUE)),
  interpretation = any(grepl("does not predict Y from X", predict_rd_lines, fixed = TRUE))
)

# 4. Render all source vignettes ---------------------------------------------
vignette_files <- list.files("vignettes", pattern = "[.](Rmd|rmd)$", full.names = TRUE)
vignette_results <- setNames(vector("list", length(vignette_files)), basename(vignette_files))
for (i in seq_along(vignette_files)) {
  path <- vignette_files[[i]]
  result <- try(rmarkdown::render(
    input = path,
    output_format = "html_document",
    output_dir = tempdir(),
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  ), silent = TRUE)
  vignette_results[[i]] <- list(
    success = !inherits(result, "try-error") && file.exists(result),
    output = if (inherits(result, "try-error")) as.character(result) else result
  )
}
vignette_status <- vapply(vignette_results, `[[`, logical(1), "success")

# 5. R CMD build and check ----------------------------------------------------
r_bin <- file.path(R.home("bin"), "R")
if (.Platform$OS.type == "windows") r_bin <- paste0(r_bin, ".exe")

build_log <- file.path(log_dir, "R-CMD-build.log")
build_result <- capture_command(
  r_bin,
  c("CMD", "build", package_dir, "--no-manual"),
  build_log,
  wd = tmp_root
)

built_tarballs <- list.files(tmp_root, pattern = "[.]tar[.]gz$", full.names = TRUE)
built_tarball <- if (length(built_tarballs)) built_tarballs[[which.max(file.info(built_tarballs)$mtime)]] else NA_character_

check_result <- list(status = NA_integer_, log = NA_character_)
check_summary <- c(errors = NA_integer_, warnings = NA_integer_, notes = NA_integer_)
check_dir <- file.path(tmp_root, "check")
if (!is.na(built_tarball) && identical(build_result$status, 0L)) {
  dir.create(check_dir, recursive = TRUE, showWarnings = FALSE)
  check_log <- file.path(log_dir, "R-CMD-check.log")
  check_result <- capture_command(
    r_bin,
    c("CMD", "check", built_tarball, "--as-cran", "--no-manual"),
    check_log,
    wd = check_dir
  )

  check_outputs <- list.files(check_dir, pattern = "00check[.]log$", recursive = TRUE, full.names = TRUE)
  if (length(check_outputs)) {
    check_lines <- readLines(check_outputs[[1]], warn = FALSE)
    status_line <- grep("Status:", check_lines, value = TRUE)
    if (length(status_line)) {
      count_status <- function(label) {
        hit <- regmatches(status_line[[1]], regexpr(paste0("[0-9]+ ", label), status_line[[1]], ignore.case = TRUE))
        if (!length(hit) || identical(hit, "")) 0L else as.integer(strsplit(hit, " ", fixed = TRUE)[[1]][1])
      }
      check_summary <- c(
        errors = count_status("ERROR"),
        warnings = count_status("WARNING"),
        notes = count_status("NOTE")
      )
    }
  }
}

# 6. Source-package content audit --------------------------------------------
tar_contents <- if (!is.na(built_tarball) && file.exists(built_tarball)) {
  utils::untar(built_tarball, list = TRUE)
} else character()
content_contract <- c(
  has_description = any(grepl("/DESCRIPTION$", tar_contents)),
  has_namespace = any(grepl("/NAMESPACE$", tar_contents)),
  has_predict_rd = any(grepl("/man/predict[.]msma[.]Rd$", tar_contents)),
  has_tests = any(grepl("/tests/testthat/", tar_contents)),
  has_vignettes = any(grepl("/vignettes/", tar_contents)),
  excludes_development = !any(grepl("/development/", tar_contents)),
  excludes_git = !any(grepl("/[.]git/", tar_contents))
)

# 7. Final status -------------------------------------------------------------
mandatory <- c(
  !inherits(source_parse, "try-error"),
  all(description_status),
  all(test_summary[c("failed", "error", "skipped", "warning")] == 0),
  is.null(roxygen_error),
  all(s3_status),
  length(rd_check) > 0L && all(rd_check),
  all(predict_rd_contract),
  all(vignette_status),
  identical(build_result$status, 0L),
  all(content_contract)
)
status <- if (all(mandatory)) "BASELINE COMPLETE" else "BASELINE FAILED"

result <- list(
  metadata = metadata,
  source_parse_ok = !inherits(source_parse, "try-error"),
  description = description_status,
  tests = test_summary,
  roxygen = list(messages = roxygen_messages, warnings = roxygen_warnings, error = roxygen_error),
  s3 = s3_status,
  rd = rd_check,
  predict_rd = predict_rd_contract,
  vignettes = vignette_results,
  build = c(status = build_result$status, tarball = built_tarball, log = build_result$log),
  check = list(status = check_result$status, summary = check_summary, log = check_result$log),
  content = content_contract,
  status = status
)
saveRDS(result, rds_file, version = 3)

lines <- c(
  "msma Version 5 M4-A package build/check baseline audit",
  paste0("Date: ", metadata$date),
  paste0("Branch: ", metadata$branch),
  paste0("Commit: ", metadata$commit),
  paste0("R: ", metadata$r_version),
  paste0("Platform: ", metadata$platform),
  paste0("Pandoc: ", metadata$pandoc),
  "",
  paste0("Source parse: ", result$source_parse_ok),
  paste0("DESCRIPTION: ", paste(names(description_status), description_status, sep = "=", collapse = ", ")),
  paste0("Tests: ", paste(names(test_summary), test_summary, sep = "=", collapse = ", ")),
  "",
  paste0("Roxygen error: ", if (is.null(roxygen_error)) "NONE" else roxygen_error),
  paste0("Roxygen warnings: ", length(roxygen_warnings)),
  paste0("Roxygen messages: ", length(roxygen_messages)),
  paste0("S3 registrations: ", paste(names(s3_status), s3_status, sep = "=", collapse = ", ")),
  paste0("Rd files parsed: ", sum(rd_check), "/", length(rd_check)),
  paste0("predict.msma Rd: ", paste(names(predict_rd_contract), predict_rd_contract, sep = "=", collapse = ", ")),
  "",
  paste0("Vignettes: ", paste(names(vignette_status), vignette_status, sep = "=", collapse = ", ")),
  paste0("R CMD build status: ", build_result$status),
  paste0("Built tarball: ", built_tarball),
  paste0("R CMD check status: ", check_result$status),
  paste0("R CMD check summary: ", paste(names(check_summary), check_summary, sep = "=", collapse = ", ")),
  "",
  paste0("Source-package content: ", paste(names(content_contract), content_contract, sep = "=", collapse = ", ")),
  "",
  "Interpretation:",
  "* Roxygen messages are audited separately from build/check failures.",
  "* The existing msma.default export diagnostic is retained for M4-B review.",
  "* M4-A does not modify tracked implementation or documentation files.",
  "",
  paste0("M4-A status: ", status)
)
writeLines(lines, report_file, useBytes = TRUE)

cat("M4-A report: ", normalizePath(report_file, winslash = "/"), "\n", sep = "")
cat("M4-A data: ", normalizePath(rds_file, winslash = "/"), "\n", sep = "")
cat("M4-A status: ", status, "\n", sep = "")
cat("R CMD check summary: ", paste(names(check_summary), check_summary, sep = "=", collapse = ", "), "\n", sep = "")
