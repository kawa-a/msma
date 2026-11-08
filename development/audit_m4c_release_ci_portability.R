# M4-C: Release artifact, CI, and portability audit for msma Version 5.
# Run from the repository root after M4-B.
# This audit does not modify tracked implementation files.

required_paths <- c(
  "src.r", "DESCRIPTION", "NEWS", "README.md", "create_4.0.R",
  "tests/testthat", "vignettes"
)
stopifnot(all(file.exists(required_paths) | dir.exists(required_paths)))

out_dir <- file.path("development", "v5-release-audit")
log_dir <- file.path(out_dir, "logs")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

report_file <- file.path(out_dir, "M4-C-RELEASE-CI-PORTABILITY-AUDIT.txt")
rds_file <- file.path(out_dir, "m4c-release-ci-portability-audit.rds")
manifest_file <- file.path(out_dir, "m4c-source-package-manifest.csv")
ci_file <- file.path(out_dir, "m4c-ci-workflow-inventory.csv")

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# Convert a path to a valid R string literal with ASCII quotes.
as_r_string <- function(x) {
  encodeString(
    x,
    quote = "\""
  )
}


capture_command <- function(command, args, log_file, wd = getwd(), env = character()) {
  original_wd <- getwd()
  log_file <- normalizePath(log_file, winslash = "/", mustWork = FALSE)
  dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
  on.exit(setwd(original_wd), add = TRUE)
  setwd(wd)

  output <- tryCatch(
    system2(command, args, stdout = TRUE, stderr = TRUE, env = env),
    error = function(e) structure(conditionMessage(e), status = 127L)
  )
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L

  writeLines(
    c(
      paste("Command:", command, paste(args, collapse = " ")),
      paste0("Working directory: ", normalizePath(wd, winslash = "/", mustWork = FALSE)),
      paste0("Exit status: ", status),
      "",
      as.character(output)
    ),
    log_file,
    useBytes = TRUE
  )

  list(status = as.integer(status), output = as.character(output), log = log_file)
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
  pandoc = if (requireNamespace("rmarkdown", quietly = TRUE)) as.character(rmarkdown::pandoc_version()) else NA_character_
)

# 1. Repository and CI inventory ---------------------------------------------
workflow_dir <- file.path(".github", "workflows")
workflow_files <- if (dir.exists(workflow_dir)) {
  list.files(workflow_dir, pattern = "[.](yml|yaml)$", full.names = TRUE)
} else character()

workflow_inventory <- if (length(workflow_files)) {
  do.call(rbind, lapply(workflow_files, function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    text <- paste(lines, collapse = "\n")
    data.frame(
      file = gsub("\\\\", "/", path),
      windows = grepl("windows", text, ignore.case = TRUE),
      macos = grepl("macos", text, ignore.case = TRUE),
      ubuntu = grepl("ubuntu|linux", text, ignore.case = TRUE),
      r_cmd_check = grepl("check-r-package|R CMD check|rcmdcheck|create_4[.]0[.]R|run_milestone6_validation", text, ignore.case = TRUE),
      cache = grepl("cache", text, ignore.case = TRUE),
      stringsAsFactors = FALSE
    )
  }))
} else {
  data.frame(
    file = character(), windows = logical(), macos = logical(),
    ubuntu = logical(), r_cmd_check = logical(), cache = logical()
  )
}
write.csv(workflow_inventory, ci_file, row.names = FALSE, fileEncoding = "UTF-8")

ci_contract <- c(
  workflow_present = nrow(workflow_inventory) > 0L,
  windows_declared = any(workflow_inventory$windows),
  macos_declared = any(workflow_inventory$macos),
  linux_declared = any(workflow_inventory$ubuntu),
  package_check_declared = any(workflow_inventory$r_cmd_check)
)

# 2. Clean-session source tests ----------------------------------------------
r_bin <- file.path(R.home("bin"), "R")
if (.Platform$OS.type == "windows") r_bin <- paste0(r_bin, ".exe")

clean_test_script <- file.path(tempdir(), "msma-m4c-clean-tests.R")
writeLines(
  c(
    sprintf("setwd(%s)", as_r_string(repo_root)),
    "source('src.r', local = globalenv())",
    "result <- testthat::test_dir('tests/testthat', reporter = 'silent', stop_on_failure = FALSE)",
    "x <- as.data.frame(result)",
    "summary <- c(failed=sum(x$failed,na.rm=TRUE),error=sum(x$error,na.rm=TRUE),skipped=sum(x$skipped,na.rm=TRUE),warning=sum(x$warning,na.rm=TRUE),passed=sum(x$passed,na.rm=TRUE))",
    "print(summary)",
    "if (any(summary[c('failed','error','skipped','warning')] != 0)) quit(status=1L)",
    "quit(status=0L)"
  ),
  clean_test_script,
  useBytes = TRUE
)
clean_test_result <- capture_command(
  r_bin,
  c("--vanilla", "--slave", "-f", clean_test_script),
  file.path(log_dir, "clean-session-tests.log"),
  wd = repo_root
)

# 3. Build a clean source package --------------------------------------------
tmp_root <- file.path(tempdir(), "msma-m4c-release")
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
  dir.create(file.path(package_dir, "inst"), recursive = TRUE, showWarnings = FALSE)
  file.copy("CITATION", file.path(package_dir, "inst", "CITATION"), overwrite = TRUE)
}
file.copy("tests", package_dir, recursive = TRUE, overwrite = TRUE)
file.copy("vignettes", package_dir, recursive = TRUE, overwrite = TRUE)

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

build_result <- capture_command(
  r_bin,
  c("CMD", "build", package_dir, "--no-manual"),
  file.path(log_dir, "R-CMD-build.log"),
  wd = tmp_root
)
tarballs <- list.files(tmp_root, pattern = "[.]tar[.]gz$", full.names = TRUE)
tarball <- if (length(tarballs)) tarballs[[which.max(file.info(tarballs)$mtime)]] else NA_character_

# 4. Artifact manifest and reproducibility metadata --------------------------
tar_contents <- if (!is.na(tarball) && file.exists(tarball)) utils::untar(tarball, list = TRUE) else character()
manifest <- data.frame(path = tar_contents, stringsAsFactors = FALSE)
write.csv(manifest, manifest_file, row.names = FALSE, fileEncoding = "UTF-8")

artifact_hash <- if (!is.na(tarball) && file.exists(tarball)) {
  unname(tools::md5sum(tarball))
} else NA_character_

content_contract <- c(
  has_description = any(grepl("/DESCRIPTION$", tar_contents)),
  has_namespace = any(grepl("/NAMESPACE$", tar_contents)),
  has_news = any(grepl("/NEWS$", tar_contents)),
  has_predict_rd = any(grepl("/man/predict[.]msma[.]Rd$", tar_contents)),
  has_msma_rd = any(grepl("/man/msma[.]Rd$", tar_contents)),
  has_tests = any(grepl("/tests/testthat/", tar_contents)),
  has_vignettes = any(grepl("/vignettes/", tar_contents)),
  excludes_development = !any(grepl("/development/", tar_contents)),
  excludes_git = !any(grepl("/[.]git/", tar_contents)),
  excludes_backup = !any(grepl("backup|src-before", tar_contents, ignore.case = TRUE))
)

# 5. Install into an isolated library and inspect exports --------------------
install_lib <- file.path(tmp_root, "library")
dir.create(install_lib, recursive = TRUE)
install_result <- if (!is.na(tarball) && file.exists(tarball)) {
  capture_command(
    r_bin,
    c("CMD", "INSTALL", "--library", install_lib, tarball),
    file.path(log_dir, "R-CMD-INSTALL.log"),
    wd = tmp_root
  )
} else list(status = NA_integer_, output = character(), log = NA_character_)

installed_audit_script <- file.path(tempdir(), "msma-m4c-installed-audit.R")
installed_audit_rds <- file.path(tmp_root, "installed-audit.rds")
writeLines(
  c(
    sprintf(".libPaths(c(%s, .libPaths()))", as_r_string(normalizePath(install_lib, winslash = "/", mustWork = FALSE))),
    "library(msma)",
    "exports <- getNamespaceExports('msma')",
    "s3 <- getNamespaceInfo(asNamespace('msma'), 'S3methods')",
    "set.seed(4101)",
    "X <- matrix(rnorm(180), nrow=30, ncol=6)",
    "colnames(X) <- paste0('x', seq_len(ncol(X)))",
    "fit <- msma(X=X, comp=2, scaling=TRUE, intseed=3)",
    "pred <- predict(fit, newX=X[1:5,,drop=FALSE], preprocessing='fitted', reconstruction='fitted', output_scale='original')",
    "audit <- list(package_version=as.character(packageVersion('msma')), package_path=find.package('msma'), exports=exports, s3=s3, prediction_names=names(pred), prediction_finite=all(is.finite(pred$X[[1]])), preprocess=inherits(fit$preprocess,'msma_preprocess'), reconstruction=inherits(fit$prediction_reconstruction,'msma_prediction_reconstruction'))",
    sprintf("saveRDS(audit, %s)", as_r_string(normalizePath(installed_audit_rds, winslash = "/", mustWork = FALSE))),
    "quit(status=0L)"
  ),
  installed_audit_script,
  useBytes = TRUE
)
installed_result <- if (identical(install_result$status, 0L)) {
  capture_command(
    r_bin,
    c("--vanilla", "--slave", "-f", installed_audit_script),
    file.path(log_dir, "installed-package-audit.log"),
    wd = tmp_root
  )
} else list(status = NA_integer_, output = character(), log = NA_character_)
installed_audit <- if (file.exists(installed_audit_rds)) readRDS(installed_audit_rds) else NULL

installed_contract <- if (!is.null(installed_audit)) c(
  exports_msma = "msma" %in% installed_audit$exports,
  exports_predict_helpers = all(c("s4pca_reconstruct", "s4pca_conformal_select") %in% installed_audit$exports),
  prediction_names = identical(installed_audit$prediction_names, c("X", "sbX", "Y", "sbY")),
  prediction_finite = isTRUE(installed_audit$prediction_finite),
  preprocess_metadata = isTRUE(installed_audit$preprocess),
  reconstruction_metadata = isTRUE(installed_audit$reconstruction)
) else c(
  exports_msma = FALSE, exports_predict_helpers = FALSE, prediction_names = FALSE,
  prediction_finite = FALSE, preprocess_metadata = FALSE, reconstruction_metadata = FALSE
)

# 6. R CMD check on the built artifact ---------------------------------------
check_dir <- file.path(tmp_root, "check-as-cran")
dir.create(check_dir, recursive = TRUE)

check_result <- if (!is.na(tarball) && file.exists(tarball)) {
  capture_command(
    r_bin,
    c("CMD", "check", tarball, "--as-cran", "--no-manual"),
    file.path(log_dir, "R-CMD-check.log"),
    wd = check_dir
  )
} else {
  list(status = NA_integer_, output = character(), log = NA_character_)
}

as_cran_output <- check_result$output

cran_ssl_failure <-
  !identical(check_result$status, 0L) &&
  any(grepl(
    "CRAN incoming feasibility",
    as_cran_output,
    fixed = TRUE
  )) &&
  any(grepl(
    "SSL connect error|cannot open the connection",
    as_cran_output,
    ignore.case = TRUE
  ))

effective_check_result <- check_result
effective_check_dir <- check_dir
check_mode <- "as-cran"

if (cran_ssl_failure) {
  standard_check_dir <- file.path(tmp_root, "check-standard")
  dir.create(
    standard_check_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  standard_check_result <- capture_command(
    r_bin,
    c("CMD", "check", tarball, "--no-manual"),
    file.path(log_dir, "R-CMD-check-standard.log"),
    wd = standard_check_dir
  )

  effective_check_result <- standard_check_result
  effective_check_dir <- standard_check_dir
  check_mode <- "standard-fallback-after-cran-ssl-failure"
} else {
  standard_check_result <- NULL
}

check_logs <- list.files(
  effective_check_dir,
  pattern = "00check[.]log$",
  recursive = TRUE,
  full.names = TRUE
)

check_summary <- c(
  errors = NA_integer_,
  warnings = NA_integer_,
  notes = NA_integer_
)

check_status_line <- character()

if (length(check_logs)) {
  check_lines <- readLines(
    check_logs[[1]],
    warn = FALSE
  )

  check_status_line <- grep(
    "Status:",
    check_lines,
    value = TRUE
  )

  if (!length(check_status_line) &&
      any(grepl("* DONE", check_lines, fixed = TRUE))) {
    check_status_line <- "Status: OK"
  }

  if (length(check_status_line)) {
    count_status <- function(label) {
      hit <- regmatches(
        check_status_line[[1]],
        regexpr(
          paste0("[0-9]+ ", label),
          check_status_line[[1]],
          ignore.case = TRUE
        )
      )

      if (!length(hit) || identical(hit, "")) {
        0L
      } else {
        as.integer(
          strsplit(hit, " ", fixed = TRUE)[[1]][1]
        )
      }
    }

    check_summary <- c(
      errors = count_status("ERROR"),
      warnings = count_status("WARNING"),
      notes = count_status("NOTE")
    )
  }
}

# 7. Final result -------------------------------------------------------------
mandatory <- c(
  all(ci_contract[c("workflow_present", "package_check_declared")]),
  identical(clean_test_result$status, 0L),
  is.null(roxygen_error),
  identical(build_result$status, 0L),
  all(content_contract),
  identical(install_result$status, 0L),
  identical(installed_result$status, 0L),
  all(installed_contract),
  identical(effective_check_result$status, 0L),
  identical(unname(check_summary[["errors"]]), 0L),
  identical(unname(check_summary[["warnings"]]), 0L)
)
status <- if (all(mandatory)) "COMPLETE" else "FAILED"

result <- list(
  metadata = metadata,
  ci = list(inventory = workflow_inventory, contract = ci_contract),
  clean_tests = clean_test_result,
  roxygen = list(messages = roxygen_messages, warnings = roxygen_warnings, error = roxygen_error),
  build = list(status = build_result$status, tarball = tarball, md5 = artifact_hash, log = build_result$log),
  content = content_contract,
  install = install_result,
  installed = list(audit = installed_audit, contract = installed_contract, result = installed_result),
  check = list(mode = check_mode, cran_ssl_failure = cran_ssl_failure, as_cran_status = check_result$status, status = effective_check_result$status, summary = check_summary, status_line = check_status_line, log = effective_check_result$log),
  status = status
)
saveRDS(result, rds_file, version = 3)

lines <- c(
  "msma Version 5 M4-C release artifact, CI, and portability audit",
  paste0("Date: ", metadata$date),
  paste0("Branch: ", metadata$branch),
  paste0("Commit: ", metadata$commit),
  paste0("R: ", metadata$r_version),
  paste0("Platform: ", metadata$platform),
  paste0("Pandoc: ", metadata$pandoc),
  "",
  paste0("CI contract: ", paste(names(ci_contract), ci_contract, sep = "=", collapse = ", ")),
  paste0("Clean-session tests status: ", clean_test_result$status),
  paste0("Roxygen error: ", if (is.null(roxygen_error)) "NONE" else roxygen_error),
  paste0("Roxygen warnings: ", length(roxygen_warnings)),
  paste0("Build status: ", build_result$status),
  paste0("Artifact: ", tarball),
  paste0("Artifact MD5: ", artifact_hash),
  paste0("Content contract: ", paste(names(content_contract), content_contract, sep = "=", collapse = ", ")),
  paste0("Install status: ", install_result$status),
  paste0("Installed audit status: ", installed_result$status),
  paste0("Installed contract: ", paste(names(installed_contract), installed_contract, sep = "=", collapse = ", ")),
  paste0("R CMD check mode: ", check_mode),
  paste0("CRAN SSL failure: ", cran_ssl_failure),
  paste0("R CMD check as-cran status: ", check_result$status),
  paste0("R CMD check effective status: ", effective_check_result$status),
  paste0("R CMD check summary: ", paste(names(check_summary), check_summary, sep = "=", collapse = ", ")),
  paste0("R CMD check status line: ", paste(check_status_line, collapse = " | ")),
  "",
  "Portability interpretation:",
  "* Local Windows build, install, clean-session tests, and R CMD check are executable.",
  "* Declared CI operating systems are inventoried from .github/workflows.",
  "* Cross-platform execution is confirmed only by the external CI runs, not by this local script.",
  "* Development-version CRAN incoming NOTE is recorded separately from errors and warnings.",
  "",
  paste0("M4-C status: ", status)
)
writeLines(lines, report_file, useBytes = TRUE)

cat("M4-C report: ", normalizePath(report_file, winslash = "/"), "\n", sep = "")
cat("M4-C data: ", normalizePath(rds_file, winslash = "/"), "\n", sep = "")
cat("M4-C status: ", status, "\n", sep = "")
cat("R CMD check: ", paste(names(check_summary), check_summary, sep = "=", collapse = ", "), "\n", sep = "")
