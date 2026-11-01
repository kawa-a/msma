# M3-F: Final integrated audit of the Version 5 prediction contract.
# Run from the msma Version 5 repository root.

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))
source("src.r", local = globalenv())

out_dir <- file.path("development", "v5-prediction")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
result_file <- file.path(out_dir, "M3-F-PREDICTION-INTEGRATION-AUDIT.txt")
rds_file <- file.path(out_dir, "m3f-prediction-integration-audit.rds")

summarize_results <- function(result) {
  x <- as.data.frame(result)
  c(
    failed = sum(x$failed, na.rm = TRUE),
    error = sum(x$error, na.rm = TRUE),
    skipped = sum(x$skipped, na.rm = TRUE),
    warning = sum(x$warning, na.rm = TRUE),
    passed = sum(x$passed, na.rm = TRUE)
  )
}

focused_files <- c(
  "test-v5-prediction-baseline.R",
  "test-v5-prediction-fitted.R",
  "test-v5-prediction-reconstruction.R",
  "test-v5-prediction-output-scale.R",
  "test-v5-preprocessing.R",
  "test-v5-preprocessing-serialization.R"
)
focused_paths <- file.path("tests", "testthat", focused_files)
stopifnot(all(file.exists(focused_paths)))

focused <- lapply(focused_paths, function(path) {
  result <- testthat::test_file(path, reporter = "silent")
  list(file = path, summary = summarize_results(result))
})
names(focused) <- focused_files

full <- testthat::test_dir(
  "tests/testthat",
  reporter = "silent",
  stop_on_failure = FALSE
)
full_summary <- summarize_results(full)

# Contract smoke audit --------------------------------------------------------
set.seed(1701)
X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
Y <- matrix(stats::rnorm(160), nrow = 40, ncol = 4)
colnames(X) <- paste0("x", seq_len(ncol(X)))
colnames(Y) <- paste0("y", seq_len(ncol(Y)))
Xnew <- matrix(stats::rnorm(60), nrow = 10, ncol = 6)
Ynew <- matrix(stats::rnorm(40), nrow = 10, ncol = 4)
colnames(Xnew) <- colnames(X)
colnames(Ynew) <- colnames(Y)

fit_pca <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
fit_pls <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

legacy <- predict(fit_pca, newX = Xnew)
fitted_batch <- predict(
  fit_pca, newX = Xnew,
  preprocessing = "fitted",
  reconstruction = "batch",
  output_scale = "analysis"
)
fitted_analysis <- predict(
  fit_pca, newX = Xnew,
  preprocessing = "fitted",
  reconstruction = "fitted",
  output_scale = "analysis"
)
fitted_original <- predict(
  fit_pca, newX = Xnew,
  preprocessing = "fitted",
  reconstruction = "fitted",
  output_scale = "original"
)
pls_original <- predict(
  fit_pls, newX = Xnew, newY = Ynew,
  preprocessing = "fitted",
  reconstruction = "fitted",
  output_scale = "original"
)

manual_original <- .invert_msma_preprocessor(
  fit_pca$preprocess,
  fitted_analysis$X,
  side = "X"
)

set.seed(1702)
target <- matrix(stats::rnorm(6), nrow = 1, ncol = 6)
colnames(target) <- colnames(X)
batch_a <- rbind(target, matrix(stats::rnorm(54), nrow = 9, ncol = 6))
batch_b <- rbind(target, matrix(stats::rnorm(54, mean = 20, sd = 5), nrow = 9, ncol = 6))
colnames(batch_a) <- colnames(X)
colnames(batch_b) <- colnames(X)

fixed_a <- predict(
  fit_pca, newX = batch_a,
  preprocessing = "fitted",
  reconstruction = "fitted",
  output_scale = "original"
)
fixed_b <- predict(
  fit_pca, newX = batch_b,
  preprocessing = "fitted",
  reconstruction = "fitted",
  output_scale = "original"
)

path <- tempfile(fileext = ".rds")
saveRDS(fit_pca, path)
restored <- readRDS(path)
unlink(path)
restored_prediction <- predict(
  restored, newX = Xnew,
  preprocessing = "fitted",
  reconstruction = "fitted",
  output_scale = "original"
)

contract <- c(
  public_formals_unchanged = identical(
    names(formals(predict.msma)),
    c("object", "newX", "newY", "...")
  ),
  preprocess_metadata = inherits(fit_pca$preprocess, "msma_preprocess"),
  reconstruction_metadata = inherits(
    fit_pca$prediction_reconstruction,
    "msma_prediction_reconstruction"
  ),
  legacy_shape = identical(names(legacy), c("X", "sbX", "Y", "sbY")),
  fitted_batch_scores_equal = identical(fitted_batch$sbX, fitted_analysis$sbX),
  reconstruction_modes_differ = !isTRUE(all.equal(fitted_batch$X, fitted_analysis$X)),
  original_equals_manual_inverse = isTRUE(all.equal(
    fitted_original$X,
    manual_original,
    tolerance = 1e-15
  )),
  pls_original_finite = all(is.finite(pls_original$X[[1]])) &&
    all(is.finite(pls_original$Y[[1]])),
  fixed_score_batch_independent = isTRUE(all.equal(
    fixed_a$sbX[[1]][1, ], fixed_b$sbX[[1]][1, ], tolerance = 1e-15
  )),
  fixed_reconstruction_batch_independent = isTRUE(all.equal(
    fixed_a$X[[1]][1, ], fixed_b$X[[1]][1, ], tolerance = 1e-15
  )),
  serialization_identical = identical(restored_prediction, fitted_original)
)

focused_ok <- all(vapply(focused, function(x) {
  all(x$summary[c("failed", "error", "skipped", "warning")] == 0)
}, logical(1)))
full_ok <- all(full_summary[c("failed", "error", "skipped", "warning")] == 0)
contract_ok <- all(contract)

status <- if (focused_ok && full_ok && contract_ok) "COMPLETE" else "FAILED"

audit <- list(
  metadata = list(
    date = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
    branch = system2("git", c("branch", "--show-current"), stdout = TRUE),
    commit = system2("git", c("rev-parse", "HEAD"), stdout = TRUE),
    status = status
  ),
  focused = focused,
  full = full_summary,
  contract = contract
)
saveRDS(audit, rds_file, version = 3)

lines <- c(
  "msma Version 5 M3-F prediction integration audit",
  paste0("Date: ", audit$metadata$date),
  paste0("Branch: ", audit$metadata$branch),
  paste0("Commit: ", audit$metadata$commit),
  "",
  "Focused test suites:"
)
for (nm in names(focused)) {
  s <- focused[[nm]]$summary
  lines <- c(lines, paste0("* ", nm, ": ", paste(names(s), s, sep = "=", collapse = ", ")))
}
lines <- c(
  lines,
  "",
  paste0("Full regression suite: ", paste(names(full_summary), full_summary, sep = "=", collapse = ", ")),
  "",
  "Integrated prediction contract:"
)
lines <- c(lines, paste0("* ", names(contract), ": ", unname(contract)))
lines <- c(
  lines,
  "",
  "Compatibility hierarchy:",
  "* default predict.msma remains legacy behavior",
  "* fitted preprocessing defaults to batch reconstruction",
  "* fitted reconstruction uses training-derived coefficients",
  "* analysis output remains the default scale",
  "* original output applies fitted inverse preprocessing",
  "* component scores remain on the analysis scale",
  "",
  paste0("M3-F status: ", status)
)
writeLines(lines, result_file, useBytes = TRUE)

cat("M3-F audit report: ", normalizePath(result_file, winslash = "/"), "\n", sep = "")
cat("M3-F audit data: ", normalizePath(rds_file, winslash = "/"), "\n", sep = "")
cat("M3-F status: ", status, "\n", sep = "")
if (!identical(status, "COMPLETE")) stop("M3-F audit failed.", call. = FALSE)
