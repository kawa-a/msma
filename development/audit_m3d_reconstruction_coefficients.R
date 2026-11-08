# M3-D: Audit fixed reconstruction coefficients before implementation changes.
# Run from the msma Version 5 repository root.

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))
source("src.r", local = globalenv())

out_dir <- file.path("development", "v5-prediction")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

# Save current function bodies ------------------------------------------------
functions <- c(
  "predict.msma",
  ".predict_msma_legacy",
  ".predict_msma_fitted",
  "project",
  "s4pca_reconstruct"
)

function_status <- data.frame(
  function_name = functions,
  exists = vapply(functions, exists, logical(1), mode = "function", inherits = TRUE),
  stringsAsFactors = FALSE
)

capture.output(
  {
    cat("msma Version 5 M3-D reconstruction coefficient audit\n")
    cat(paste0("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), "\n"))
    cat(paste0("Branch: ", system2("git", c("branch", "--show-current"), stdout = TRUE), "\n"))
    cat(paste0("Commit: ", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n\n"))
    print(function_status, row.names = FALSE)
  },
  file = file.path(out_dir, "M3-D-RECONSTRUCTION-COEFFICIENT-AUDIT.txt")
)

for (fn in functions[function_status$exists]) {
  body_lines <- deparse(body(get(fn, mode = "function")), width.cutoff = 120L)
  safe_name <- gsub("[^A-Za-z0-9._-]", "-", fn)
  writeLines(
    sprintf("%4d | %s", seq_along(body_lines), body_lines),
    file.path(out_dir, paste0(safe_name, "-before-m3d.txt")),
    useBytes = TRUE
  )
}

# Source inventory ------------------------------------------------------------
patterns <- paste(
  c(
    "reconstruction_loadings",
    "project\\(",
    "wbX",
    "wbY",
    "sbX",
    "sbY",
    "score",
    "loadings",
    "crossprod",
    "solve\\(",
    "predict_msma"
  ),
  collapse = "|"
)

hits <- grep(patterns, src, ignore.case = TRUE)
context <- sort(unique(unlist(lapply(hits, function(i) {
  seq.int(max(1L, i - 5L), min(length(src), i + 10L))
}))))
writeLines(
  sprintf("%5d | %s", context, src[context]),
  file.path(out_dir, "M3-D-SOURCE-CONTEXT.txt"),
  useBytes = TRUE
)

# Runtime fixtures ------------------------------------------------------------
set.seed(1501)
X <- matrix(stats::rnorm(240), nrow = 40, ncol = 6)
Y <- matrix(stats::rnorm(160), nrow = 40, ncol = 4)
colnames(X) <- paste0("x", seq_len(ncol(X)))
colnames(Y) <- paste0("y", seq_len(ncol(Y)))

fit_pca <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)
fit_pls <- msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3)

summarize_fit <- function(fit, fit_name) {
  candidate_names <- names(fit)[grepl(
    "reconstruct|loading|weight|score|coef|wbX|wbY|sbX|sbY|Xscale|Yscale|preprocess",
    names(fit),
    ignore.case = TRUE
  )]
  list(
    fit_name = fit_name,
    class = class(fit),
    dmode = fit$dmode,
    candidate_names = candidate_names,
    candidate_structure = lapply(candidate_names, function(nm) {
      value <- fit[[nm]]
      list(class = class(value), length = length(value), dim = dim(value), names = names(value))
    })
  )
}

fit_inventory <- list(
  PCA = summarize_fit(fit_pca, "PCA"),
  PLS = summarize_fit(fit_pls, "PLS")
)
names(fit_inventory$PCA$candidate_structure) <- fit_inventory$PCA$candidate_names
names(fit_inventory$PLS$candidate_structure) <- fit_inventory$PLS$candidate_names
saveRDS(fit_inventory, file.path(out_dir, "m3d-fit-inventory.rds"))

capture.output(
  {
    cat("PCA fit reconstruction-related fields\n")
    print(fit_inventory$PCA$candidate_names)
    for (nm in fit_inventory$PCA$candidate_names) {
      cat("\n$", nm, "\n", sep = "")
      str(fit_pca[[nm]], max.level = 2)
    }
    cat("\n\nPLS fit reconstruction-related fields\n")
    print(fit_inventory$PLS$candidate_names)
    for (nm in fit_inventory$PLS$candidate_names) {
      cat("\n$", nm, "\n", sep = "")
      str(fit_pls[[nm]], max.level = 2)
    }
  },
  file = file.path(out_dir, "M3-D-FIT-FIELD-INVENTORY.txt")
)

# Determine whether training-derived fixed coefficients can reproduce training
# reconstruction for each block. This is an audit, not a new public contract.
audit_fixed_coefficients <- function(fit, side = c("X", "Y")) {
  side <- match.arg(side)
  if (side == "Y" && identical(fit$dmode, "PCA")) return(NULL)

  score_name <- if (side == "X") "sbX" else "sbY"
  raw_score <- fit[[score_name]]
  if (is.null(raw_score)) return(list(error = paste0(score_name, " is absent")))

  score_blocks <- if (is.list(raw_score)) raw_score else list(raw_score)
  original_blocks <- if (side == "X") {
    .invert_msma_preprocessor(fit$preprocess, fit$X, side = "X")
  } else {
    .invert_msma_preprocessor(fit$preprocess, fit$Y, side = "Y")
  }
  processed_blocks <- if (side == "X") fit$X else fit$Y
  processed_blocks <- if (is.list(processed_blocks)) processed_blocks else list(processed_blocks)

  lapply(seq_along(score_blocks), function(i) {
    scores <- score_blocks[[i]]
    processed <- processed_blocks[[i]]
    coefficient <- project(processed, scores)
    reconstruction <- cbind(scores) %*% coefficient
    list(
      coefficient = coefficient,
      coefficient_dim = dim(coefficient),
      reconstruction_dim = dim(reconstruction),
      finite = all(is.finite(coefficient)) && all(is.finite(reconstruction)),
      processed_sum_squares = sum(processed^2),
      residual_sum_squares = sum((processed - reconstruction)^2),
      original_dim = dim(original_blocks[[i]])
    )
  })
}

fixed_audit <- list(
  PCA_X = audit_fixed_coefficients(fit_pca, "X"),
  PLS_X = audit_fixed_coefficients(fit_pls, "X"),
  PLS_Y = audit_fixed_coefficients(fit_pls, "Y")
)
saveRDS(fixed_audit, file.path(out_dir, "m3d-fixed-coefficient-audit.rds"))

capture.output(
  {
    cat("Training-derived fixed reconstruction coefficient audit\n\n")
    str(fixed_audit, max.level = 4)
  },
  file = file.path(out_dir, "M3-D-FIXED-COEFFICIENT-AUDIT.txt")
)

# Existing tests that mention reconstruction or prediction coefficients --------
test_files <- list.files("tests/testthat", pattern = "[.][Rr]$", full.names = TRUE)
test_rows <- do.call(rbind, lapply(test_files, function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  idx <- grep("project\\(|reconstruction_loadings|predict\\(|sbX|sbY", lines)
  if (!length(idx)) return(NULL)
  data.frame(file = path, line = idx, text = lines[idx], stringsAsFactors = FALSE)
}))
if (is.null(test_rows)) {
  test_rows <- data.frame(file = character(), line = integer(), text = character())
}
write.csv(
  test_rows,
  file.path(out_dir, "m3d-reconstruction-test-inventory.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("M3-D audit files created in: ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
cat("No source implementation was changed.\n")
