# msma Version 5 Milestone 2: preprocessing audit
# Run from the Ver5 repository root. This script does not modify src.r.

stopifnot(file.exists("src.r"), file.exists("DESCRIPTION"))

out_dir <- file.path("development", "v5-preprocessing")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

find_context <- function(pattern, label, before = 5L, after = 10L,
                         fixed = FALSE) {
  hits <- grep(pattern, src, fixed = fixed)
  if (!length(hits)) {
    return(c(paste0("## ", label), "", "No matches found.", ""))
  }
  idx <- sort(unique(unlist(lapply(hits, function(i) {
    seq.int(max(1L, i - before), min(length(src), i + after))
  }))))
  c(
    paste0("## ", label),
    "",
    paste0("Matches: ", length(hits)),
    "",
    "```r",
    sprintf("%5d | %s", idx, src[idx]),
    "```",
    ""
  )
}

patterns <- list(
  scaling_calls = list(pattern = "scale\\(", label = "Scaling calls"),
  center_storage = list(pattern = "center|Xcenter|Ycenter|Zcenter", label = "Centering metadata"),
  scale_storage = list(pattern = "Xscale|Yscale|Zscale|scaled:scale", label = "Scaling metadata"),
  sweep_calls = list(pattern = "sweep\\(", label = "Manual centering and scaling"),
  missing_values = list(pattern = "is.na|NA|missing", label = "Missing-value handling"),
  zero_variance = list(pattern = "==0|== 0|is.finite", label = "Zero-variance and non-finite handling"),
  reconstruction = list(pattern = "reconstruct|newdata|reconstruction", label = "Reconstruction and new-data paths"),
  legacy_engine = list(pattern = ".msma_default_legacy", label = "Legacy engine preprocessing context", fixed = TRUE),
  extended_engine = list(pattern = ".msma_default_extended", label = "Extended engine preprocessing context", fixed = TRUE),
  structured_engine = list(pattern = ".s4pca_fit", label = "Structured engine preprocessing context", fixed = TRUE)
)

report <- c(
  "# msma Version 5 preprocessing audit",
  "",
  paste0("- Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Package version: ", read.dcf("DESCRIPTION", fields = "Version")[1, 1]),
  paste0("- Branch: ", system2("git", c("branch", "--show-current"), stdout = TRUE)),
  paste0("- Commit: ", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
  "",
  "## Objective",
  "",
  "Document preprocessing behavior in the legacy, extended, soft, and exclusive paths before refactoring.",
  "This audit does not change numerical behavior.",
  ""
)

for (x in patterns) {
  report <- c(
    report,
    find_context(
      pattern = x$pattern,
      label = x$label,
      fixed = isTRUE(x$fixed)
    )
  )
}

report <- c(
  report,
  "## Required invariants",
  "",
  "1. Version 4 results remain numerically unchanged.",
  "2. Legacy X, Y, and Z preprocessing remains unchanged.",
  "3. Structured X preprocessing and reconstruction metadata remain unchanged.",
  "4. Extended NMF, sNMF, CMP, multivariate-Z, and con4spv behavior remains unchanged.",
  "5. NA handling remains path-specific until explicitly redesigned.",
  "6. Existing public fields such as Xcenter, Xscale, and legacy scale attributes remain available.",
  "7. New preprocessing metadata is additive and internal during the first refactor.",
  "",
  "## Proposed first-stage design",
  "",
  "Introduce metadata helpers before moving numerical transformations:",
  "",
  "```r",
  ".new_msma_preprocess <- function(engine, scaling) {",
  "  list(",
  "    engine = engine,",
  "    scaling = scaling,",
  "    X = NULL,",
  "    Y = NULL,",
  "    Z = NULL,",
  "    missing = NULL",
  "  )",
  "}",
  "```",
  "",
  "Do not initially replace existing scale() or sweep() calls. First attach equivalent metadata and verify all regression tests."
)

report_file <- file.path(out_dir, "PREPROCESSING-AUDIT.md")
writeLines(report, report_file, useBytes = TRUE)

summary_df <- do.call(rbind, lapply(names(patterns), function(nm) {
  x <- patterns[[nm]]
  data.frame(
    category = nm,
    matches = length(grep(x$pattern, src, fixed = isTRUE(x$fixed))),
    stringsAsFactors = FALSE
  )
}))

summary_file <- file.path(out_dir, "preprocessing-match-summary.csv")
write.csv(summary_df, summary_file, row.names = FALSE)

cat("Preprocessing audit completed.\n")
cat("Report: ", normalizePath(report_file, winslash = "/"), "\n", sep = "")
cat("Summary: ", normalizePath(summary_file, winslash = "/"), "\n", sep = "")
print(summary_df)
