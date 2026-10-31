# msma Version 5 Milestone 1: dispatcher audit
# Run from the Ver5 repository root before changing src.r.

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))

out_dir <- file.path("development", "v5-dispatcher")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

find_lines <- function(pattern, fixed = FALSE) {
  hit <- grep(pattern, src, fixed = fixed)
  if (!length(hit)) return(data.frame(line = integer(), text = character()))
  data.frame(line = hit, text = src[hit], row.names = NULL)
}

sections <- list(
  msma_definitions = find_lines("^msma\\s*<-\\s*function|^msma\\s*=\\s*function|^msma\\.default\\s*<-\\s*function"),
  structure_method = find_lines("structure.method", fixed = TRUE),
  gamma_x = find_lines("gammaX", fixed = TRUE),
  history_method = find_lines("history.method", fixed = TRUE),
  exclusive = find_lines("exclusive", fixed = TRUE),
  soft = find_lines("soft", fixed = TRUE),
  unsupported = find_lines("not supported|unsupported|stop\\(", fixed = FALSE),
  scaling = find_lines("scaling", fixed = TRUE),
  s4_iterations = find_lines("niterS4", fixed = TRUE)
)

capture_context <- function(lines, before = 4L, after = 8L) {
  if (!length(lines)) return(character())
  idx <- sort(unique(unlist(lapply(lines, function(i) seq.int(max(1L, i-before), min(length(src), i+after))))))
  sprintf("%5d | %s", idx, src[idx])
}

report <- c(
  "# msma Version 5 dispatcher audit",
  "",
  paste0("- Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("- Source version: ", read.dcf("DESCRIPTION", fields = "Version")[1, 1]),
  paste0("- Branch: ", system2("git", c("branch", "--show-current"), stdout = TRUE)),
  paste0("- Commit: ", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
  "",
  "## Audit objective",
  "",
  "Identify the current dispatch rules for legacy, soft-structured, and exclusive PCA before refactoring.",
  "No numerical implementation is changed by this audit.",
  ""
)

for (nm in names(sections)) {
  tab <- sections[[nm]]
  report <- c(report, paste0("## ", gsub("_", " ", nm)), "")
  if (!nrow(tab)) {
    report <- c(report, "No matches found.", "")
  } else {
    report <- c(report, "```r", capture_context(tab$line), "```", "")
  }
}

report <- c(
  report,
  "## Required invariants for the refactor",
  "",
  "1. Calls without structured arguments remain on the Version 4 legacy path.",
  "2. `structure.method = \"none\"` and `gammaX = 0` preserve Version 4 results.",
  "3. Positive `gammaX` retains the Version 4 implicit soft-structure opt-in behavior.",
  "4. `structure.method = \"soft\"` uses the validated S4PCA engine.",
  "5. `structure.method = \"exclusive\"` preserves hard exclusion.",
  "6. Unsupported combinations fail before estimation with the same error class and message where tested.",
  "7. Public result names and values remain unchanged.",
  "8. Fixed-seed results remain reproducible.",
  "",
  "## Proposed dispatcher contract",
  "",
  "```r",
  ".resolve_msma_engine <- function(structure.method, gammaX, ...) {",
  "  # Return one of: legacy, soft, exclusive.",
  "  # This resolver must contain policy only and perform no numerical estimation.",
  "}",
  "",
  "msma <- function(...) {",
  "  engine <- .resolve_msma_engine(...) ",
  "  switch(engine,",
  "    legacy = .msma_legacy(...),",
  "    soft = .msma_soft(...),",
  "    exclusive = .msma_exclusive(...) ",
  "  )",
  "}",
  "```",
  "",
  "The first refactor should extract policy without changing the existing estimation bodies."
)

report_file <- file.path(out_dir, "DISPATCHER-AUDIT.md")
writeLines(report, report_file, useBytes = TRUE)

summary_file <- file.path(out_dir, "dispatcher-match-summary.csv")
summary_df <- do.call(rbind, lapply(names(sections), function(nm) {
  data.frame(category = nm, matches = nrow(sections[[nm]]))
}))
write.csv(summary_df, summary_file, row.names = FALSE)

cat("Dispatcher audit completed.\n")
cat("Report: ", normalizePath(report_file, winslash = "/"), "\n", sep = "")
cat("Summary: ", normalizePath(summary_file, winslash = "/"), "\n", sep = "")
print(summary_df)
