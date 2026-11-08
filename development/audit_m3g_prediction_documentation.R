# M3-G-A: Audit prediction documentation before public documentation changes.
# Run from the msma Version 5 repository root.

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))
source("src.r", local = globalenv())

out_dir <- file.path("development", "v5-prediction")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_if_exists <- function(path) {
  if (!file.exists(path)) return(character())

  lines <- tryCatch(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e) character()
  )

  if (!length(lines)) return(character())

  converted <- iconv(
    lines,
    from = "",
    to = "UTF-8",
    sub = "byte"
  )

  converted[is.na(converted)] <- ""
  converted
}

all_files <- list.files(
  ".",
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE
)
all_files <- all_files[!grepl("(^|/)[.]git(/|$)", gsub("\\\\", "/", all_files))]

# Restrict the documentation inventory to text-based files.
text_extensions <- c(
  "R", "r", "Rd", "rd",
  "Rmd", "rmd", "qmd", "md",
  "txt", "csv", "yml", "yaml"
)

normalized_files <- gsub(
  "\\\\",
  "/",
  all_files
)

candidate_files <- all_files[
  grepl(
    "(^|/)(R|man|vignettes|development)(/|$)|NEWS|README|cran-comments|_pkgdown|DESCRIPTION|NAMESPACE",
    normalized_files,
    ignore.case = TRUE
  )
]

candidate_names <- basename(candidate_files)
candidate_extensions <- tools::file_ext(candidate_files)

extension_allowed <-
  candidate_extensions %in% text_extensions |
  candidate_names %in% c(
    "NEWS",
    "DESCRIPTION",
    "NAMESPACE",
    "LICENSE"
  )

candidate_files <- candidate_files[
  extension_allowed
]

patterns <- c(
  "predict.msma",
  "preprocessing",
  "reconstruction",
  "output_scale",
  "prediction_reconstruction",
  "sbX",
  "sbY",
  "newY",
  "low-rank",
  "low rank"
)

inventory <- do.call(rbind, lapply(candidate_files, function(path) {
  lines <- read_if_exists(path)
  if (!length(lines)) return(NULL)
  hits <- which(vapply(lines, function(line) {
    line_lower <- tolower(line)

    any(
      vapply(
        tolower(patterns),
        grepl,
        logical(1),
        x = line_lower,
        fixed = TRUE
      )
    )
  }, logical(1)))
  if (!length(hits)) return(NULL)
  data.frame(
    file = gsub("\\\\", "/", path),
    line = hits,
    text = lines[hits],
    stringsAsFactors = FALSE
  )
}))
if (is.null(inventory)) {
  inventory <- data.frame(file = character(), line = integer(), text = character())
}
write.csv(
  inventory,
  file.path(out_dir, "m3g-documentation-inventory.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Identify likely public documentation entry points.
entry_paths <- c(
  "src.r",
  "NEWS.md",
  "NEWS",
  "README.md",
  "DESCRIPTION",
  "NAMESPACE",
  "man/predict.msma.Rd",
  "vignettes"
)
entry_status <- data.frame(
  path = entry_paths,
  exists = file.exists(entry_paths) | dir.exists(entry_paths),
  type = ifelse(dir.exists(entry_paths), "directory", "file"),
  stringsAsFactors = FALSE
)
write.csv(
  entry_status,
  file.path(out_dir, "m3g-documentation-entry-status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# Save the current public function contract and helper metadata contract.
contract <- list(
  predict_formals = formals(predict.msma),
  fitted_formals = formals(.predict_msma_fitted),
  prediction_modes = list(
    preprocessing = c("legacy", "fitted"),
    reconstruction = c("batch", "fitted"),
    output_scale = c("analysis", "original")
  ),
  return_names = c("X", "sbX", "Y", "sbY"),
  notes = c(
    "predict.msma does not predict Y from X",
    "X and Y are scored and reconstructed independently",
    "sbX and sbY remain on the analysis scale",
    "output_scale controls reconstructed X and Y only",
    "legacy behavior remains the default"
  )
)
saveRDS(contract, file.path(out_dir, "m3g-prediction-documentation-contract.rds"))

# Capture existing relevant source documentation context.
src <- read_if_exists("src.r")
hits <- grep(
  "predict[.]msma|Prediction|preprocessing|reconstruction|output_scale",
  src,
  ignore.case = TRUE
)
context <- sort(unique(unlist(lapply(hits, function(i) {
  seq.int(max(1L, i - 8L), min(length(src), i + 15L))
}))))
writeLines(
  sprintf("%5d | %s", context, src[context]),
  file.path(out_dir, "M3-G-SOURCE-DOCUMENTATION-CONTEXT.txt"),
  useBytes = TRUE
)

# Find vignettes and Rd files for planned edits.
vignette_files <- list.files(
  "vignettes",
  pattern = "[.](Rmd|rmd|qmd|md)$",
  full.names = TRUE,
  recursive = TRUE
)
rd_files <- list.files(
  "man",
  pattern = "[.]Rd$",
  full.names = TRUE,
  recursive = TRUE
)

summary_lines <- c(
  "msma Version 5 M3-G-A prediction documentation audit",
  paste0("Date: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
  paste0("Branch: ", system2("git", c("branch", "--show-current"), stdout = TRUE)),
  paste0("Commit: ", system2("git", c("rev-parse", "HEAD"), stdout = TRUE)),
  "",
  "Current public prediction contract:",
  "* default preprocessing: legacy",
  "* fitted preprocessing: preprocessing = fitted",
  "* default fitted reconstruction: reconstruction = batch",
  "* fixed fitted reconstruction: reconstruction = fitted",
  "* default output scale: output_scale = analysis",
  "* original-variable output: output_scale = original",
  "* return names: X, sbX, Y, sbY",
  "* predict.msma does not predict Y from X",
  "* X and Y are scored and reconstructed independently",
  "* sbX and sbY remain on the analysis scale",
  "",
  "Documentation entry points:",
  paste0("* ", entry_status$path, ": ", entry_status$exists),
  "",
  paste0("Rd files found: ", length(rd_files)),
  paste0("Vignette files found: ", length(vignette_files)),
  paste0("Documentation inventory hits: ", nrow(inventory)),
  "",
  "Required M3-G deliverables:",
  "* update predict.msma user documentation",
  "* document preprocessing, reconstruction, and output_scale",
  "* explain the X, sbX, Y, sbY return contract",
  "* state that predict.msma does not predict Y from X",
  "* add PCA and PLS examples",
  "* add multiblock example if supported by existing vignette structure",
  "* add Version 5 migration guidance",
  "* update NEWS",
  "* add documentation regression tests",
  "* run full tests and package check",
  "",
  "M3-G-A status: DOCUMENTATION PATHS IDENTIFIED; SOURCE UNCHANGED"
)
writeLines(
  summary_lines,
  file.path(out_dir, "M3-G-DOCUMENTATION-AUDIT.txt"),
  useBytes = TRUE
)

cat("M3-G documentation audit created in: ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
cat("Source implementation was not changed.\n")
