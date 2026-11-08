# Integrate the M2-D preprocessor into s4pca_reconstruct().
# Run from the msma Version 5 repository root.

stopifnot(file.exists("src.r"))

src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

if (any(grepl("preprocess = object\\$preprocess", src))) {
  stop("M2-E reconstruction integration appears to be already present.", call. = FALSE)
}

backup_dir <- file.path("development", "v5-reconstruction")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
backup_file <- file.path(backup_dir, "src-before-m2e-reconstruction-integration.r")
file.copy("src.r", backup_file, overwrite = TRUE)

sweep_line <- grep(
  "X\\s*(<-|=)\\s*sweep\\(sweep\\(X,\\s*2,\\s*object\\$Xcenter",
  src
)

if (length(sweep_line) != 1L) {
  stop("Could not uniquely locate the existing s4pca_reconstruct preprocessing line.", call. = FALSE)
}

if_line <- sweep_line - 1L
end_line <- sweep_line + 1L

if (!grepl("if\\s*\\(isTRUE\\(object\\$scaling\\)\\)", src[if_line])) {
  stop("The line before the preprocessing sweep is not the expected scaling condition.", call. = FALSE)
}

if (!grepl("^\\s*}\\s*$", src[end_line])) {
  stop("The line after the preprocessing sweep is not the expected closing brace.", call. = FALSE)
}

indent <- sub("^(\\s*).*", "\\1", src[if_line])
replacement <- c(
  paste0(indent, "if (!is.null(object$preprocess) &&"),
  paste0(indent, "    inherits(object$preprocess, \"msma_preprocess\")) {"),
  paste0(indent, "  X <- .apply_msma_preprocessor("),
  paste0(indent, "    preprocess = object$preprocess,"),
  paste0(indent, "    newdata = X,"),
  paste0(indent, "    side = \"X\""),
  paste0(indent, "  )"),
  paste0(indent, "} else if (isTRUE(object$scaling)) {"),
  paste0(indent, "  X <- sweep(sweep(X, 2, object$Xcenter, \"-\"),"),
  paste0(indent, "    2, object$Xscale, \"/\")"),
  paste0(indent, "}")
)

src_updated <- c(
  if (if_line > 1L) src[seq_len(if_line - 1L)],
  replacement,
  if (end_line < length(src)) src[seq.int(end_line + 1L, length(src))]
)

writeLines(src_updated, "src.r", useBytes = TRUE)

syntax <- try(parse(file = "src.r"), silent = TRUE)
if (inherits(syntax, "try-error")) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop(as.character(syntax), call. = FALSE)
}

check <- readLines("src.r", warn = FALSE, encoding = "UTF-8")
stopifnot(
  sum(grepl("preprocess = object\\$preprocess", check)) == 1L,
  sum(grepl("inherits\\(object\\$preprocess, \\\"msma_preprocess\\\"\\)", check)) == 1L
)

cat("M2-E reconstruction preprocessing integration completed.\n")
cat("Backup: ", normalizePath(backup_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
