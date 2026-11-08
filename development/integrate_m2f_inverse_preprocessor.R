# Add the inverse preprocessor and integrate it into s4pca_reconstruct().
# Run from the msma Version 5 repository root.

stopifnot(file.exists("src.r"))
src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

if (any(grepl("^\\.invert_msma_preprocessor\\s*(<-|=)\\s*function", src))) {
  stop(".invert_msma_preprocessor() is already present.", call. = FALSE)
}
if (any(grepl("Xhat\\s*(<-|=)\\s*\\.invert_msma_preprocessor\\(", src))) {
  stop("M2-F reconstruction integration appears to be already present.", call. = FALSE)
}

backup_dir <- file.path("development", "v5-reconstruction")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
backup_file <- file.path(backup_dir, "src-before-m2f-inverse-integration.r")
file.copy("src.r", backup_file, overwrite = TRUE)

helper_code <- c(
  "",
  "# Invert fitted preprocessing metadata and return data to the original scale.",
  ".invert_msma_preprocessor <- function(",
  "  preprocess,",
  "  data,",
  "  side = c(\"X\", \"Y\", \"Z\")",
  ") {",
  "  side <- match.arg(side)",
  "  .validate_msma_preprocessor(",
  "    preprocess = preprocess,",
  "    newdata = data,",
  "    side = side",
  "  )",
  "",
  "  if (identical(side, \"Z\")) return(data)",
  "",
  "  side_metadata <- preprocess[[side]]",
  "  input_was_list <- is.list(data)",
  "  blocks <- if (input_was_list) data else list(data)",
  "",
  "  centers <- side_metadata$center",
  "  scales <- side_metadata$scale",
  "  centers <- if (is.null(centers)) rep(list(NULL), length(blocks)) else if (is.list(centers)) centers else list(centers)",
  "  scales <- if (is.null(scales)) rep(list(NULL), length(blocks)) else if (is.list(scales)) scales else list(scales)",
  "",
  "  inverted <- lapply(seq_along(blocks), function(block_index) {",
  "    result <- blocks[[block_index]]",
  "    center <- centers[[block_index]]",
  "    scale <- scales[[block_index]]",
  "",
  "    if (!is.null(scale)) {",
  "      safe_scale <- scale",
  "      safe_scale[!is.finite(safe_scale) | safe_scale == 0] <- 1",
  "      result <- sweep(result, 2, safe_scale, \"*\")",
  "    }",
  "    if (!is.null(center)) result <- sweep(result, 2, center, \"+\")",
  "    result",
  "  })",
  "",
  "  if (input_was_list) inverted else inverted[[1]]",
  "}",
  ""
)

insert_at <- grep("^\\.resolve_msma_engine\\s*(<-|=)\\s*function", src)
if (length(insert_at) != 1L) stop("Could not uniquely locate .resolve_msma_engine().", call. = FALSE)

src <- c(
  src[seq_len(insert_at - 1L)],
  helper_code,
  src[insert_at:length(src)]
)

inverse_line <- grep(
  "Xhat\\s*(<-|=)\\s*sweep\\(sweep\\(Xhat,\\s*2,\\s*object\\$Xscale",
  src
)
if (length(inverse_line) != 1L) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop("Could not uniquely locate the existing reconstruction inverse line.", call. = FALSE)
}

if_line <- inverse_line - 1L
end_line <- inverse_line + 1L
if (!grepl("if\\s*\\(!scaled\\s*&&\\s*isTRUE\\(object\\$scaling\\)\\)", src[if_line])) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop("Unexpected condition before the inverse reconstruction line.", call. = FALSE)
}
if (!grepl("^\\s*}\\s*$", src[end_line])) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop("Unexpected closing line after the inverse reconstruction line.", call. = FALSE)
}

indent <- sub("^(\\s*).*", "\\1", src[if_line])
replacement <- c(
  paste0(indent, "if (!scaled) {"),
  paste0(indent, "  if (!is.null(object$preprocess) &&"),
  paste0(indent, "      inherits(object$preprocess, \"msma_preprocess\")) {"),
  paste0(indent, "    Xhat <- .invert_msma_preprocessor("),
  paste0(indent, "      preprocess = object$preprocess,"),
  paste0(indent, "      data = Xhat,"),
  paste0(indent, "      side = \"X\""),
  paste0(indent, "    )"),
  paste0(indent, "  } else if (isTRUE(object$scaling)) {"),
  paste0(indent, "    Xhat <- sweep(sweep(Xhat, 2, object$Xscale, \"*\"),"),
  paste0(indent, "      2, object$Xcenter, \"+\")"),
  paste0(indent, "  }"),
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
  sum(grepl("^\\.invert_msma_preprocessor\\s*(<-|=)\\s*function", check)) == 1L,
  sum(grepl("Xhat\\s*(<-|=)\\s*\\.invert_msma_preprocessor\\(", check)) == 1L
)

cat("M2-F inverse preprocessing integration completed.\n")
cat("Backup: ", normalizePath(backup_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
