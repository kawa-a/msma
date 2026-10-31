# Add M2-D validation and application helpers to src.r.
# Run from the msma Ver5 repository root.

stopifnot(file.exists("src.r"))

src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

function_patterns <- c(
  validator = "^\\.validate_msma_preprocessor\\s*(<-|=)\\s*function",
  applicator = "^\\.apply_msma_preprocessor\\s*(<-|=)\\s*function"
)

existing <- vapply(function_patterns, function(pattern) {
  any(grepl(pattern, src))
}, logical(1))

if (any(existing)) {
  stop(
    paste0(
      "M2-D function already present: ",
      paste(names(existing)[existing], collapse = ", ")
    ),
    call. = FALSE
  )
}

backup_dir <- file.path("development", "v5-preprocessing")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
backup_file <- file.path(backup_dir, "src-before-m2d-preprocessor.r")
file.copy("src.r", backup_file, overwrite = TRUE)

helper_code <- c(
  "",
  "# Validate preprocessing metadata and new data before transformation.",
  ".validate_msma_preprocessor <- function(",
  "  preprocess,",
  "  newdata,",
  "  side = c(\"X\", \"Y\", \"Z\")",
  ") {",
  "  side <- match.arg(side)",
  "",
  "  if (!inherits(preprocess, \"msma_preprocess\")) {",
  "    stop(\"`preprocess` must inherit from \\\"msma_preprocess\\\".\", call. = FALSE)",
  "  }",
  "",
  "  side_metadata <- preprocess[[side]]",
  "  if (is.null(side_metadata)) {",
  "    stop(paste0(\"No preprocessing metadata are available for side \", side, \".\"), call. = FALSE)",
  "  }",
  "",
  "  if (identical(side, \"Z\")) return(invisible(TRUE))",
  "",
  "  blocks <- if (is.list(newdata)) newdata else list(newdata)",
  "  if (length(blocks) == 0L) stop(\"`newdata` must contain at least one data block.\", call. = FALSE)",
  "",
  "  valid_blocks <- vapply(blocks, function(block) is.matrix(block) && is.numeric(block), logical(1))",
  "  if (!all(valid_blocks)) {",
  "    stop(\"`newdata` must be a numeric matrix or a list of numeric matrices.\", call. = FALSE)",
  "  }",
  "",
  "  centers <- side_metadata$center",
  "  scales <- side_metadata$scale",
  "  if (is.null(centers) && is.null(scales)) return(invisible(TRUE))",
  "",
  "  centers <- if (is.list(centers)) centers else list(centers)",
  "  scales <- if (is.list(scales)) scales else list(scales)",
  "  expected_blocks <- max(length(centers), length(scales))",
  "",
  "  if (length(blocks) != expected_blocks) {",
  "    stop(paste0(\"`newdata` has \", length(blocks), \" block(s), but preprocessing metadata require \", expected_blocks, \" block(s).\"), call. = FALSE)",
  "  }",
  "",
  "  for (block_index in seq_along(blocks)) {",
  "    block <- blocks[[block_index]]",
  "    center <- centers[[block_index]]",
  "    scale <- scales[[block_index]]",
  "",
  "    expected_columns <- if (!is.null(center)) length(center) else if (!is.null(scale)) length(scale) else ncol(block)",
  "    if (ncol(block) != expected_columns) {",
  "      stop(paste0(\"Block \", block_index, \" has \", ncol(block), \" column(s), but preprocessing metadata require \", expected_columns, \".\"), call. = FALSE)",
  "    }",
  "",
  "    expected_names <- if (!is.null(center)) names(center) else if (!is.null(scale)) names(scale) else NULL",
  "    observed_names <- colnames(block)",
  "    if (!is.null(expected_names) && !is.null(observed_names) && !identical(observed_names, expected_names)) {",
  "      stop(paste0(\"Column names or column order do not match the fitted preprocessing metadata for block \", block_index, \".\"), call. = FALSE)",
  "    }",
  "  }",
  "",
  "  invisible(TRUE)",
  "}",
  "",
  "# Apply fitted preprocessing metadata to new data.",
  ".apply_msma_preprocessor <- function(",
  "  preprocess,",
  "  newdata,",
  "  side = c(\"X\", \"Y\", \"Z\")",
  ") {",
  "  side <- match.arg(side)",
  "  .validate_msma_preprocessor(preprocess = preprocess, newdata = newdata, side = side)",
  "",
  "  if (identical(side, \"Z\")) return(newdata)",
  "",
  "  side_metadata <- preprocess[[side]]",
  "  input_was_list <- is.list(newdata)",
  "  blocks <- if (input_was_list) newdata else list(newdata)",
  "",
  "  centers <- side_metadata$center",
  "  scales <- side_metadata$scale",
  "  centers <- if (is.null(centers)) rep(list(NULL), length(blocks)) else if (is.list(centers)) centers else list(centers)",
  "  scales <- if (is.null(scales)) rep(list(NULL), length(blocks)) else if (is.list(scales)) scales else list(scales)",
  "",
  "  transformed <- lapply(seq_along(blocks), function(block_index) {",
  "    result <- blocks[[block_index]]",
  "    center <- centers[[block_index]]",
  "    scale <- scales[[block_index]]",
  "",
  "    if (!is.null(center)) result <- sweep(result, 2, center, \"-\")",
  "    if (!is.null(scale)) {",
  "      safe_scale <- scale",
  "      safe_scale[!is.finite(safe_scale) | safe_scale == 0] <- 1",
  "      result <- sweep(result, 2, safe_scale, \"/\")",
  "    }",
  "    result",
  "  })",
  "",
  "  if (input_was_list) transformed else transformed[[1]]",
  "}",
  ""
)

insert_at <- grep("^\\.resolve_msma_engine\\s*(<-|=)\\s*function", src)
stopifnot(length(insert_at) == 1L)

src_updated <- c(
  src[seq_len(insert_at - 1L)],
  helper_code,
  src[insert_at:length(src)]
)

writeLines(src_updated, "src.r", useBytes = TRUE)

syntax <- try(parse(file = "src.r"), silent = TRUE)
if (inherits(syntax, "try-error")) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop(as.character(syntax), call. = FALSE)
}

src_check <- readLines("src.r", warn = FALSE, encoding = "UTF-8")
counts <- c(
  validator = sum(grepl(function_patterns[["validator"]], src_check)),
  applicator = sum(grepl(function_patterns[["applicator"]], src_check))
)
stopifnot(identical(unname(counts), c(1L, 1L)))

cat("M2-D helpers added to src.r.\n")
cat("Backup: ", normalizePath(backup_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
print(counts)
