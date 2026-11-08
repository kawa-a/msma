# M3-C: Add an explicit fitted-preprocessing mode to predict.msma().
# The default legacy mode remains numerically unchanged.
# Run from the msma Version 5 repository root.

stopifnot(file.exists("src.r"))

src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

if (any(grepl("^\\.predict_msma_legacy\\s*(<-|=)\\s*function", src))) {
  stop("M3-C prediction helpers already appear to be present.", call. = FALSE)
}

backup_dir <- file.path("development", "v5-prediction")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
backup_file <- file.path(backup_dir, "src-before-m3c-fitted-prediction.r")
file.copy("src.r", backup_file, overwrite = TRUE)

start <- grep("^predict\\.msma\\s*(<-|=)\\s*function", src)
if (length(start) != 1L) {
  stop("Could not uniquely locate predict.msma().", call. = FALSE)
}

# Find the closing brace of predict.msma() by parsing cumulative brace depth.
brace_delta <- function(line) {
  chars <- strsplit(line, "", fixed = TRUE)[[1]]
  sum(chars == "{") - sum(chars == "}")
}

depth <- 0L
end <- NA_integer_
for (i in seq.int(start, length(src))) {
  depth <- depth + brace_delta(src[[i]])
  if (i > start && depth == 0L) {
    end <- i
    break
  }
}
if (is.na(end)) stop("Could not locate the end of predict.msma().", call. = FALSE)

replacement <- c(
  ".predict_msma_legacy <- function(object, newX, newY = NULL) {",
  "  if (missing(newX)) stop(\"newX should be specified\")",
  "  if (is.null(newY) | (object$dmode == \"PCA\")) newY <- newX",
  "  if (!inherits(newX, \"list\")) newX <- list(newX)",
  "  if (!inherits(newY, \"list\")) newY <- list(newY)",
  "  if (!inherits(newX, \"list\")) stop(\"newX should be a list\")",
  "  if (!inherits(newY, \"list\")) stop(\"newY should be a list\")",
  "  Ynb <- length(newY)",
  "  Xnb <- length(newX)",
  "  newX <- lapply(seq_along(newX), function(x) {",
  "    scale(newX[[x]], center = object$Xscale[[x]]$`scaled:center`)",
  "  })",
  "  newX <- lapply(newX, function(x) apply(x, 2, function(x2) {",
  "    x2[is.na(x2)] <- 0",
  "    x2",
  "  }))",
  "  sbX <- lapply(seq_len(Xnb), function(x) newX[[x]] %*% object$wbX[[x]])",
  "  predX <- lapply(seq_len(Xnb), function(x) {",
  "    cbind(sbX[[x]]) %*% project(newX[[x]], sbX[[x]])",
  "  })",
  "  if (!(is.null(newY) | object$dmode == \"PCA\")) {",
  "    newY <- lapply(seq_along(newY), function(x) {",
  "      scale(newY[[x]], center = object$Yscale[[x]]$`scaled:center`)",
  "    })",
  "    newY <- lapply(newY, function(y) apply(y, 2, function(y2) {",
  "      y2[is.na(y2)] <- 0",
  "      y2",
  "    }))",
  "    sbY <- lapply(seq_len(Ynb), function(x) newY[[x]] %*% object$wbY[[x]])",
  "    predY <- lapply(seq_len(Ynb), function(x) {",
  "      cbind(sbY[[x]]) %*% project(newY[[x]], sbY[[x]])",
  "    })",
  "  } else {",
  "    predY <- sbY <- NULL",
  "  }",
  "  list(X = predX, sbX = sbX, Y = predY, sbY = sbY)",
  "}",
  "",
  ".predict_msma_replace_missing <- function(blocks) {",
  "  lapply(blocks, function(block) {",
  "    block[is.na(block)] <- 0",
  "    block",
  "  })",
  "}",
  "",
  ".predict_msma_fitted <- function(object, newX, newY = NULL) {",
  "  if (missing(newX)) stop(\"newX should be specified\", call. = FALSE)",
  "  if (is.null(object$preprocess) ||",
  "      !inherits(object$preprocess, \"msma_preprocess\")) {",
  "    stop(",
  "      \"Fitted preprocessing requires a Version 5 fit with `preprocess` metadata.\",",
  "      call. = FALSE",
  "    )",
  "  }",
  "  if (identical(object$dmode, \"PLS\") && is.null(newY)) {",
  "    stop(\"`newY` must be supplied for fitted PLS prediction.\", call. = FALSE)",
  "  }",
  "",
  "  input_x_was_list <- is.list(newX)",
  "  x_blocks <- if (input_x_was_list) newX else list(newX)",
  "  x_blocks <- .apply_msma_preprocessor(",
  "    preprocess = object$preprocess,",
  "    newdata = x_blocks,",
  "    side = \"X\"",
  "  )",
  "  x_blocks <- .predict_msma_replace_missing(x_blocks)",
  "  sbX <- lapply(seq_along(x_blocks), function(i) {",
  "    x_blocks[[i]] %*% object$wbX[[i]]",
  "  })",
  "  predX <- lapply(seq_along(x_blocks), function(i) {",
  "    cbind(sbX[[i]]) %*% project(x_blocks[[i]], sbX[[i]])",
  "  })",
  "  names(sbX) <- names(x_blocks)",
  "  names(predX) <- names(x_blocks)",
  "",
  "  if (identical(object$dmode, \"PCA\")) {",
  "    predY <- sbY <- NULL",
  "  } else {",
  "    input_y_was_list <- is.list(newY)",
  "    y_blocks <- if (input_y_was_list) newY else list(newY)",
  "    y_blocks <- .apply_msma_preprocessor(",
  "      preprocess = object$preprocess,",
  "      newdata = y_blocks,",
  "      side = \"Y\"",
  "    )",
  "    y_blocks <- .predict_msma_replace_missing(y_blocks)",
  "    sbY <- lapply(seq_along(y_blocks), function(i) {",
  "      y_blocks[[i]] %*% object$wbY[[i]]",
  "    })",
  "    predY <- lapply(seq_along(y_blocks), function(i) {",
  "      cbind(sbY[[i]]) %*% project(y_blocks[[i]], sbY[[i]])",
  "    })",
  "    names(sbY) <- names(y_blocks)",
  "    names(predY) <- names(y_blocks)",
  "  }",
  "",
  "  list(X = predX, sbX = sbX, Y = predY, sbY = sbY)",
  "}",
  "",
  "predict.msma <- function(object, newX, newY = NULL, ...) {",
  "  dots <- list(...)",
  "  preprocessing <- if (is.null(dots$preprocessing)) {",
  "    \"legacy\"",
  "  } else {",
  "    match.arg(dots$preprocessing, c(\"legacy\", \"fitted\"))",
  "  }",
  "  if (identical(preprocessing, \"legacy\")) {",
  "    return(.predict_msma_legacy(object = object, newX = newX, newY = newY))",
  "  }",
  "  .predict_msma_fitted(object = object, newX = newX, newY = newY)",
  "}"
)

updated <- c(
  if (start > 1L) src[seq_len(start - 1L)],
  replacement,
  if (end < length(src)) src[seq.int(end + 1L, length(src))]
)
writeLines(updated, "src.r", useBytes = TRUE)

syntax <- try(parse(file = "src.r"), silent = TRUE)
if (inherits(syntax, "try-error")) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop(as.character(syntax), call. = FALSE)
}

check <- readLines("src.r", warn = FALSE, encoding = "UTF-8")
stopifnot(
  sum(grepl("^\\.predict_msma_legacy\\s*(<-|=)\\s*function", check)) == 1L,
  sum(grepl("^\\.predict_msma_fitted\\s*(<-|=)\\s*function", check)) == 1L,
  sum(grepl("^predict\\.msma\\s*(<-|=)\\s*function", check)) == 1L
)

cat("M3-C fitted prediction mode integrated.\n")
cat("Backup: ", normalizePath(backup_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
