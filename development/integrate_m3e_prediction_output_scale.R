# M3-E: Add explicit prediction reconstruction output scale.
# Default remains analysis scale for backward compatibility.
# Run from the msma Version 5 repository root after M3-D.

stopifnot(file.exists("src.r"))
src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

if (any(grepl("output_scale = c[(]", src, fixed = FALSE))) {
  stop("M3-E output-scale support already appears to be present.", call. = FALSE)
}

backup_dir <- file.path("development", "v5-prediction")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
backup_file <- file.path(backup_dir, "src-before-m3e-output-scale.r")
file.copy("src.r", backup_file, overwrite = TRUE)

find_function_range <- function(lines, function_name) {
  escaped_name <- gsub(".", "\\.", function_name, fixed = TRUE)
  start <- grep(
    paste0("^", escaped_name, "\\s*(<-|=)\\s*function"),
    lines
  )
  if (length(start) != 1L) {
    stop("Could not uniquely locate ", function_name, "().", call. = FALSE)
  }

  brace_delta <- function(line) {
    chars <- strsplit(line, "", fixed = TRUE)[[1]]
    sum(chars == "{") - sum(chars == "}")
  }

  depth <- 0L
  seen_opening_brace <- FALSE
  end <- NA_integer_

  for (i in seq.int(start, length(lines))) {
    if (grepl("{", lines[[i]], fixed = TRUE)) seen_opening_brace <- TRUE
    depth <- depth + brace_delta(lines[[i]])
    if (seen_opening_brace && depth == 0L) {
      end <- i
      break
    }
  }

  if (is.na(end)) {
    stop("Could not locate the end of ", function_name, "().", call. = FALSE)
  }
  c(start = start, end = end)
}

fitted_range <- find_function_range(src, ".predict_msma_fitted")
fitted_replacement <- c(
  ".predict_msma_fitted <- function(",
  "  object,",
  "  newX,",
  "  newY = NULL,",
  "  reconstruction = c(\"batch\", \"fitted\"),",
  "  output_scale = c(\"analysis\", \"original\")",
  ") {",
  "  reconstruction <- match.arg(reconstruction)",
  "  output_scale <- match.arg(output_scale)",
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
  "  if (identical(reconstruction, \"fitted\") &&",
  "      (is.null(object$prediction_reconstruction) ||",
  "       !inherits(object$prediction_reconstruction, \"msma_prediction_reconstruction\"))) {",
  "    stop(",
  "      \"Fitted reconstruction requires Version 5 reconstruction metadata.\",",
  "      call. = FALSE",
  "    )",
  "  }",
  "",
  "  x_blocks <- if (is.list(newX)) newX else list(newX)",
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
  "    if (identical(reconstruction, \"fitted\")) {",
  "      sbX[[i]] %*% object$prediction_reconstruction$X[[i]]",
  "    } else {",
  "      cbind(sbX[[i]]) %*% project(x_blocks[[i]], sbX[[i]])",
  "    }",
  "  })",
  "  names(sbX) <- names(x_blocks)",
  "  names(predX) <- names(x_blocks)",
  "",
  "  if (identical(object$dmode, \"PCA\")) {",
  "    predY <- sbY <- NULL",
  "  } else {",
  "    y_blocks <- if (is.list(newY)) newY else list(newY)",
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
  "      if (identical(reconstruction, \"fitted\")) {",
  "        sbY[[i]] %*% object$prediction_reconstruction$Y[[i]]",
  "      } else {",
  "        cbind(sbY[[i]]) %*% project(y_blocks[[i]], sbY[[i]])",
  "      }",
  "    })",
  "    names(sbY) <- names(y_blocks)",
  "    names(predY) <- names(y_blocks)",
  "  }",
  "",
  "  if (identical(output_scale, \"original\")) {",
  "    predX <- .invert_msma_preprocessor(",
  "      preprocess = object$preprocess,",
  "      data = predX,",
  "      side = \"X\"",
  "    )",
  "    if (!is.null(predY)) {",
  "      predY <- .invert_msma_preprocessor(",
  "        preprocess = object$preprocess,",
  "        data = predY,",
  "        side = \"Y\"",
  "      )",
  "    }",
  "  }",
  "",
  "  list(X = predX, sbX = sbX, Y = predY, sbY = sbY)",
  "}"
)
src <- c(
  if (fitted_range[["start"]] > 1L) src[seq_len(fitted_range[["start"]] - 1L)],
  fitted_replacement,
  if (fitted_range[["end"]] < length(src)) src[seq.int(fitted_range[["end"]] + 1L, length(src))]
)

predict_range <- find_function_range(src, "predict.msma")
predict_replacement <- c(
  "predict.msma <- function(object, newX, newY = NULL, ...) {",
  "  dots <- list(...)",
  "  preprocessing <- if (is.null(dots$preprocessing)) {",
  "    \"legacy\"",
  "  } else {",
  "    match.arg(dots$preprocessing, c(\"legacy\", \"fitted\"))",
  "  }",
  "  reconstruction <- if (is.null(dots$reconstruction)) {",
  "    \"batch\"",
  "  } else {",
  "    match.arg(dots$reconstruction, c(\"batch\", \"fitted\"))",
  "  }",
  "  output_scale <- if (is.null(dots$output_scale)) {",
  "    \"analysis\"",
  "  } else {",
  "    match.arg(dots$output_scale, c(\"analysis\", \"original\"))",
  "  }",
  "  if (identical(preprocessing, \"legacy\")) {",
  "    if (!identical(reconstruction, \"batch\")) {",
  "      stop(",
  "        \"Fitted reconstruction requires `preprocessing = \\\"fitted\\\"`.\",",
  "        call. = FALSE",
  "      )",
  "    }",
  "    if (!identical(output_scale, \"analysis\")) {",
  "      stop(",
  "        \"Original-scale output requires `preprocessing = \\\"fitted\\\"`.\",",
  "        call. = FALSE",
  "      )",
  "    }",
  "    return(.predict_msma_legacy(object = object, newX = newX, newY = newY))",
  "  }",
  "  .predict_msma_fitted(",
  "    object = object,",
  "    newX = newX,",
  "    newY = newY,",
  "    reconstruction = reconstruction,",
  "    output_scale = output_scale",
  "  )",
  "}"
)
src <- c(
  if (predict_range[["start"]] > 1L) src[seq_len(predict_range[["start"]] - 1L)],
  predict_replacement,
  if (predict_range[["end"]] < length(src)) src[seq.int(predict_range[["end"]] + 1L, length(src))]
)

writeLines(src, "src.r", useBytes = TRUE)
syntax <- try(parse(file = "src.r"), silent = TRUE)
if (inherits(syntax, "try-error")) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop(as.character(syntax), call. = FALSE)
}

check <- readLines("src.r", warn = FALSE, encoding = "UTF-8")
stopifnot(
  sum(grepl("output_scale = c[(]", check)) == 1L,
  sum(grepl("output_scale = output_scale", check, fixed = TRUE)) == 1L,
  sum(grepl("Original-scale output requires", check, fixed = TRUE)) == 1L
)

cat("M3-E prediction output scale integrated.\n")
cat("Backup: ", normalizePath(backup_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
