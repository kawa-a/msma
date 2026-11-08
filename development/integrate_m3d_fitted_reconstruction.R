# M3-D: Add fitted reconstruction coefficients and prediction mode.
# Run from the msma Version 5 repository root after M3-C.

stopifnot(file.exists("src.r"))
src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")

if (any(grepl("^\\.build_msma_prediction_reconstruction\\s*(<-|=)\\s*function", src))) {
  stop("M3-D fitted reconstruction support already appears to be present.", call. = FALSE)
}

backup_dir <- file.path("development", "v5-prediction")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
backup_file <- file.path(backup_dir, "src-before-m3d-fitted-reconstruction.r")
file.copy("src.r", backup_file, overwrite = TRUE)

find_function_range <- function(lines, function_name) {
  escaped_name <- gsub(
    ".",
    "\\.",
    function_name,
    fixed = TRUE
  )

  start <- grep(
    paste0(
      "^",
      escaped_name,
      "\\s*(<-|=)\\s*function"
    ),
    lines
  )

  if (length(start) != 1L) {
    stop(
      "Could not uniquely locate ",
      function_name,
      "().",
      call. = FALSE
    )
  }

  brace_delta <- function(line) {
    chars <- strsplit(
      line,
      "",
      fixed = TRUE
    )[[1]]

    sum(chars == "{") -
      sum(chars == "}")
  }

  depth <- 0L
  seen_opening_brace <- FALSE
  end <- NA_integer_

  for (i in seq.int(start, length(lines))) {
    line_delta <- brace_delta(lines[[i]])

    if (grepl(
      "{",
      lines[[i]],
      fixed = TRUE
    )) {
      seen_opening_brace <- TRUE
    }

    depth <- depth + line_delta

    if (
      seen_opening_brace &&
        depth == 0L
    ) {
      end <- i
      break
    }
  }

  if (is.na(end)) {
    stop(
      "Could not locate the end of ",
      function_name,
      "().",
      call. = FALSE
    )
  }

  c(
    start = start,
    end = end
  )
}

# Add builder immediately before .attach_msma_preprocess().
attach_range <- find_function_range(src, ".attach_msma_preprocess")
insert_at <- unname(attach_range[["start"]])

builder <- c(
  "",
  "# Build training-derived coefficients for batch-independent reconstruction.",
  ".build_msma_prediction_reconstruction <- function(fit) {",
  "  build_side <- function(data, scores) {",
  "    if (is.null(data) || is.null(scores)) return(NULL)",
  "    data_blocks <- if (is.list(data)) data else list(data)",
  "    score_blocks <- if (is.list(scores)) scores else list(scores)",
  "    if (length(data_blocks) != length(score_blocks)) {",
  "      stop(\"Training data and score block counts differ.\", call. = FALSE)",
  "    }",
  "    coefficients <- lapply(seq_along(data_blocks), function(i) {",
  "      coefficient <- project(data_blocks[[i]], score_blocks[[i]])",
  "      if (!is.matrix(coefficient) || any(!is.finite(coefficient))) {",
  "        stop(\"Fitted reconstruction coefficients must be finite matrices.\", call. = FALSE)",
  "      }",
  "      coefficient",
  "    })",
  "    names(coefficients) <- names(data_blocks)",
  "    coefficients",
  "  }",
  "",
  "  x_coefficients <- build_side(fit$X, fit$sbX)",
  "  y_coefficients <- if (identical(fit$dmode, \"PLS\")) {",
  "    build_side(fit$Y, fit$sbY)",
  "  } else {",
  "    NULL",
  "  }",
  "",
  "  if (is.null(x_coefficients)) return(NULL)",
  "",
  "  structure(",
  "    list(",
  "      X = x_coefficients,",
  "      Y = y_coefficients,",
  "      scale = \"analysis\",",
  "      method = \"training-score-projection\",",
  "      version = 1L",
  "    ),",
  "    class = \"msma_prediction_reconstruction\"",
  "  )",
  "}",
  ""
)

src <- c(
  if (insert_at > 1L) src[seq_len(insert_at - 1L)],
  builder,
  src[insert_at:length(src)]
)

# Re-find attach function and insert metadata assignment before final return.
attach_range <- find_function_range(src, ".attach_msma_preprocess")
attach_lines <- src[seq.int(attach_range[["start"]], attach_range[["end"]])]
return_candidates <- which(trimws(attach_lines) == "fit")
if (!length(return_candidates)) {
  file.copy(backup_file, "src.r", overwrite = TRUE)
  stop("Could not locate the final `fit` return in .attach_msma_preprocess().", call. = FALSE)
}
return_local <- tail(return_candidates, 1L)
return_global <- attach_range[["start"]] + return_local - 1L
src <- append(
  src,
  values = c(
    "  fit$prediction_reconstruction <-",
    "    .build_msma_prediction_reconstruction(fit)",
    ""
  ),
  after = return_global - 1L
)

# Replace .predict_msma_fitted() with reconstruction-aware implementation.
fitted_range <- find_function_range(src, ".predict_msma_fitted")
fitted_replacement <- c(
  ".predict_msma_fitted <- function(",
  "  object,",
  "  newX,",
  "  newY = NULL,",
  "  reconstruction = c(\"batch\", \"fitted\")",
  ") {",
  "  reconstruction <- match.arg(reconstruction)",
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
  "  list(X = predX, sbX = sbX, Y = predY, sbY = sbY)",
  "}"
)
src <- c(
  if (fitted_range[["start"]] > 1L) src[seq_len(fitted_range[["start"]] - 1L)],
  fitted_replacement,
  if (fitted_range[["end"]] < length(src)) src[seq.int(fitted_range[["end"]] + 1L, length(src))]
)

# Replace public dispatcher with reconstruction argument parsed from ... .
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
  "  if (identical(preprocessing, \"legacy\")) {",
  "    if (!identical(reconstruction, \"batch\")) {",
  "      stop(",
  "        \"Fitted reconstruction requires `preprocessing = \\\"fitted\\\"`.\",",
  "        call. = FALSE",
  "      )",
  "    }",
  "    return(.predict_msma_legacy(object = object, newX = newX, newY = newY))",
  "  }",
  "  .predict_msma_fitted(",
  "    object = object,",
  "    newX = newX,",
  "    newY = newY,",
  "    reconstruction = reconstruction",
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
  sum(grepl("^\\.build_msma_prediction_reconstruction\\s*(<-|=)\\s*function", check)) == 1L,
  sum(grepl("prediction_reconstruction <-", check, fixed = TRUE)) >= 1L,
  sum(grepl("reconstruction = reconstruction", check, fixed = TRUE)) == 1L
)

cat("M3-D fitted reconstruction integrated.\n")
cat("Backup: ", normalizePath(backup_file, winslash = "/"), "\n", sep = "")
cat("Syntax: OK\n")
