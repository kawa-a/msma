# M3-G-B: Integrate Version 5 prediction documentation.

required_files <- c(
  "src.r", "NEWS", "README.md",
  file.path("vignettes", "msma-introduction.Rmd"),
  file.path("vignettes", "msma-multiblock.Rmd")
)
stopifnot(all(file.exists(required_files)))

backup_dir <- file.path("development", "v5-prediction", "m3g-documentation-backup")
dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
for (path in required_files) {
  backup_name <- gsub("[/\\\\]", "__", path)
  file.copy(path, file.path(backup_dir, backup_name), overwrite = TRUE)
}

read_utf8 <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")
write_utf8 <- function(lines, path) writeLines(lines, path, useBytes = TRUE)

# predict.msma roxygen ---------------------------------------------------------
src <- read_utf8("src.r")

# Remove the historical predict.msma roxygen block before adding the
# consolidated Version 5 documentation.
legacy_title <- grep("^#' Prediction$", src)
legacy_method <- grep("#' @method predict msma", src, fixed = TRUE)

if (
  length(legacy_title) == 1L &&
    length(legacy_method) == 1L &&
    legacy_title < legacy_method
) {
  legacy_end <- legacy_method

  while (
    legacy_end < length(src) &&
      grepl("^#'", src[legacy_end + 1L])
  ) {
    legacy_end <- legacy_end + 1L
  }

  src <- src[-seq.int(legacy_title, legacy_end)]
}
if (!any(grepl("#' Project and Reconstruct New Data with an msma Fit", src, fixed = TRUE))) {
  predict_line <- grep("^predict\\.msma\\s*(<-|=)\\s*function", src)
  stopifnot(length(predict_line) == 1L)
  roxygen <- c(
    "#' Project and Reconstruct New Data with an msma Fit",
    "#'",
    "#' Projects supplied X-side and, for PLS, Y-side data into the fitted",
    "#' component spaces and returns component scores and low-rank",
    "#' reconstructions.",
    "#'",
    "#' This method does not predict Y from X. When \\code{newY} is supplied,",
    "#' X and Y are projected and reconstructed independently in their",
    "#' corresponding fitted component spaces.",
    "#'",
    "#' Historical prediction behavior remains the default. Additional",
    "#' Version 5 prediction controls can be supplied through \\code{...}.",
    "#'",
    "#' @param object an object of class \\code{\\\"msma\\\"}.",
    "#' @param newX a numeric matrix or list of numeric matrices for the X side.",
    "#' @param newY an optional numeric matrix or list of numeric matrices for",
    "#'   the Y side. Fitted PLS prediction requires \\code{newY}. The supplied",
    "#'   Y data are projected and reconstructed independently and are not",
    "#'   predicted from \\code{newX}.",
    "#' @param ... additional prediction controls:",
    "#'   \\describe{",
    "#'     \\item{\\code{preprocessing}}{Either \\code{\\\"legacy\\\"} or",
    "#'       \\code{\\\"fitted\\\"}. The default preserves historical numerical",
    "#'       behavior; fitted preprocessing reuses training center and scale.}",
    "#'     \\item{\\code{reconstruction}}{Either \\code{\\\"batch\\\"} or",
    "#'       \\code{\\\"fitted\\\"}. Fitted reconstruction uses training-derived",
    "#'       coefficients and is independent of prediction-batch composition.}",
    "#'     \\item{\\code{output_scale}}{Either \\code{\\\"analysis\\\"} or",
    "#'       \\code{\\\"original\\\"}. Original output applies inverse fitted",
    "#'       preprocessing to reconstructed X and Y blocks.}",
    "#'   }",
    "#'",
    "#' @return a list with four elements:",
    "#'   \\describe{",
    "#'     \\item{\\code{X}}{X-side low-rank reconstructions.}",
    "#'     \\item{\\code{sbX}}{X-side component scores on the analysis scale.}",
    "#'     \\item{\\code{Y}}{Y-side low-rank reconstructions, or \\code{NULL} for PCA.}",
    "#'     \\item{\\code{sbY}}{Y-side component scores, or \\code{NULL} for PCA.}",
    "#'   }",
    "#'",
    "#' @details",
    "#' Fitted reconstruction and original-scale output require",
    "#' \\code{preprocessing = \\\"fitted\\\"}. Scores remain on the analysis scale.",
    "#'",
    "#' @examples",
    "#' set.seed(1)",
    "#' X <- matrix(rnorm(180), nrow = 30, ncol = 6)",
    "#' colnames(X) <- paste0(\"x\", seq_len(ncol(X)))",
    "#' fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)",
    "#' prediction <- predict(",
    "#'   fit,",
    "#'   newX = X[1:5, , drop = FALSE],",
    "#'   preprocessing = \"fitted\",",
    "#'   reconstruction = \"fitted\",",
    "#'   output_scale = \"original\"",
    "#' )",
    "#' names(prediction)",
    "#'",
    "#' @name predict.msma",
    "#' @aliases predict.msma",
    "#' @rdname predict.msma",
    "#' @docType methods",
    "#' @method predict msma",
    "#' @export"
  )
  src <- append(src, roxygen, after = predict_line - 1L)
  write_utf8(src, "src.r")
}

# NEWS ------------------------------------------------------------------------
news <- read_utf8("NEWS")
if (!any(grepl("PREDICTION WORKFLOW", news, fixed = TRUE))) {
  header <- grep("^Changes in development version", news)
  stopifnot(length(header) == 1L)
  block <- c(
    "", "PREDICTION WORKFLOW", "",
    "* Added fitted preprocessing to predict.msma() through",
    "  preprocessing = \"fitted\".", "",
    "* Added training-derived fixed reconstruction coefficients through",
    "  reconstruction = \"fitted\".", "",
    "* Added reconstruction output-scale control through",
    "  output_scale = \"analysis\" and output_scale = \"original\".", "",
    "* Preserved historical prediction behavior as the default for backward",
    "  numerical compatibility.", "",
    "* Added validation of block counts, dimensions, column names, and column",
    "  order for fitted preprocessing.", "",
    "* Added serialization of preprocessing and prediction-reconstruction",
    "  metadata.", "",
    "* Clarified that predict.msma() projects and reconstructs X and Y",
    "  independently and does not predict Y from X."
  )
  news <- append(news, block, after = header)
  write_utf8(news, "NEWS")
}

# README ----------------------------------------------------------------------
readme <- read_utf8("README.md")
if (!any(grepl("## Version 5 development: prediction workflow", readme, fixed = TRUE))) {
  installation <- grep("^## Installation$", readme)
  stopifnot(length(installation) == 1L)
  block <- c(
    "## Version 5 development: prediction workflow", "",
    "The Version 5 development branch extends `predict.msma()` while preserving",
    "the historical prediction behavior as the default.", "",
    "`predict.msma()` projects and reconstructs supplied X-side and Y-side",
    "data. It does not predict Y from X.", "", "```r", "set.seed(1)",
    "X <- matrix(stats::rnorm(180), nrow = 30, ncol = 6)",
    "colnames(X) <- paste0(\"x\", seq_len(ncol(X)))", "",
    "fit <- msma(X = X, comp = 2, scaling = TRUE, intseed = 3)", "",
    "prediction <- predict(", "  fit,", "  newX = X[1:5, , drop = FALSE],",
    "  preprocessing = \"fitted\",", "  reconstruction = \"fitted\",",
    "  output_scale = \"original\"", ")", "", "names(prediction)", "```", "",
    "Prediction controls are `preprocessing`, `reconstruction`, and",
    "`output_scale`. Component scores `sbX` and `sbY` remain on the analysis",
    "scale. `output_scale` controls reconstructed `X` and `Y` values.", ""
  )
  readme <- c(readme[seq_len(installation - 1L)], block, readme[installation:length(readme)])
  write_utf8(readme, "README.md")
}

# Introduction vignette -------------------------------------------------------
intro_file <- file.path("vignettes", "msma-introduction.Rmd")
intro <- read_utf8(intro_file)
start <- grep("^# Prediction$", intro)
session <- grep("^# Session information$", intro)
if (length(start) == 1L) {
  stopifnot(length(session) == 1L, start < session)
  block <- c(
    "# Prediction and low-rank reconstruction", "",
    "`predict.msma()` projects supplied data into fitted component spaces and",
    "returns component scores and low-rank reconstructions. It does not predict",
    "`Y` from `X`. For PLS, `newX` and `newY` are reconstructed independently.", "",
    "The returned elements are reconstructed `X`, X-side scores `sbX`,",
    "reconstructed `Y`, and Y-side scores `sbY`. For PCA, `Y` and `sbY` are `NULL`.", "",
    "## Historical prediction behavior", "",
    "Calling `predict()` without Version 5 controls preserves historical behavior.", "",
    "```{r prediction-legacy}",
    "legacy_prediction <- predict(fit_pca, newX = X[1:5, , drop = FALSE])",
    "names(legacy_prediction)", "```", "",
    "## Fitted preprocessing and reconstruction", "",
    "Fitted preprocessing reuses training center and scale. Fitted reconstruction",
    "uses training-derived reconstruction coefficients.", "",
    "```{r prediction-fitted}", "fitted_prediction <- predict(", "  fit_pca,",
    "  newX = X[1:5, , drop = FALSE],", "  preprocessing = \"fitted\",",
    "  reconstruction = \"fitted\",", "  output_scale = \"original\"", ")",
    "lapply(fitted_prediction$X, dim)", "lapply(fitted_prediction$sbX, dim)", "```", "",
    "`output_scale = \"analysis\"` returns centered or standardized reconstructions.",
    "`output_scale = \"original\"` returns reconstructions on original variable scales.",
    "Component scores remain on the analysis scale.", "",
    "## PLS prediction", "", "```{r prediction-pls}", "prediction_pls <- predict(",
    "  fit_pls,", "  newX = X[1:5, , drop = FALSE],",
    "  newY = Y[1:5, , drop = FALSE],", "  preprocessing = \"fitted\",",
    "  reconstruction = \"fitted\",", "  output_scale = \"original\"", ")",
    "names(prediction_pls)", "```", "",
    "`newY` is not an outcome predicted from `newX`. The supplied `newY` is",
    "projected and reconstructed independently.", ""
  )
  intro <- c(intro[seq_len(start - 1L)], block, intro[session:length(intro)])
  write_utf8(intro, intro_file)
}

# Multiblock vignette ---------------------------------------------------------
mb_file <- file.path("vignettes", "msma-multiblock.Rmd")
mb <- read_utf8(mb_file)
if (!any(grepl("# Prediction for multiblock data", mb, fixed = TRUE))) {
  session <- grep("^# Session information$", mb)
  stopifnot(length(session) == 1L)
  block <- c(
    "# Prediction for multiblock data", "",
    "New multiblock data must use the same block structure, variable names,",
    "and variable order as training data when fitted preprocessing is requested.", "",
    "```{r mb-prediction}",
    "Xnew <- lapply(X, function(block) block[1:5, , drop = FALSE])", "",
    "prediction_mb <- predict(", "  fit_mb_pca,", "  newX = Xnew,",
    "  preprocessing = \"fitted\",", "  reconstruction = \"fitted\",",
    "  output_scale = \"original\"", ")", "",
    "names(prediction_mb$X)", "lapply(prediction_mb$X, colnames)",
    "lapply(prediction_mb$sbX, dim)", "```", "",
    "Fitted preprocessing preserves multiblock names and validates column names",
    "and order within each block.", ""
  )
  mb <- c(mb[seq_len(session - 1L)], block, mb[session:length(mb)])
  write_utf8(mb, mb_file)
}

syntax <- try(parse(file = "src.r"), silent = TRUE)
if (inherits(syntax, "try-error")) stop(as.character(syntax), call. = FALSE)
cat("M3-G prediction documentation integrated.\n")
cat("src.r syntax: OK\n")
