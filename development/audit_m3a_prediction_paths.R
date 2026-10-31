# M3-A: Audit prediction paths before any implementation change.
# Run from the msma Version 5 repository root.

stopifnot(file.exists("src.r"), dir.exists("tests/testthat"))

out_dir <- file.path("development", "v5-prediction")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

src <- readLines("src.r", warn = FALSE, encoding = "UTF-8")
test_files <- list.files(
  file.path("tests", "testthat"),
  pattern = "[.][Rr]$",
  full.names = TRUE
)

find_definitions <- function(lines) {
  pattern <- "^[.A-Za-z][.A-Za-z0-9_]*\\s*(<-|=)\\s*function\\s*\\("
  idx <- grep(pattern, lines)
  names <- sub(
    "^([.A-Za-z][.A-Za-z0-9_]*)\\s*(<-|=).*$",
    "\\1",
    lines[idx]
  )
  data.frame(
    line = idx,
    function_name = names,
    definition = lines[idx],
    stringsAsFactors = FALSE
  )
}

all_defs <- find_definitions(src)
prediction_defs <- all_defs[
  grepl(
    "predict|project|score|reconstruct|preprocess",
    all_defs$function_name,
    ignore.case = TRUE
  ),
  ,
  drop = FALSE
]

prediction_pattern <- paste(
  c(
    "predict\\.",
    "predictiv",
    "prediction",
    "newdata",
    "Xnew",
    "Ynew",
    "project",
    "Xcenter",
    "Xscale",
    "Ycenter",
    "Yscale",
    "preprocess",
    "scaled:center",
    "scaled:scale",
    "sweep\\(",
    "scale\\("
  ),
  collapse = "|"
)

src_hits <- grep(prediction_pattern, src, ignore.case = TRUE)
context_idx <- sort(unique(unlist(lapply(src_hits, function(i) {
  seq.int(max(1L, i - 5L), min(length(src), i + 10L))
}))))

writeLines(
  sprintf("%5d | %s", context_idx, src[context_idx]),
  file.path(out_dir, "M3-A-SOURCE-CONTEXT.txt"),
  useBytes = TRUE
)

write.csv(
  prediction_defs,
  file.path(out_dir, "prediction-function-definitions.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

call_pattern <- paste(
  c(
    "predict\\(",
    "predict\\.",
    "predictiv",
    "newdata",
    "Xnew",
    "Ynew",
    "project",
    "preprocess"
  ),
  collapse = "|"
)

test_inventory <- do.call(
  rbind,
  lapply(test_files, function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    idx <- grep(call_pattern, lines, ignore.case = TRUE)
    if (!length(idx)) return(NULL)
    data.frame(
      file = path,
      line = idx,
      text = lines[idx],
      stringsAsFactors = FALSE
    )
  })
)

if (is.null(test_inventory)) {
  test_inventory <- data.frame(
    file = character(),
    line = integer(),
    text = character(),
    stringsAsFactors = FALSE
  )
}

write.csv(
  test_inventory,
  file.path(out_dir, "prediction-test-inventory.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

public_candidates <- c(
  "predict.msma",
  "predict.s4pca",
  "s4pca_reconstruct",
  ".s4pca_project_reference",
  ".apply_msma_preprocessor",
  ".invert_msma_preprocessor"
)

function_status <- data.frame(
  function_name = public_candidates,
  exists = vapply(
    public_candidates,
    exists,
    logical(1),
    mode = "function",
    inherits = TRUE
  ),
  stringsAsFactors = FALSE
)

write.csv(
  function_status,
  file.path(out_dir, "prediction-function-status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

capture.output(
  {
    cat("msma Version 5 M3-A prediction path audit\n")
    cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), "\n")
    cat("Branch:", system2("git", c("branch", "--show-current"), stdout = TRUE), "\n")
    cat("Commit:", system2("git", c("rev-parse", "HEAD"), stdout = TRUE), "\n\n")

    cat("Function status\n")
    print(function_status, row.names = FALSE)

    cat("\nPrediction-related definitions in src.r\n")
    if (nrow(prediction_defs)) {
      print(prediction_defs, row.names = FALSE)
    } else {
      cat("No matching definitions found.\n")
    }

    cat("\nPrediction-related source hits:", length(src_hits), "\n")
    cat("Prediction-related test hits:", nrow(test_inventory), "\n")

    cat("\nPublic fit fields containing prediction terms\n")
    set.seed(1201)
    X <- matrix(stats::rnorm(120), nrow = 20, ncol = 6)
    Y <- matrix(stats::rnorm(80), nrow = 20, ncol = 4)
    Z <- as.numeric(scale(X[, 1] + stats::rnorm(20)))
    colnames(X) <- paste0("x", seq_len(ncol(X)))
    colnames(Y) <- paste0("y", seq_len(ncol(Y)))

    fits <- list(
      legacy_pca = msma(X = X, comp = 2, scaling = TRUE, intseed = 3),
      legacy_pls = msma(X = X, Y = Y, comp = 2, scaling = TRUE, intseed = 3),
      extended = msma(X = X, comp = 2, sprmethod = "NMF", scaling = FALSE, intseed = 3),
      soft = msma(
        X = X, Z = Z, comp = 2, lambdaX = 0.1, muX = 0.8,
        structure.method = "soft", gammaX = 0.1, niterS4 = 5,
        scaling = TRUE, intseed = 4
      ),
      exclusive = msma(
        X = X, Z = Z, comp = 2, lambdaX = 0.1, muX = 0.8,
        structure.method = "exclusive", gammaX = 0, niterS4 = 5,
        scaling = TRUE, intseed = 4
      )
    )

    for (fit_name in names(fits)) {
      fit <- fits[[fit_name]]
      matching <- names(fit)[grepl("predict|score|reconstruct|preprocess", names(fit), ignore.case = TRUE)]
      cat("\n", fit_name, "\n", sep = "")
      cat("class:", paste(class(fit), collapse = ", "), "\n")
      cat("matching fields:", paste(matching, collapse = ", "), "\n")
      for (field in matching) {
        cat("\n$", field, "\n", sep = "")
        str(fit[[field]], max.level = 2)
      }
    }
  },
  file = file.path(out_dir, "M3-A-PREDICTION-AUDIT.txt")
)

cat("M3-A audit files created in: ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
cat("No source or test implementation was changed.\n")
