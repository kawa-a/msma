find_source_root <- function() {
  starts <- unique(normalizePath(
    c(getwd(), testthat::test_path()),
    winslash = "/",
    mustWork = FALSE
  ))

  candidates <- unique(unlist(lapply(starts, function(start) {
    current <- start
    out <- character()
    for (i in seq_len(6L)) {
      out <- c(out, current)
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
    out
  })))

  valid <- vapply(candidates, function(path) {
    file.exists(file.path(path, "DESCRIPTION")) &&
      file.exists(file.path(path, "src.r")) &&
      dir.exists(file.path(path, "vignettes"))
  }, logical(1))

  if (!any(valid)) {
    testthat::skip("Source-tree documentation files are not available.")
  }

  candidates[which(valid)[1L]]
}


testthat::test_that(
  "Version 5 prediction documentation covers the public contract",
  {
    root <- find_source_root()
    documentation_files <- file.path(
      root,
      c(
        "NEWS",
        "README.md",
        file.path("vignettes", "msma-introduction.Rmd"),
        file.path("vignettes", "msma-multiblock.Rmd"),
        "src.r"
      )
    )

    testthat::expect_true(all(file.exists(documentation_files)))

    text <- paste(
      unlist(lapply(
        documentation_files,
        readLines,
        warn = FALSE,
        encoding = "UTF-8"
      )),
      collapse = "\n"
    )

    required_terms <- c(
      "preprocessing",
      "reconstruction",
      "output_scale",
      "analysis",
      "original",
      "sbX",
      "sbY",
      "does not predict Y from X"
    )

    for (term in required_terms) {
      testthat::expect_match(text, term, fixed = TRUE)
    }
  }
)


testthat::test_that(
  "predict.msma has one consolidated roxygen contract",
  {
    root <- find_source_root()
    src <- readLines(
      file.path(root, "src.r"),
      warn = FALSE,
      encoding = "UTF-8"
    )

    block_start <- grep(
      "^#\047 Project and Reconstruct New Data with an msma Fit$",
      src
    )

    function_line <- grep(
      "^predict\\.msma\\s*(<-|=)\\s*function",
      src
    )

    testthat::expect_length(block_start, 1L)
    testthat::expect_length(function_line, 1L)
    testthat::expect_lt(block_start, function_line)

    roxygen_block <- src[
      seq.int(
        block_start,
        function_line - 1L
      )
    ]

    required_lines <- c(
      "#\047 Project and Reconstruct New Data with an msma Fit",
      "#\047 @name predict.msma",
      "#\047 @aliases predict.msma",
      "#\047 @rdname predict.msma",
      "#\047 @docType methods",
      "#\047 @method predict msma",
      "#\047 @export"
    )

    for (line in required_lines) {
      testthat::expect_identical(
        sum(
          grepl(
            line,
            roxygen_block,
            fixed = TRUE
          )
        ),
        1L
      )
    }

    testthat::expect_identical(
      sum(
        grepl(
          "#\047 @method predict msma",
          src,
          fixed = TRUE
        )
      ),
      1L
    )

    testthat::expect_false(
      any(
        grepl(
          "This function produces a prediction from new data",
          src,
          fixed = TRUE
        )
      )
    )
  }
)


testthat::test_that(
  "prediction vignettes contain Version 5 examples",
  {
    root <- find_source_root()
    introduction <- readLines(
      file.path(root, "vignettes", "msma-introduction.Rmd"),
      warn = FALSE,
      encoding = "UTF-8"
    )
    multiblock <- readLines(
      file.path(root, "vignettes", "msma-multiblock.Rmd"),
      warn = FALSE,
      encoding = "UTF-8"
    )

    testthat::expect_true(any(grepl(
      "prediction-fitted", introduction, fixed = TRUE
    )))
    testthat::expect_true(any(grepl(
      "prediction-pls", introduction, fixed = TRUE
    )))
    testthat::expect_true(any(grepl(
      "mb-prediction", multiblock, fixed = TRUE
    )))
  }
)
