.validate_one_structure <- function(structure, p, variable_names = NULL,
                                    symmetric = TRUE, tolerance = 1e-8) {
  if (is.null(structure)) return(NULL)
  structure <- as.matrix(structure)
  if (!is.numeric(structure)) stop("structure must be numeric", call. = FALSE)
  if (!identical(dim(structure), c(p, p))) {
    stop("structure must be a square matrix matching the number of variables", call. = FALSE)
  }
  if (any(!is.finite(structure))) {
    stop("structure must not contain NA, NaN, or Inf", call. = FALSE)
  }
  if (symmetric && !isTRUE(all.equal(structure, t(structure), tolerance = tolerance))) {
    stop("structure must be symmetric", call. = FALSE)
  }
  if (!is.null(variable_names) && !is.null(rownames(structure))) {
    if (!identical(rownames(structure), variable_names) ||
        !identical(colnames(structure), variable_names)) {
      stop("structure names must match the data variable names and order", call. = FALSE)
    }
  }
  structure
}

.validate_structure_blocks <- function(structure, data, side = "X") {
  data <- if (is.list(data)) data else list(data)
  if (is.null(structure)) return(rep(list(NULL), length(data)))
  structure <- if (is.list(structure)) structure else list(structure)
  if (length(structure) == 1L && length(data) > 1L) {
    stop(side, " structure must contain one matrix per block", call. = FALSE)
  }
  if (length(structure) != length(data)) {
    stop(side, " structure list length must match the number of blocks", call. = FALSE)
  }
  Map(function(S, D) .validate_one_structure(S, ncol(D), colnames(D)),
      structure, data)
}


