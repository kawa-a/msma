# Frozen reference implementation derived from the manuscript-generating code.
# Do not refactor this file. Changes require a documented reference-version bump.

.reference_normvec <- function(a) {
  if (all(a == 0)) a else c(a) / sqrt(drop(crossprod(c(a))))
}

.reference_sparse <- function(x, lam, eta = 1, type = "lasso", inidx = NULL) {
  if (all(x == 0)) {
    lam <- 0
  } else if (isTRUE(length(lam) == 1) && lam >= max(abs(x))) {
    tmpdiff <- diff(sort(unique(abs(x))))
    tmpdiff <- tmpdiff[tmpdiff > 1e-8]
    lam <- max(abs(x)) - ifelse(length(tmpdiff) > 0, tail(tmpdiff, 1) / 2, 1e-6)
  }
  lam <- rep(lam, length(x))
  if (!is.null(inidx)) lam[inidx] <- 0
  sapply(seq_along(x), function(j) {
    if (type == "lasso") {
      sign(x[j]) * max(c(abs(x[j]) - lam[j], 0))
    } else if (type == "hard") {
      x[j] * (abs(x[j]) > lam[j])
    } else if (type == "scad") {
      if (eta == 2) stop("eta=2 not available for scad")
      ifelse(
        abs(x[j]) > 2 * lam[j],
        ifelse(
          abs(x[j]) <= eta * lam[j],
          ((eta - 1) * x[j] - sign(x[j]) * eta * lam[j]) / (eta - 2),
          x[j]
        ),
        sign(x[j]) * max(c(abs(x[j]) - lam[j], 0))
      )
    } else if (type == "mcp") {
      if (eta == 1) stop("eta=1 not available for mcp")
      ifelse(
        abs(x[j]) <= eta * lam[j],
        sign(x[j]) * max(c(abs(x[j]) - lam[j], 0)) / (1 - 1 / eta),
        x[j]
      )
    } else {
      stop("unknown sparse penalty")
    }
  })
}

.reference_project <- function(x, y) {
  if (is.null(ncol(y))) {
    c(x %*% cbind(y) / drop(crossprod(y)))
  } else {
    crossy <- t(y) %*% y
    i <- 0
    judge <- TRUE
    while (judge) {
      invcrossy <- try(
        solve(crossy + 1e-18 * 10^i * diag(ncol(crossy))),
        silent = TRUE
      )
      judge <- inherits(invcrossy, "try-error")
      i <- i + 1
    }
    invcrossy %*% t(y) %*% x
  }
}

S3PCA_reference <- function(Xs, Zs = NULL, comp = 6,
                            lambda = 0.02, gamma = 0,
                            excl_mode = c("sumabs", "any", "count"),
                            mu = 1.0, str = FALSE, niter = 30,
                            verbose = FALSE) {
  excl_mode <- match.arg(excl_mode)
  n <- nrow(Xs)
  p <- ncol(Xs)
  s <- .reference_normvec(rnorm(n))
  Xd <- Xs
  S <- W <- NULL
  nzi <- NULL
  c1 <- 1
  while (c1 <= comp) {
    c1 <- c1 + 1
    for (i in seq_len(niter)) {
      w <- c(t(Xd) %*% c((1 - mu) * s + mu * if (is.null(Zs)) 0 else Zs))
      if (str && length(nzi)) w[nzi] <- 0
      if (gamma > 0 && !is.null(W)) {
        if (excl_mode == "sumabs") h <- rowSums(abs(W))
        else if (excl_mode == "any") h <- as.numeric(rowSums(abs(W) > 0) > 0)
        else h <- rowSums(abs(W) > 0)
        lam_vec <- lambda + gamma * h
      } else {
        lam_vec <- lambda
      }
      w <- .reference_normvec(w)
      w <- .reference_normvec(.reference_sparse(w, lam = lam_vec, type = "lasso"))
      s <- c(Xd %*% w)
    }
    S <- cbind(S, s)
    W <- cbind(W, w)
    nz_now <- which(w != 0)
    if (length(nz_now)) nzi <- unique(c(nzi, nz_now))
    if (length(nzi) == p) break
    pred <- S %*% .reference_project(Xs, S)
    Xd <- Xs - pred
    if (verbose) {
      overlap_rate <- mean(rowSums(abs(W) > 1e-8) >= 2)
      cat(sprintf("comp=%d  overlap=%.3f  used=%d\n",
                  ncol(W), overlap_rate, length(nzi)))
    }
  }
  colnames(S) <- colnames(W) <- paste0("comp", seq_len(c1 - 1))
  rownames(W) <- colnames(Xs)
  rss <- sum(Xd^2)
  df <- sum(W != 0)
  n_ <- nrow(Xs)
  p_ <- ncol(Xs)
  aic <- n_ * log(rss / n_) + 2 * df
  bic <- n_ * log(rss / n_) + log(n_) * df
  ebic <- bic + 4 * 0.5 * log(p_)
  list(
    W = W, S = S, rss = rss, AIC = aic, BIC = bic, EBIC = ebic,
    overlap = mean(rowSums(abs(W) > 1e-8) >= 2), used_idx = nzi,
    settings = list(
      lambda = lambda, gamma = gamma, excl_mode = excl_mode,
      mu = mu, str = str, niter = niter
    )
  )
}


