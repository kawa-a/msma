# msma Version 5 dispatcher audit

- Date: 2026-10-31 10:11:07 +0900
- Source version: 5.0.0.9000
- Branch: feature/v5-dispatcher
- Commit: 5491b49d8e56e0d36ecfdc39302bb6560718c98f

## Audit objective

Identify the current dispatch rules for legacy, soft-structured, and exclusive PCA before refactoring.
No numerical implementation is changed by this audit.

## msma definitions

```r
  100 | #' ##### Sparse Principal Component Analysis #####
  101 | #' fit3 = msma(X, comp=5, lambdaX=2.5)
  102 | #' summary(fit3)
  103 | #' 
  104 | msma = function(X, ...) UseMethod("msma")
  105 | 
  106 | #' @rdname msma
  107 | #' @method msma default
  108 | #' @export
  109 | # Backward-compatible dispatcher for msma 3.2.
  110 | # The exact version 3.1 engine is used unless a version 3.2 feature is requested.
  111 | msma.default = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL,
  112 |   lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL,
```

## structure method

```r
   51 | #' @param ceps a numeric scalar for the convergence condition of the algorithm
   52 | #' @param sprmethod a character, 
   53 | #' @param nneg a character 
   54 | #' @param con4spv a numeric vector
   55 | #' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
   56 | #' @param gammaX non-negative history-dependent penalty strength.
   57 | #' @param history.method one of \"sumabs\", \"any\", or \"count\".
   58 | #' @param niterS4 fixed number of structured PCA updates per component.
   59 | #' @param ... further arguments passed to or from other methods.
   60 | #' @return \item{dmode}{Which modes "PLS" or "PCA"}
   61 | #' @return \item{X}{Scaled X which has a list form.}
   62 | #' @return \item{Y}{Scaled Y which has a list form.}
   63 | #' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
  113 |   inXsup=NULL, inYsup=NULL, muX=0, muY=0, defmethod="canonical",
  114 |   scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  115 |   sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  116 |   nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  117 |   structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  118 |   history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
  119 | {
  120 |   structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  123 |   if(use_s4pca){
  124 |     if(identical(structure.method, "none")) structure.method = "soft"
  125 |     if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
  126 |     if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
  127 |     if(length(comp)!=1L) stop("Structured PCA requires a single component count", call.=FALSE)
  128 |     if(!is.null(lambdaY) || !is.null(lambdaXsup) || !is.null(lambdaYsup)) stop("Y-side and super-level penalties are not supported for structured PCA", call.=FALSE)
  129 |     if(!identical(type,"lasso")) stop("Structured PCA supports type = \"lasso\" only", call.=FALSE)
  130 |     if(!is.null(inX) || !is.null(inY) || !is.null(inXsup) || !is.null(inYsup)) stop("Forced-in variables are not supported for structured PCA", call.=FALSE)
  131 |     if(!identical(sprmethod,"PCA") || !is.null(con4spv)) stop("Structured PCA cannot be combined with NMF, sNMF, CMP, or con4spv", call.=FALSE)
  132 |     if(!is.null(Z) && !is.null(dim(Z)) && ncol(as.matrix(Z))>1L) stop("Structured PCA accepts only vector or one-column Z", call.=FALSE)
  139 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  140 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  141 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  142 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  143 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  144 |       niter=niterS4,seed=intseed,verbose=verbose)
  145 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  146 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  147 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  148 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  149 |     class(fit_s4)="s4pca"
  150 |     return(fit_s4)
  151 |   }
  152 |   z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  153 |   z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  154 |   z_is_multivariate = z_ncol > 1L
  155 |   use_extended = !identical(sprmethod, "PCA") || z_is_multivariate || !is.null(con4spv)
 2512 |     fit <- msma(
 2513 |       X = X[train_idx, , drop = FALSE],
 2514 |       Z = if (is.null(Z)) NULL else Z[train_idx],
 2515 |       comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
 2516 |       structure.method = "soft", gammaX = grid$gammaX[g],
 2517 |       history.method = history.method, niterS4 = niterS4,
 2518 |       scaling = scaling, intseed = seed + g
 2519 |     )
 2520 |     pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
 2521 |     conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
 2522 |     q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
 2523 |     out[[g]] <- data.frame(
 2524 |       lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
 2610 |   selected <- summary[eligible[ord[1]], , drop = FALSE]
 2611 |   final_fit <- NULL
 2612 |   if (refit) {
 2613 |     final_fit <- msma(X = X, Z = Z, comp = selected$comp,
 2614 |       lambdaX = selected$lambdaX, muX = muX, structure.method = "soft",
 2615 |       gammaX = selected$gammaX, history.method = input$history_method,
 2616 |       niterS4 = input$niter, scaling = scaling, intseed = intseed)
 2617 |   }
 2618 |   result <- list(selected = selected, summary = summary, details = details,
 2619 |     fit = final_fit, settings = list(alpha = alpha, repeats = as.integer(repeats),
 2620 |       calibration_fraction = calibration_fraction, max_overlap = max_overlap,
 2621 |       muX = muX, history.method = input$history_method,
 2622 |       niterS4 = input$niter, scaling = scaling, intseed = intseed))
```

## gamma x

```r
   52 | #' @param sprmethod a character, 
   53 | #' @param nneg a character 
   54 | #' @param con4spv a numeric vector
   55 | #' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
   56 | #' @param gammaX non-negative history-dependent penalty strength.
   57 | #' @param history.method one of \"sumabs\", \"any\", or \"count\".
   58 | #' @param niterS4 fixed number of structured PCA updates per component.
   59 | #' @param ... further arguments passed to or from other methods.
   60 | #' @return \item{dmode}{Which modes "PLS" or "PCA"}
   61 | #' @return \item{X}{Scaled X which has a list form.}
   62 | #' @return \item{Y}{Scaled Y which has a list form.}
   63 | #' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
   64 | #' @return \item{Yscale}{Scaling information for Y. The means and standard deviations for each block of Y are returned.}
  113 |   inXsup=NULL, inYsup=NULL, muX=0, muY=0, defmethod="canonical",
  114 |   scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  115 |   sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  116 |   nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  117 |   structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  118 |   history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
  119 | {
  120 |   structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  123 |   if(use_s4pca){
  124 |     if(identical(structure.method, "none")) structure.method = "soft"
  125 |     if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
  126 |     if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
  127 |     if(length(comp)!=1L) stop("Structured PCA requires a single component count", call.=FALSE)
  128 |     if(!is.null(lambdaY) || !is.null(lambdaXsup) || !is.null(lambdaYsup)) stop("Y-side and super-level penalties are not supported for structured PCA", call.=FALSE)
  129 |     if(!identical(type,"lasso")) stop("Structured PCA supports type = \"lasso\" only", call.=FALSE)
  130 |     if(!is.null(inX) || !is.null(inY) || !is.null(inXsup) || !is.null(inYsup)) stop("Forced-in variables are not supported for structured PCA", call.=FALSE)
  138 |       X_scale[!is.finite(X_scale) | X_scale==0]=1
  139 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  140 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  141 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  142 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  143 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  144 |       niter=niterS4,seed=intseed,verbose=verbose)
  145 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  146 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  147 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  148 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  149 |     class(fit_s4)="s4pca"
  150 |     return(fit_s4)
  151 |   }
  152 |   z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  153 |   z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  154 |   z_is_multivariate = z_ncol > 1L
  155 |   use_extended = !identical(sprmethod, "PCA") || z_is_multivariate || !is.null(con4spv)
  156 | 
 2483 |   }
 2484 |   Xhat
 2485 | }
 2486 | 
 2487 | .s4pca_grid <- function(lambdaX, gammaX, comp) {
 2488 |   if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
 2489 |     stop("lambdaX must contain non-negative finite values", call. = FALSE)
 2490 |   }
 2491 |   if (!is.numeric(gammaX) || !length(gammaX) || any(!is.finite(gammaX)) || any(gammaX < 0)) {
 2492 |     stop("gammaX must contain non-negative finite values", call. = FALSE)
 2493 |   }
 2494 |   if (!is.numeric(comp) || !length(comp) || any(!is.finite(comp)) || any(comp < 1) || any(comp != as.integer(comp))) {
 2495 |     stop("comp must contain positive integers", call. = FALSE)
 2496 |   }
 2497 |   unique(expand.grid(lambdaX = as.numeric(lambdaX), gammaX = as.numeric(gammaX),
 2498 |                      comp = as.integer(comp), KEEP.OUT.ATTRS = FALSE,
 2499 |                      stringsAsFactors = FALSE))
 2500 | }
 2501 | 
 2502 | .s4pca_one_split <- function(X, Z, grid, calibration_fraction, alpha,
 2503 |                              max_overlap, muX, history.method, niterS4,
 2504 |                              scaling, seed) {
 2505 |   set.seed(seed)
 2512 |     fit <- msma(
 2513 |       X = X[train_idx, , drop = FALSE],
 2514 |       Z = if (is.null(Z)) NULL else Z[train_idx],
 2515 |       comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
 2516 |       structure.method = "soft", gammaX = grid$gammaX[g],
 2517 |       history.method = history.method, niterS4 = niterS4,
 2518 |       scaling = scaling, intseed = seed + g
 2519 |     )
 2520 |     pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
 2521 |     conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
 2522 |     q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
 2523 |     out[[g]] <- data.frame(
 2524 |       lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
 2525 |       score = q, overlap_all = fit$overlap_all,
 2526 |       overlap_selected = fit$overlap_selected,
 2527 |       admissible = fit$overlap_selected <= max_overlap
 2528 |     )
 2529 |   }
 2530 |   do.call(rbind, out)
 2531 | }
 2532 | 
 2539 | #'
 2540 | #' @param X numeric data matrix.
 2541 | #' @param Z optional numeric supervision vector or one-column matrix.
 2542 | #' @param lambdaX candidate sparsity parameters.
 2543 | #' @param gammaX candidate soft-structure parameters.
 2544 | #' @param comp candidate component counts.
 2545 | #' @param alpha miscoverage level in (0,1).
 2546 | #' @param repeats positive number of repeated splits.
 2547 | #' @param calibration_fraction fraction assigned to calibration in each split.
 2548 | #' @param max_overlap maximum admissible \code{overlap_selected}.
 2549 | #' @param muX supervision weight in [0,1].
 2550 | #' @param history.method one of \code{"sumabs"}, \code{"any"}, or \code{"count"}.
 2551 | #' @param niterS4 fixed S4PCA updates per component.
 2554 | #' @param refit logical; refit the selected model on all observations.
 2555 | #' @return an object of class \code{s4pca_selection}.
 2556 | #' @export
 2557 | s4pca_conformal_select <- function(
 2558 |     X, Z = NULL, lambdaX = c(0.05, 0.1, 0.2), gammaX = c(0, 0.05, 0.1),
 2559 |     comp = 2:4, alpha = 0.1, repeats = 5, calibration_fraction = 0.25,
 2560 |     max_overlap = 1, muX = 1, history.method = "sumabs", niterS4 = 30,
 2561 |     scaling = TRUE, intseed = 1, refit = TRUE) {
 2562 |   input <- .s4pca_validate_inputs(X = X, Z = Z, comp = 1, lambda = 0,
 2563 |     gamma = 0, history_method = history.method, mu = muX,
 2564 |     hard_exclusion = FALSE, niter = niterS4, seed = intseed)
 2565 |   X <- input$X; Z <- input$Z
 2566 |   if (nrow(X) < 4L) stop("X must contain at least four rows for split selection", call. = FALSE)
 2581 |   }
 2582 |   if (length(refit) != 1L || !is.logical(refit) || is.na(refit)) {
 2583 |     stop("refit must be TRUE or FALSE", call. = FALSE)
 2584 |   }
 2585 |   grid <- .s4pca_grid(lambdaX, gammaX, comp)
 2586 |   split_results <- lapply(seq_len(as.integer(repeats)), function(b) {
 2587 |     z <- .s4pca_one_split(X, Z, grid, calibration_fraction, alpha, max_overlap,
 2588 |       muX, input$history_method, input$niter, scaling, intseed + 1000L * b)
 2589 |     z[["repeat"]] <- b
 2590 |     z
 2591 |   })
 2592 |   details <- do.call(rbind, split_results)
 2593 |   key <- interaction(details$lambdaX, details$gammaX, details$comp, drop = TRUE)
 2594 |   groups <- split(details, key)
 2595 |   summary <- do.call(rbind, lapply(groups, function(d) data.frame(
 2596 |     lambdaX = d$lambdaX[1], gammaX = d$gammaX[1], comp = d$comp[1],
 2597 |     score = stats::median(d$score),
 2598 |     overlap_all = stats::median(d$overlap_all),
 2599 |     overlap_selected = stats::median(d$overlap_selected),
 2600 |     admissible = stats::median(d$overlap_selected) <= max_overlap
 2601 |   )))
 2602 |   rownames(summary) <- NULL
 2603 |   eligible <- which(summary$admissible)
 2604 |   if (!length(eligible)) {
 2605 |     warning("No candidate satisfied max_overlap; selecting from the full grid", call. = FALSE)
 2606 |     eligible <- seq_len(nrow(summary))
 2607 |   }
 2608 |   ord <- order(summary$score[eligible], summary$overlap_selected[eligible],
 2609 |                summary$comp[eligible], summary$lambdaX[eligible], summary$gammaX[eligible])
 2610 |   selected <- summary[eligible[ord[1]], , drop = FALSE]
 2611 |   final_fit <- NULL
 2612 |   if (refit) {
 2613 |     final_fit <- msma(X = X, Z = Z, comp = selected$comp,
 2614 |       lambdaX = selected$lambdaX, muX = muX, structure.method = "soft",
 2615 |       gammaX = selected$gammaX, history.method = input$history_method,
 2616 |       niterS4 = input$niter, scaling = scaling, intseed = intseed)
 2617 |   }
 2618 |   result <- list(selected = selected, summary = summary, details = details,
 2619 |     fit = final_fit, settings = list(alpha = alpha, repeats = as.integer(repeats),
 2620 |       calibration_fraction = calibration_fraction, max_overlap = max_overlap,
 2621 |       muX = muX, history.method = input$history_method,
 2622 |       niterS4 = input$niter, scaling = scaling, intseed = intseed))
 2623 |   class(result) <- "s4pca_selection"
```

## history method

```r
   53 | #' @param nneg a character 
   54 | #' @param con4spv a numeric vector
   55 | #' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
   56 | #' @param gammaX non-negative history-dependent penalty strength.
   57 | #' @param history.method one of \"sumabs\", \"any\", or \"count\".
   58 | #' @param niterS4 fixed number of structured PCA updates per component.
   59 | #' @param ... further arguments passed to or from other methods.
   60 | #' @return \item{dmode}{Which modes "PLS" or "PCA"}
   61 | #' @return \item{X}{Scaled X which has a list form.}
   62 | #' @return \item{Y}{Scaled Y which has a list form.}
   63 | #' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
   64 | #' @return \item{Yscale}{Scaling information for Y. The means and standard deviations for each block of Y are returned.}
   65 | #' @return \item{comp}{the number of componets}
  114 |   scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  115 |   sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  116 |   nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  117 |   structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  118 |   history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
  119 | {
  120 |   structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  123 |   if(use_s4pca){
  124 |     if(identical(structure.method, "none")) structure.method = "soft"
  125 |     if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
  126 |     if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
  139 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  140 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  141 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  142 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  143 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  144 |       niter=niterS4,seed=intseed,verbose=verbose)
  145 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  146 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  147 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  148 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  149 |     class(fit_s4)="s4pca"
  150 |     return(fit_s4)
  151 |   }
 2499 |                      stringsAsFactors = FALSE))
 2500 | }
 2501 | 
 2502 | .s4pca_one_split <- function(X, Z, grid, calibration_fraction, alpha,
 2503 |                              max_overlap, muX, history.method, niterS4,
 2504 |                              scaling, seed) {
 2505 |   set.seed(seed)
 2506 |   n <- nrow(X)
 2507 |   n_cal <- max(2L, min(n - 2L, as.integer(ceiling(n * calibration_fraction))))
 2508 |   cal_idx <- sort(sample.int(n, n_cal, replace = FALSE))
 2509 |   train_idx <- setdiff(seq_len(n), cal_idx)
 2510 |   out <- vector("list", nrow(grid))
 2511 |   for (g in seq_len(nrow(grid))) {
 2513 |       X = X[train_idx, , drop = FALSE],
 2514 |       Z = if (is.null(Z)) NULL else Z[train_idx],
 2515 |       comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
 2516 |       structure.method = "soft", gammaX = grid$gammaX[g],
 2517 |       history.method = history.method, niterS4 = niterS4,
 2518 |       scaling = scaling, intseed = seed + g
 2519 |     )
 2520 |     pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
 2521 |     conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
 2522 |     q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
 2523 |     out[[g]] <- data.frame(
 2524 |       lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
 2525 |       score = q, overlap_all = fit$overlap_all,
 2546 | #' @param repeats positive number of repeated splits.
 2547 | #' @param calibration_fraction fraction assigned to calibration in each split.
 2548 | #' @param max_overlap maximum admissible \code{overlap_selected}.
 2549 | #' @param muX supervision weight in [0,1].
 2550 | #' @param history.method one of \code{"sumabs"}, \code{"any"}, or \code{"count"}.
 2551 | #' @param niterS4 fixed S4PCA updates per component.
 2552 | #' @param scaling logical data scaling indicator.
 2553 | #' @param intseed random seed.
 2554 | #' @param refit logical; refit the selected model on all observations.
 2555 | #' @return an object of class \code{s4pca_selection}.
 2556 | #' @export
 2557 | s4pca_conformal_select <- function(
 2558 |     X, Z = NULL, lambdaX = c(0.05, 0.1, 0.2), gammaX = c(0, 0.05, 0.1),
 2559 |     comp = 2:4, alpha = 0.1, repeats = 5, calibration_fraction = 0.25,
 2560 |     max_overlap = 1, muX = 1, history.method = "sumabs", niterS4 = 30,
 2561 |     scaling = TRUE, intseed = 1, refit = TRUE) {
 2562 |   input <- .s4pca_validate_inputs(X = X, Z = Z, comp = 1, lambda = 0,
 2563 |     gamma = 0, history_method = history.method, mu = muX,
 2564 |     hard_exclusion = FALSE, niter = niterS4, seed = intseed)
 2565 |   X <- input$X; Z <- input$Z
 2566 |   if (nrow(X) < 4L) stop("X must contain at least four rows for split selection", call. = FALSE)
 2567 |   if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
 2568 |     stop("alpha must be in (0, 1)", call. = FALSE)
 2569 |   }
 2570 |   if (length(calibration_fraction) != 1L || !is.numeric(calibration_fraction) ||
 2571 |       !is.finite(calibration_fraction) || calibration_fraction <= 0 || calibration_fraction >= 1) {
 2611 |   final_fit <- NULL
 2612 |   if (refit) {
 2613 |     final_fit <- msma(X = X, Z = Z, comp = selected$comp,
 2614 |       lambdaX = selected$lambdaX, muX = muX, structure.method = "soft",
 2615 |       gammaX = selected$gammaX, history.method = input$history_method,
 2616 |       niterS4 = input$niter, scaling = scaling, intseed = intseed)
 2617 |   }
 2618 |   result <- list(selected = selected, summary = summary, details = details,
 2619 |     fit = final_fit, settings = list(alpha = alpha, repeats = as.integer(repeats),
 2620 |       calibration_fraction = calibration_fraction, max_overlap = max_overlap,
 2621 |       muX = muX, history.method = input$history_method,
 2622 |       niterS4 = input$niter, scaling = scaling, intseed = intseed))
 2623 |   class(result) <- "s4pca_selection"
 2624 |   result
 2625 | }
 2626 | 
 2627 | 
```

## exclusive

```r
   51 | #' @param ceps a numeric scalar for the convergence condition of the algorithm
   52 | #' @param sprmethod a character, 
   53 | #' @param nneg a character 
   54 | #' @param con4spv a numeric vector
   55 | #' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
   56 | #' @param gammaX non-negative history-dependent penalty strength.
   57 | #' @param history.method one of \"sumabs\", \"any\", or \"count\".
   58 | #' @param niterS4 fixed number of structured PCA updates per component.
   59 | #' @param ... further arguments passed to or from other methods.
   60 | #' @return \item{dmode}{Which modes "PLS" or "PCA"}
   61 | #' @return \item{X}{Scaled X which has a list form.}
   62 | #' @return \item{Y}{Scaled Y which has a list form.}
   63 | #' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
  113 |   inXsup=NULL, inYsup=NULL, muX=0, muY=0, defmethod="canonical",
  114 |   scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  115 |   sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  116 |   nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  117 |   structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  118 |   history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
  119 | {
  120 |   structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  123 |   if(use_s4pca){
  124 |     if(identical(structure.method, "none")) structure.method = "soft"
  125 |     if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
  126 |     if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
  127 |     if(length(comp)!=1L) stop("Structured PCA requires a single component count", call.=FALSE)
  128 |     if(!is.null(lambdaY) || !is.null(lambdaXsup) || !is.null(lambdaYsup)) stop("Y-side and super-level penalties are not supported for structured PCA", call.=FALSE)
  139 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  140 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  141 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  142 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  143 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  144 |       niter=niterS4,seed=intseed,verbose=verbose)
  145 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  146 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  147 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  148 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  149 |     class(fit_s4)="s4pca"
  150 |     return(fit_s4)
  151 |   }
```

## soft

```r
   51 | #' @param ceps a numeric scalar for the convergence condition of the algorithm
   52 | #' @param sprmethod a character, 
   53 | #' @param nneg a character 
   54 | #' @param con4spv a numeric vector
   55 | #' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
   56 | #' @param gammaX non-negative history-dependent penalty strength.
   57 | #' @param history.method one of \"sumabs\", \"any\", or \"count\".
   58 | #' @param niterS4 fixed number of structured PCA updates per component.
   59 | #' @param ... further arguments passed to or from other methods.
   60 | #' @return \item{dmode}{Which modes "PLS" or "PCA"}
   61 | #' @return \item{X}{Scaled X which has a list form.}
   62 | #' @return \item{Y}{Scaled Y which has a list form.}
   63 | #' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
  113 |   inXsup=NULL, inYsup=NULL, muX=0, muY=0, defmethod="canonical",
  114 |   scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  115 |   sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  116 |   nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  117 |   structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  118 |   history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
  119 | {
  120 |   structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  123 |   if(use_s4pca){
  124 |     if(identical(structure.method, "none")) structure.method = "soft"
  125 |     if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
  126 |     if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
  127 |     if(length(comp)!=1L) stop("Structured PCA requires a single component count", call.=FALSE)
  128 |     if(!is.null(lambdaY) || !is.null(lambdaXsup) || !is.null(lambdaYsup)) stop("Y-side and super-level penalties are not supported for structured PCA", call.=FALSE)
  129 |     if(!identical(type,"lasso")) stop("Structured PCA supports type = \"lasso\" only", call.=FALSE)
  130 |     if(!is.null(inX) || !is.null(inY) || !is.null(inXsup) || !is.null(inYsup)) stop("Forced-in variables are not supported for structured PCA", call.=FALSE)
  131 |     if(!identical(sprmethod,"PCA") || !is.null(con4spv)) stop("Structured PCA cannot be combined with NMF, sNMF, CMP, or con4spv", call.=FALSE)
  132 |     if(!is.null(Z) && !is.null(dim(Z)) && ncol(as.matrix(Z))>1L) stop("Structured PCA accepts only vector or one-column Z", call.=FALSE)
 2512 |     fit <- msma(
 2513 |       X = X[train_idx, , drop = FALSE],
 2514 |       Z = if (is.null(Z)) NULL else Z[train_idx],
 2515 |       comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
 2516 |       structure.method = "soft", gammaX = grid$gammaX[g],
 2517 |       history.method = history.method, niterS4 = niterS4,
 2518 |       scaling = scaling, intseed = seed + g
 2519 |     )
 2520 |     pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
 2521 |     conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
 2522 |     q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
 2523 |     out[[g]] <- data.frame(
 2524 |       lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
 2539 | #'
 2540 | #' @param X numeric data matrix.
 2541 | #' @param Z optional numeric supervision vector or one-column matrix.
 2542 | #' @param lambdaX candidate sparsity parameters.
 2543 | #' @param gammaX candidate soft-structure parameters.
 2544 | #' @param comp candidate component counts.
 2545 | #' @param alpha miscoverage level in (0,1).
 2546 | #' @param repeats positive number of repeated splits.
 2547 | #' @param calibration_fraction fraction assigned to calibration in each split.
 2548 | #' @param max_overlap maximum admissible \code{overlap_selected}.
 2549 | #' @param muX supervision weight in [0,1].
 2550 | #' @param history.method one of \code{"sumabs"}, \code{"any"}, or \code{"count"}.
 2551 | #' @param niterS4 fixed S4PCA updates per component.
 2610 |   selected <- summary[eligible[ord[1]], , drop = FALSE]
 2611 |   final_fit <- NULL
 2612 |   if (refit) {
 2613 |     final_fit <- msma(X = X, Z = Z, comp = selected$comp,
 2614 |       lambdaX = selected$lambdaX, muX = muX, structure.method = "soft",
 2615 |       gammaX = selected$gammaX, history.method = input$history_method,
 2616 |       niterS4 = input$niter, scaling = scaling, intseed = intseed)
 2617 |   }
 2618 |   result <- list(selected = selected, summary = summary, details = details,
 2619 |     fit = final_fit, settings = list(alpha = alpha, repeats = as.integer(repeats),
 2620 |       calibration_fraction = calibration_fraction, max_overlap = max_overlap,
 2621 |       muX = muX, history.method = input$history_method,
 2622 |       niterS4 = input$niter, scaling = scaling, intseed = intseed))
```

## unsupported

```r
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  123 |   if(use_s4pca){
  124 |     if(identical(structure.method, "none")) structure.method = "soft"
  125 |     if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
  126 |     if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
  127 |     if(length(comp)!=1L) stop("Structured PCA requires a single component count", call.=FALSE)
  128 |     if(!is.null(lambdaY) || !is.null(lambdaXsup) || !is.null(lambdaYsup)) stop("Y-side and super-level penalties are not supported for structured PCA", call.=FALSE)
  129 |     if(!identical(type,"lasso")) stop("Structured PCA supports type = \"lasso\" only", call.=FALSE)
  130 |     if(!is.null(inX) || !is.null(inY) || !is.null(inXsup) || !is.null(inYsup)) stop("Forced-in variables are not supported for structured PCA", call.=FALSE)
  131 |     if(!identical(sprmethod,"PCA") || !is.null(con4spv)) stop("Structured PCA cannot be combined with NMF, sNMF, CMP, or con4spv", call.=FALSE)
  132 |     if(!is.null(Z) && !is.null(dim(Z)) && ncol(as.matrix(Z))>1L) stop("Structured PCA accepts only vector or one-column Z", call.=FALSE)
  133 |     lambda_s4 = if(is.null(lambdaX)) 0 else lambdaX
  134 |     if(length(lambda_s4)!=1L) stop("Structured PCA requires scalar lambdaX", call.=FALSE)
  135 |     X_input=X; X_center=rep(0,ncol(X)); X_scale=rep(1,ncol(X))
  136 |     if(isTRUE(scaling)){
  137 |       X_center=colMeans(X_input); X_scale=apply(sweep(X_input,2,X_center,"-"),2,stats::sd)
  138 |       X_scale[!is.finite(X_scale) | X_scale==0]=1
  139 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  140 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  141 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  142 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  143 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  144 |       niter=niterS4,seed=intseed,verbose=verbose)
  145 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  146 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  147 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  148 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  183 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  184 | comp = comp[1]
  185 | 
  186 | ##### Requirement #####
  187 | if(missing(X)) stop("X should be specified")
  188 | if(is.null(Y)){ 
  189 | dmode = "PCA"
  190 | Y = X
  191 | if(!is.null(lambdaY)) stop("lambdaY can not be specified")
  192 | }else
  193 | {
  194 | dmode = "PLS"
  195 | }
  196 | 
  197 | if(!inherits(X, "list")) X = list(X)
  198 | if(!inherits(Y, "list")) Y = list(Y)
  199 | 
  200 | if(!inherits(X, "list")) stop("X should be a list")
  201 | if(!inherits(Y, "list")) stop("Y should be a list")
  202 | 
  203 | Ynb = length(Y)
  204 | Xnb = length(X)
  205 | 
  206 | if(length(lambdaX) == 1) lambdaX = rep(lambdaX, Xnb)
  207 | if(length(lambdaY) == 1) lambdaY = rep(lambdaY, Ynb)
  208 | 
  209 | if(!is.null(lambdaX)){if(length(lambdaX) != Xnb) stop("lambdaX should be same length as the number of blocks of X")}
  210 | if(!is.null(lambdaY)){if(length(lambdaY) != Ynb) stop("lambdaY should be same length as the number of blocks of Y")}
  211 | 
  212 | if(!is.null(inX)){if(!inherits(inX, "list")) inX = list(inX)}
  213 | if(!is.null(inY)){if(!inherits(inY, "list")) inY = list(inY)}
  214 | 
  215 | Yps = unlist(lapply(Y, ncol))
  216 | Xps = unlist(lapply(X, ncol))
  217 | 
  218 | Yns = unlist(lapply(Y, nrow))
  219 | Xns = unlist(lapply(X, nrow))
  220 | 
  221 | if(length(unique(Yns)) == 1){Yns = unique(Yns)}else{stop("numbers of rows should be same among Y")}
  222 | if(length(unique(Xns)) == 1){Xns = unique(Xns)}else{stop("numbers of rows should be same among X")}
  223 | if(Yns == Xns){n = Xns}else{stop("numbers of rows should be same between X and Y")}
  224 | if(!is.null(Z)){ if(length(Z) != n) stop("length of Z should be same as row of X or Y")}
  225 | 
  226 | if(!is.null(inX)){
  227 | extest = unlist(lapply(1:Xnb, function(a) inX[[a]] %in% (1:Xps[a]) ))
  228 | if(!(all(extest))) stop("inX should be specified the column number of X")
  229 | }
  230 | if(!is.null(inY)){
  231 | extest = unlist(lapply(1:Ynb, function(a) inY[[a]] %in% (1:Yps[a]) ))
  232 | if(!(all(extest))) stop("inY should be specified the column number of Y")
  233 | }
  234 | 
  235 | if(muX < 0) stop("muX should be positive")
  236 | if(muY < 0) stop("muY should be positive")
  237 | 
  238 | ########## Start ##########
  239 | X = lapply(X, function(x) scale(x, scale=scaling))
  240 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  241 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  242 | 
  243 | Xscale = lapply(X, attributes)
  244 | Yscale = lapply(Y, attributes)
  472 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  473 | comp = comp[1]
  474 | 
  475 | ##### Requirement #####
  476 | if(missing(X)) stop("X should be specified")
  477 | if(is.null(Y)){ 
  478 | dmode = "PCA"
  479 | Y = X
  480 | if(!is.null(lambdaY)) stop("lambdaY can not be specified")
  481 | }else
  482 | {
  483 | dmode = "PLS"
  484 | }
  485 | 
  486 | if(!inherits(X, "list")) X = list(X)
  487 | if(!inherits(Y, "list")) Y = list(Y)
  488 | 
  489 | if(!inherits(X, "list")) stop("X should be a list")
  490 | if(!inherits(Y, "list")) stop("Y should be a list")
  491 | 
  492 | Ynb = length(Y)
  493 | Xnb = length(X)
  494 | 
  495 | Ybnames = names(Y)
  496 | Xbnames = names(X)
  497 | 
  498 | if(is.null(Ybnames)){Ybnames = paste("block", 1:Ynb, sep="")}
  500 | 
  501 | if(length(lambdaX) == 1) lambdaX = rep(lambdaX, Xnb)
  502 | if(length(lambdaY) == 1) lambdaY = rep(lambdaY, Ynb)
  503 | 
  504 | if(!is.null(lambdaX)){if(length(lambdaX) != Xnb) stop("lambdaX should be same length as the number of blocks of X")}
  505 | if(!is.null(lambdaY)){if(length(lambdaY) != Ynb) stop("lambdaY should be same length as the number of blocks of Y")}
  506 | 
  507 | if(!is.null(inX)){if(!inherits(inX, "list")) inX = list(inX)}
  508 | if(!is.null(inY)){if(!inherits(inY, "list")) inY = list(inY)}
  509 | 
  510 | Yps = unlist(lapply(Y, ncol))
  511 | Xps = unlist(lapply(X, ncol))
  512 | 
  513 | Yns = unlist(lapply(Y, nrow))
  514 | Xns = unlist(lapply(X, nrow))
  515 | 
  516 | if(length(unique(Yns)) == 1){Yns = unique(Yns)}else{stop("numbers of rows should be same among Y")}
  517 | if(length(unique(Xns)) == 1){Xns = unique(Xns)}else{stop("numbers of rows should be same among X")}
  518 | if(Yns == Xns){n = Xns}else{stop("numbers of rows should be same between X and Y")}
  519 | if(!is.null(Z)){ 
  520 | Z = cbind(Z)
  521 | if(nrow(Z) != n) stop("length of Z should be same as row of X or Y")
  522 | }
  523 | 
  524 | if(!is.null(inX)){
  525 | extest = unlist(lapply(1:Xnb, function(a) inX[[a]] %in% (1:Xps[a]) ))
  526 | if(!(all(extest))) stop("inX should be specified the column number of X")
  527 | }
  528 | if(!is.null(inY)){
  529 | extest = unlist(lapply(1:Ynb, function(a) inY[[a]] %in% (1:Yps[a]) ))
  530 | if(!(all(extest))) stop("inY should be specified the column number of Y")
  531 | }
  532 | 
  533 | if(muX < 0) stop("muX should be positive")
  534 | if(muY < 0) stop("muY should be positive")
  535 | 
  536 | ########## Start ##########
  537 | X = lapply(X, function(x) scale(x, scale=scaling))
  538 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  539 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  540 | 
  541 | Xscale = lapply(X, attributes)
  542 | Yscale = lapply(Y, attributes)
  918 | #'
  919 | plot.msma = function(x, v=c("weight", "score", "cpev")[1], axes = 1, axes2 = 1, block=c("block", "super")[1], plottype=c("bar", "scatter")[1], XY=c("X", "Y", "XY")[1], col=NULL, signflip=FALSE, xlim=NULL, ylim=NULL,...) {
  920 | 
  921 | ### ###
  922 | if(length(axes) > 2) stop("the length of axes should be less than 2.")
  923 | if(length(axes2) > 2) stop("the length of axes should be less than 2.")
  924 | 
  925 | if(x$comp[1] == 1){
  926 | axes = 1
  927 | if(plottype == "scatter") stop("number of components should be more than 2")
  928 | }
  929 | 
  930 | ### ###
  931 | if(XY == "XY"){
  932 | if(length(axes) != 1) stop("the length of axes should be 1.")
  933 | v1="s"
  934 | s1 = do.call(cbind, lapply(x[[paste0(v1, "sX")]][axes], function(x2) x2[, axes2,drop=FALSE]))
  935 | s2 = do.call(cbind, lapply(x[[paste0(v1, "sY")]][axes], function(x2) x2[, axes2,drop=FALSE]))
  936 | cor1 = cor.test(s1, s2)
  937 | m1 = round(max(abs(range(c(s1, s2)))))
  938 | if(is.null(xlim)) xlim=c(-m1, m1)
  939 | if(is.null(ylim)) ylim=c(-m1, m1)
  940 | plot(s1, s2, xlab="X score", ylab="Y score", main=paste("Component", axes[1]), sub=paste0("r=", round(cor1$estimate,2), " (p", ifelse(cor1$p.value>0.0001, "=", ""), format.pval(round(cor1$p.value, 4), eps=0.0001, scientific=FALSE), ")"), xlim=xlim, ylim=ylim, ...)
  971 | m1 = round(max(abs(range(tmpvar))))
  972 | if(is.null(xlim)) xlim=c(-m1, m1)
  973 | if(is.null(ylim)) ylim=c(-m1, m1)
  974 | if(plottype == "scatter"){
  975 | if(length(axes) != 2) stop("the length of axes should be 2.")
  976 | if(is.null(col)){ col1=1 }else{ col1=col}
  977 | plot(tmpvar, xlab=paste("Component", axes[1]), ylab=paste("Component", axes[2]), xlim=xlim, ylim=ylim, type=t1,  main=block, col=col1,...)
  978 | abline(h = 0, lty=2); abline(v = 0, lty=2);
  979 | if(v == "weight"){for(i in 1:nrow(tmpvar)) text(tmpvar[i,1], tmpvar[i,2], rownames(tmpvar)[i])}
  980 | }else if(plottype == "bar"){
  981 | #par(mfrow=c(1, length(axes)), mar = c(4,max(nchar(rownames(tmpvar))),4,4))
  982 | if(x$comp[2] > 1){ main1 = paste("(", "Nest", axes2, ")")}else{main1 = ""}
  983 | mar1 = ifelse(is.null(rownames(tmpvar)), 4, max(nchar(rownames(tmpvar))))
 1027 | #' test = predict(fit2, newX=X, newY=Y)
 1028 | #' 
 1029 | predict.msma = function(object, newX, newY=NULL,...){
 1030 | ##### Requirement #####
 1031 | if(missing(newX)) stop("newX should be specified")
 1032 | #if(missing(newY)) stop("newY should be specified")
 1033 | 
 1034 | if(is.null(newY) | (object$dmode == "PCA")) newY = newX
 1035 | 
 1036 | if(!inherits(newX, "list")) newX = list(newX)
 1037 | if(!inherits(newY, "list")) newY = list(newY)
 1038 | 
 1039 | if(!inherits(newX, "list")) stop("newX should be a list")
 1040 | if(!inherits(newY, "list")) stop("newY should be a list")
 1041 | 
 1042 | Ynb = length(newY)
 1043 | Xnb = length(newX)
 1044 | 
 1045 | ########## Start ##########
 1046 | #newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`, scale=object$Xscale[[x]]$`scaled:scale`))
 1047 | newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`))
 1048 | newX = lapply(newX, function(x) apply(x, 2, function(x2){ x2[is.na(x2)] = 0; x2}))
 1115 | #'cv2
 1116 | #'
 1117 | cvmsma = function(X, Y=NULL, Z=NULL, comp=1, lambdaX, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, nfold=5, seed=1, intseed=1, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1], con4spv=NULL){
 1118 | ##### Requirement #####
 1119 | if(missing(X)) stop("X should be specified")
 1120 | if(is.null(Y)){ 
 1121 | dmode = "PCA"
 1122 | Y = X
 1123 | if(!is.null(lambdaY)) stop("lambdaY can not be specified")
 1124 | }else{
 1125 | dmode = "PLS"
 1126 | }
 1127 | 
 1128 | if(!inherits(X, "list")) X = list(X)
 1129 | if(!inherits(Y, "list")) Y = list(Y)
 1130 | if(!inherits(X, "list")) stop("X should be a list")
 1131 | if(!inherits(Y, "list")) stop("Y should be a list")
 1132 | 
 1133 | Ynb = length(Y)
 1134 | Xnb = length(X)
 1135 | 
 1136 | if(length(lambdaX) == 1) lambdaX = rep(lambdaX, Xnb)
 1137 | if(length(lambdaY) == 1) lambdaY = rep(lambdaY, Ynb)
 1138 | 
 1139 | if(!is.null(lambdaX)){if(length(lambdaX) != Xnb) stop("lambdaX should be same length as the number of blocks of X")}
 1140 | if(!is.null(lambdaY)){if(length(lambdaY) != Ynb) stop("lambdaY should be same length as the number of blocks of Y")}
 1141 | 
 1142 | if(!is.null(inX)){if(!inherits(inX, "list")) inX = list(inX)}
 1143 | if(!is.null(inY)){if(!inherits(inY, "list")) inY = list(inY)}
 1144 | 
 1145 | Yps = unlist(lapply(Y, ncol))
 1146 | Xps = unlist(lapply(X, ncol))
 1147 | 
 1148 | if(!is.null(inX)){
 1149 | extest = unlist(lapply(1:Xnb, function(a) inX[[a]] %in% (1:Xps[a]) ))
 1150 | if(!(all(extest))) stop("inX should be specified the column number of X")
 1151 | }
 1152 | if(!is.null(inY)){
 1153 | extest = unlist(lapply(1:Ynb, function(a) inY[[a]] %in% (1:Yps[a]) ))
 1154 | if(!(all(extest))) stop("inY should be specified the column number of Y")
 1155 | }
 1156 | 
 1157 | ########## Start ##########
 1158 | ##### Set Up #####
 1159 | n = nrow(Y[[1]])
 1160 | id = 1:n
 1161 | divid = cut(id, c(-Inf, floor(quantile(id, seq(1/nfold, 1-1/nfold, length=(nfold-1)))), Inf), labels = FALSE)
 1162 | set.seed(seed); fold = sample(divid)
 1410 | #' summary(fit4)
 1411 | #'
 1412 | regparasearch = function(X, Y=NULL, Z=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, comp=1, nfold=5, maxrep=3, minpct=0, maxpct=1, criterion=c("CV", "BIC")[1], whichselect=NULL, homo=NULL, intseed=1, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1]){
 1413 | ##### Requirement #####
 1414 | if(missing(X)) stop("X should be specified")
 1415 | if(minpct>=1 | minpct<0) stop("minpct should be 0 <= minpct < 1")
 1416 | if(maxpct>1 | maxpct<=0) stop("maxpct should be 0 < maxpct <= 1")
 1417 | 
 1418 | abnames = c("X", "Y", "Xsup", "Ysup")
 1419 | if(length(comp)<2) comp[2] = 1
 1420 | 
 1421 | ##### Candidates for Regularized Parameters #####
 1422 | intfit = msma(X, Y, Z, comp=1, lambdaX=rep(NULL, length(X)), lambdaY=rep(NULL, length(Y)), lambdaXsup=NULL, lambdaYsup=NULL, eta, type, inX, inY, inXsup=NULL, inYsup=NULL, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg)
 1423 | 
 1424 | intweight = list(X=intfit$wbX, Y=intfit$wbY, Xsup=intfit$wsX, Ysup=intfit$wsY)
 1673 | if(any(is.null(lambdaYsup))){ lambdaYsup = 0}
 1674 | 
 1675 | if(is.null(Z)) Z = rep(0, nrow(X[[1]]))
 1676 | muXY = 1 - muX - muY
 1677 | if(muXY < 0) stop("muX or muY should be smaller values to be 0 <= 1 - muX - muY <= 1.")
 1678 | 
 1679 | #svd0 = svd(do.call(cbind, Y)); ssY = normvec(svd0$u[,1])
 1680 | #ssY = normvec(rep(1, nrow(Y[[1]])))
 1681 | set.seed(intseed); ssY = normvec(rnorm(nrow(X[[1]])))
 1682 | 
 1683 | if(length(X) == 1){wsX = 1}else{ set.seed(intseed); wsX = normvec(rnorm(length(X)))}
 1684 | if(length(Y) == 1){wsY = 1}else{ set.seed(intseed); wsY = normvec(rnorm(length(Y)))}
 1685 | 
 1743 | if(is.null(con4spv)){ con4spv = rep(1, ncol(Z))}
 1744 | con4spv=con4spv/sum(abs(con4spv))
 1745 | 
 1746 | muXY = 1 - muX - muY
 1747 | if(muXY < 0) stop("muX or muY should be smaller values to be 0 <= 1 - muX - muY <= 1.")
 1748 | 
 1749 | #svd0 = svd(do.call(cbind, Y)); ssY = normvec(svd0$u[,1])
 1750 | #ssY = normvec(rep(1, nrow(Y[[1]])))
 1751 | set.seed(intseed); ssY = normvec(rnorm(nrow(X[[1]])))
 1752 | 
 1753 | if(length(X) == 1){wsX = 1}else{ set.seed(intseed); wsX = normvec(rnorm(length(X)))}
 1754 | if(length(Y) == 1){wsY = 1}else{ set.seed(intseed); wsY = normvec(rnorm(length(Y)))}
 1755 | 
 1818 | if(type=="hard"){
 1819 | x[x1] * (abs(x[x1]) > lam[x1])
 1820 | }else
 1821 | if(type=="scad"){
 1822 | if(eta==2) stop("eta=2 not available for scad")
 1823 | ifelse(abs(x[x1]) > 2*lam[x1], ifelse(abs(x[x1]) <= eta*lam[x1], ((eta-1)*x[x1] - sign(x[x1])*eta*lam[x1])/(eta-2), x[x1]), 
 1824 | sign(x[x1]) * max(c(abs(x[x1]) - lam[x1], 0)))
 1825 | }else
 1826 | if(type=="mcp"){
 1827 | if(eta==1) stop("eta=1 not available for mcp")
 1828 | ifelse(abs(x[x1]) <= eta*lam[x1], (sign(x[x1]) * max(c(abs(x[x1]) - lam[x1], 0)))/(1-1/eta), x[x1])
 1829 | }
 1830 | })
 1831 | sout
 1832 | }
 1833 | 
 1834 | #' @rdname msma-internal
 1835 | cand4lambda = function(x, len){
 1886 | #' @return \item{X}{Simulated X which has a list form}
 1887 | #' @return \item{Y}{Simulated Y which has a list form}
 1888 | simdata = function(n = 100, rho = 0.8, Yps = c(100, 120, 150), Xps = 500, seed=1){
 1889 | 
 1890 | if(length(n) > 1) stop("n should be scalar")
 1891 | if(length(rho) > 1) stop("rho should be scalar")
 1892 | 
 1893 | #require("mvtnorm")
 1894 | 
 1895 | set.seed(seed)
 1896 | Y = lapply(Yps, function(p){
 1897 | sigma = matrix(rho, p, p)
 1898 | diag(sigma) = 1
 1899 | rmnorm(n, rep(0, p), sigma)
 2166 | #' @rdname msma-internal
 2167 | rmnorm <- function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)), method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE, checkSymmetry = TRUE)
 2168 | {
 2169 | if (checkSymmetry && !isSymmetric(sigma, tol = sqrt(.Machine$double.eps), check.attributes = FALSE)) {
 2170 | stop("sigma must be a symmetric matrix")
 2171 | }
 2172 | if (length(mean) != nrow(sigma))
 2173 | stop("mean and sigma have non-conforming size")
 2174 | 
 2175 | method <- match.arg(method)
 2176 | 
 2177 | R <- if(method == "eigen") {
 2178 | ev <- eigen(sigma, symmetric = TRUE)
 2179 | if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))){
 2180 |     warning("sigma is numerically not positive semidefinite")
 2181 | }
 2255 |       sign(x[j]) * max(c(abs(x[j]) - lam[j], 0))
 2256 |     } else if (type == "hard") {
 2257 |       x[j] * (abs(x[j]) > lam[j])
 2258 |     } else if (type == "scad") {
 2259 |       if (eta == 2) stop("eta=2 not available for scad")
 2260 |       ifelse(abs(x[j]) > 2 * lam[j],
 2261 |         ifelse(abs(x[j]) <= eta * lam[j],
 2262 |           ((eta - 1) * x[j] - sign(x[j]) * eta * lam[j]) / (eta - 2), x[j]),
 2263 |         sign(x[j]) * max(c(abs(x[j]) - lam[j], 0)))
 2264 |     } else if (type == "mcp") {
 2265 |       if (eta == 1) stop("eta=1 not available for mcp")
 2266 |       ifelse(abs(x[j]) <= eta * lam[j],
 2267 |         sign(x[j]) * max(c(abs(x[j]) - lam[j], 0)) / (1 - 1 / eta), x[j])
 2268 |     } else {
 2269 |       stop("unknown sparse penalty")
 2270 |     }
 2271 |   })
 2272 | }
 2273 | 
 2274 | .s4pca_project_reference <- function(x, y) {
 2275 |   if (is.null(ncol(y))) {
 2276 |     c(x %*% cbind(y) / drop(crossprod(y)))
 2277 |   } else {
 2362 |                                    mu = 1, hard_exclusion = FALSE,
 2363 |                                    niter = 30, seed = NULL) {
 2364 |   if (!is.matrix(X)) X <- as.matrix(X)
 2365 |   if (!is.numeric(X) || length(dim(X)) != 2L) {
 2366 |     stop("X must be a numeric matrix", call. = FALSE)
 2367 |   }
 2368 |   if (nrow(X) < 2L || ncol(X) < 1L) {
 2369 |     stop("X must contain at least two rows and one column", call. = FALSE)
 2370 |   }
 2371 |   if (any(!is.finite(X))) {
 2372 |     stop("X must not contain NA, NaN, or Inf", call. = FALSE)
 2373 |   }
 2374 |   if (!is.null(Z)) {
 2375 |     if (is.matrix(Z) && ncol(Z) == 1L) Z <- drop(Z)
 2376 |     if (!is.numeric(Z) || !is.null(dim(Z))) {
 2377 |       stop("Z must be a numeric vector or a one-column numeric matrix", call. = FALSE)
 2378 |     }
 2379 |     if (length(Z) != nrow(X)) {
 2380 |       stop("length(Z) must equal nrow(X)", call. = FALSE)
 2381 |     }
 2382 |     if (any(!is.finite(Z))) {
 2383 |       stop("Z must not contain NA, NaN, or Inf", call. = FALSE)
 2384 |     }
 2385 |   }
 2386 |   scalar_integer <- function(x, name, lower) {
 2387 |     if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < lower || x != as.integer(x)) {
 2388 |       stop(name, " must be a single integer >= ", lower, call. = FALSE)
 2389 |     }
 2390 |     as.integer(x)
 2391 |   }
 2392 |   scalar_nonnegative <- function(x, name) {
 2393 |     if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 0) {
 2394 |       stop(name, " must be a single non-negative finite number", call. = FALSE)
 2395 |     }
 2396 |     as.numeric(x)
 2397 |   }
 2398 |   comp <- scalar_integer(comp, "comp", 1L)
 2399 |   niter <- scalar_integer(niter, "niter", 1L)
 2400 |   lambda <- scalar_nonnegative(lambda, "lambda")
 2401 |   gamma <- scalar_nonnegative(gamma, "gamma")
 2402 |   if (length(mu) != 1L || !is.numeric(mu) || !is.finite(mu) || mu < 0 || mu > 1) {
 2403 |     stop("mu must be a single finite number in [0, 1]", call. = FALSE)
 2404 |   }
 2405 |   history_method <- match.arg(history_method, c("sumabs", "any", "count"))
 2406 |   if (length(hard_exclusion) != 1L || !is.logical(hard_exclusion) || is.na(hard_exclusion)) {
 2407 |     stop("hard_exclusion must be TRUE or FALSE", call. = FALSE)
 2408 |   }
 2409 |   if (!is.null(seed)) seed <- scalar_integer(seed, "seed", 0L)
 2410 |   list(X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
 2411 |        history_method = history_method, mu = as.numeric(mu),
 2412 |        hard_exclusion = hard_exclusion, niter = niter, seed = seed)
 2413 | }
 2414 | 
 2415 | .s4pca_fit <- function(X, Z = NULL, comp = 6, lambda = 0.02, gamma = 0,
 2458 | #' @param scaled logical; if TRUE, return reconstruction on the standardized scale.
 2459 | #' @return a reconstructed numeric matrix.
 2460 | #' @export
 2461 | s4pca_reconstruct <- function(object, newdata = NULL, scaled = FALSE) {
 2462 |   if (!inherits(object, "s4pca")) stop("object must inherit from 's4pca'", call. = FALSE)
 2463 |   if (length(scaled) != 1L || !is.logical(scaled) || is.na(scaled)) {
 2464 |     stop("scaled must be TRUE or FALSE", call. = FALSE)
 2465 |   }
 2466 |   B <- object$reconstruction_loadings
 2467 |   if (is.null(B)) stop("object does not contain reconstruction coefficients", call. = FALSE)
 2468 |   if (is.null(newdata)) {
 2469 |     Xhat <- object$S %*% B
 2470 |   } else {
 2471 |     X <- as.matrix(newdata)
 2472 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2473 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2474 |     }
 2475 |     if (isTRUE(object$scaling)) {
 2476 |       X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
 2477 |     }
 2478 |     Xhat <- (X %*% object$W) %*% B
 2479 |   }
 2480 |   colnames(Xhat) <- rownames(object$W)
 2481 |   if (!scaled && isTRUE(object$scaling)) {
 2485 | }
 2486 | 
 2487 | .s4pca_grid <- function(lambdaX, gammaX, comp) {
 2488 |   if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
 2489 |     stop("lambdaX must contain non-negative finite values", call. = FALSE)
 2490 |   }
 2491 |   if (!is.numeric(gammaX) || !length(gammaX) || any(!is.finite(gammaX)) || any(gammaX < 0)) {
 2492 |     stop("gammaX must contain non-negative finite values", call. = FALSE)
 2493 |   }
 2494 |   if (!is.numeric(comp) || !length(comp) || any(!is.finite(comp)) || any(comp < 1) || any(comp != as.integer(comp))) {
 2495 |     stop("comp must contain positive integers", call. = FALSE)
 2496 |   }
 2497 |   unique(expand.grid(lambdaX = as.numeric(lambdaX), gammaX = as.numeric(gammaX),
 2498 |                      comp = as.integer(comp), KEEP.OUT.ATTRS = FALSE,
 2499 |                      stringsAsFactors = FALSE))
 2500 | }
 2501 | 
 2502 | .s4pca_one_split <- function(X, Z, grid, calibration_fraction, alpha,
 2503 |                              max_overlap, muX, history.method, niterS4,
 2562 |   input <- .s4pca_validate_inputs(X = X, Z = Z, comp = 1, lambda = 0,
 2563 |     gamma = 0, history_method = history.method, mu = muX,
 2564 |     hard_exclusion = FALSE, niter = niterS4, seed = intseed)
 2565 |   X <- input$X; Z <- input$Z
 2566 |   if (nrow(X) < 4L) stop("X must contain at least four rows for split selection", call. = FALSE)
 2567 |   if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
 2568 |     stop("alpha must be in (0, 1)", call. = FALSE)
 2569 |   }
 2570 |   if (length(calibration_fraction) != 1L || !is.numeric(calibration_fraction) ||
 2571 |       !is.finite(calibration_fraction) || calibration_fraction <= 0 || calibration_fraction >= 1) {
 2572 |     stop("calibration_fraction must be in (0, 1)", call. = FALSE)
 2573 |   }
 2574 |   if (length(max_overlap) != 1L || !is.numeric(max_overlap) ||
 2575 |       !is.finite(max_overlap) || max_overlap < 0 || max_overlap > 1) {
 2576 |     stop("max_overlap must be in [0, 1]", call. = FALSE)
 2577 |   }
 2578 |   if (length(repeats) != 1L || !is.numeric(repeats) || !is.finite(repeats) ||
 2579 |       repeats < 1 || repeats != as.integer(repeats)) {
 2580 |     stop("repeats must be a positive integer", call. = FALSE)
 2581 |   }
 2582 |   if (length(refit) != 1L || !is.logical(refit) || is.na(refit)) {
 2583 |     stop("refit must be TRUE or FALSE", call. = FALSE)
 2584 |   }
 2585 |   grid <- .s4pca_grid(lambdaX, gammaX, comp)
 2586 |   split_results <- lapply(seq_len(as.integer(repeats)), function(b) {
 2587 |     z <- .s4pca_one_split(X, Z, grid, calibration_fraction, alpha, max_overlap,
 2588 |       muX, input$history_method, input$niter, scaling, intseed + 1000L * b)
 2589 |     z[["repeat"]] <- b
 2590 |     z
 2591 |   })
```

## scaling

```r
   43 | #' @param inYsup a (list of) numeric vector to specify the blocks of Y which are always in the model. 
   44 | #' @param muX a numeric scalar for the weight of X for the supervised case. 0 <= muX <= 1.
   45 | #' @param muY a numeric scalar for the weight of Y for the supervised case. 0 <= muY <= 1. 
   46 | #' @param defmethod a character representing the deflation method. This version has only the choice "canonical." 
   47 | #' @param scaling a logical, indicating whether or not data scaling is performed. The default is TRUE.
   48 | #' @param verbose information
   49 | #' @param intseed seed number for the random number in the parameter estimation algorithm.
   50 | #' @param x an object of class "\code{msma}", usually, a result of a call to \code{\link{msma}}
   51 | #' @param ceps a numeric scalar for the convergence condition of the algorithm
   52 | #' @param sprmethod a character, 
   53 | #' @param nneg a character 
   54 | #' @param con4spv a numeric vector
   55 | #' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
  110 | # The exact version 3.1 engine is used unless a version 3.2 feature is requested.
  111 | msma.default = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL,
  112 |   lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL,
  113 |   inXsup=NULL, inYsup=NULL, muX=0, muY=0, defmethod="canonical",
  114 |   scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  115 |   sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  116 |   nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  117 |   structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  118 |   history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
  119 | {
  120 |   structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  132 |     if(!is.null(Z) && !is.null(dim(Z)) && ncol(as.matrix(Z))>1L) stop("Structured PCA accepts only vector or one-column Z", call.=FALSE)
  133 |     lambda_s4 = if(is.null(lambdaX)) 0 else lambdaX
  134 |     if(length(lambda_s4)!=1L) stop("Structured PCA requires scalar lambdaX", call.=FALSE)
  135 |     X_input=X; X_center=rep(0,ncol(X)); X_scale=rep(1,ncol(X))
  136 |     if(isTRUE(scaling)){
  137 |       X_center=colMeans(X_input); X_scale=apply(sweep(X_input,2,X_center,"-"),2,stats::sd)
  138 |       X_scale[!is.finite(X_scale) | X_scale==0]=1
  139 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  140 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  141 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  142 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  143 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  144 |       niter=niterS4,seed=intseed,verbose=verbose)
  145 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  146 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  147 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  148 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  149 |     class(fit_s4)="s4pca"
  150 |     return(fit_s4)
  151 |   }
  152 |   z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  153 |   z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  154 |   z_is_multivariate = z_ncol > 1L
  155 |   use_extended = !identical(sprmethod, "PCA") || z_is_multivariate || !is.null(con4spv)
  161 |     return(.msma_default_legacy(
  162 |       X=X, Y=Y, Z=Z_legacy, comp=comp, lambdaX=lambdaX, lambdaY=lambdaY,
  163 |       lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type,
  164 |       inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX=muX, muY=muY,
  165 |       defmethod=defmethod, scaling=scaling, verbose=verbose,
  166 |       intseed=intseed, ceps=ceps, ...
  167 |     ))
  168 |   }
  169 | 
  170 |   .msma_default_extended(
  171 |     X=X, Y=Y, Z=Z, comp=comp, lambdaX=lambdaX, lambdaY=lambdaY,
  172 |     lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type,
  173 |     inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX=muX, muY=muY,
  174 |     defmethod=defmethod, scaling=scaling, verbose=verbose,
  175 |     intseed=intseed, ceps=ceps, sprmethod=sprmethod, nneg=nneg,
  176 |     con4spv=con4spv, ...
  177 |   )
  178 | }
  179 | 
  180 | .msma_default_legacy = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, defmethod = "canonical", scaling = TRUE, verbose=FALSE, intseed=1, ceps=0.0001, ...)
  181 | {
  182 | # number of nested componets
  183 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  184 | comp = comp[1]
  185 | 
  186 | ##### Requirement #####
  187 | if(missing(X)) stop("X should be specified")
  188 | if(is.null(Y)){ 
  235 | if(muX < 0) stop("muX should be positive")
  236 | if(muY < 0) stop("muY should be positive")
  237 | 
  238 | ########## Start ##########
  239 | X = lapply(X, function(x) scale(x, scale=scaling))
  240 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  241 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  242 | 
  243 | Xscale = lapply(X, attributes)
  244 | Yscale = lapply(Y, attributes)
  245 | 
  246 | if(scaling){
  247 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  248 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  249 | }
  250 | 
  251 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  252 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  253 | 
  254 | tvarX = lapply(X, function(x) sum(x^2))
  465 | class(out) = "msma"
  466 | out
  467 | }
  468 | 
  469 | .msma_default_extended = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, defmethod = "canonical", scaling = TRUE, verbose=FALSE, intseed=1, ceps=0.0001, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1], con4spv=NULL,...)
  470 | {
  471 | # number of nested componets
  472 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  473 | comp = comp[1]
  474 | 
  475 | ##### Requirement #####
  476 | if(missing(X)) stop("X should be specified")
  477 | if(is.null(Y)){ 
  533 | if(muX < 0) stop("muX should be positive")
  534 | if(muY < 0) stop("muY should be positive")
  535 | 
  536 | ########## Start ##########
  537 | X = lapply(X, function(x) scale(x, scale=scaling))
  538 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  539 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  540 | 
  541 | Xscale = lapply(X, attributes)
  542 | Yscale = lapply(Y, attributes)
  543 | 
  544 | if(scaling){
  545 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  546 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  547 | }
  548 | 
  549 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  550 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  551 | 
  552 | tvarX = lapply(X, function(x) sum(x^2))
 2471 |     X <- as.matrix(newdata)
 2472 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2473 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2474 |     }
 2475 |     if (isTRUE(object$scaling)) {
 2476 |       X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
 2477 |     }
 2478 |     Xhat <- (X %*% object$W) %*% B
 2479 |   }
 2480 |   colnames(Xhat) <- rownames(object$W)
 2481 |   if (!scaled && isTRUE(object$scaling)) {
 2482 |     Xhat <- sweep(sweep(Xhat, 2, object$Xscale, "*"), 2, object$Xcenter, "+")
 2483 |   }
 2484 |   Xhat
 2485 | }
 2486 | 
 2487 | .s4pca_grid <- function(lambdaX, gammaX, comp) {
 2488 |   if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
 2489 |     stop("lambdaX must contain non-negative finite values", call. = FALSE)
 2500 | }
 2501 | 
 2502 | .s4pca_one_split <- function(X, Z, grid, calibration_fraction, alpha,
 2503 |                              max_overlap, muX, history.method, niterS4,
 2504 |                              scaling, seed) {
 2505 |   set.seed(seed)
 2506 |   n <- nrow(X)
 2507 |   n_cal <- max(2L, min(n - 2L, as.integer(ceiling(n * calibration_fraction))))
 2508 |   cal_idx <- sort(sample.int(n, n_cal, replace = FALSE))
 2509 |   train_idx <- setdiff(seq_len(n), cal_idx)
 2510 |   out <- vector("list", nrow(grid))
 2511 |   for (g in seq_len(nrow(grid))) {
 2512 |     fit <- msma(
 2514 |       Z = if (is.null(Z)) NULL else Z[train_idx],
 2515 |       comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
 2516 |       structure.method = "soft", gammaX = grid$gammaX[g],
 2517 |       history.method = history.method, niterS4 = niterS4,
 2518 |       scaling = scaling, intseed = seed + g
 2519 |     )
 2520 |     pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
 2521 |     conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
 2522 |     q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
 2523 |     out[[g]] <- data.frame(
 2524 |       lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
 2525 |       score = q, overlap_all = fit$overlap_all,
 2526 |       overlap_selected = fit$overlap_selected,
 2548 | #' @param max_overlap maximum admissible \code{overlap_selected}.
 2549 | #' @param muX supervision weight in [0,1].
 2550 | #' @param history.method one of \code{"sumabs"}, \code{"any"}, or \code{"count"}.
 2551 | #' @param niterS4 fixed S4PCA updates per component.
 2552 | #' @param scaling logical data scaling indicator.
 2553 | #' @param intseed random seed.
 2554 | #' @param refit logical; refit the selected model on all observations.
 2555 | #' @return an object of class \code{s4pca_selection}.
 2556 | #' @export
 2557 | s4pca_conformal_select <- function(
 2558 |     X, Z = NULL, lambdaX = c(0.05, 0.1, 0.2), gammaX = c(0, 0.05, 0.1),
 2559 |     comp = 2:4, alpha = 0.1, repeats = 5, calibration_fraction = 0.25,
 2560 |     max_overlap = 1, muX = 1, history.method = "sumabs", niterS4 = 30,
 2561 |     scaling = TRUE, intseed = 1, refit = TRUE) {
 2562 |   input <- .s4pca_validate_inputs(X = X, Z = Z, comp = 1, lambda = 0,
 2563 |     gamma = 0, history_method = history.method, mu = muX,
 2564 |     hard_exclusion = FALSE, niter = niterS4, seed = intseed)
 2565 |   X <- input$X; Z <- input$Z
 2566 |   if (nrow(X) < 4L) stop("X must contain at least four rows for split selection", call. = FALSE)
 2567 |   if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
 2568 |     stop("alpha must be in (0, 1)", call. = FALSE)
 2569 |   }
 2584 |   }
 2585 |   grid <- .s4pca_grid(lambdaX, gammaX, comp)
 2586 |   split_results <- lapply(seq_len(as.integer(repeats)), function(b) {
 2587 |     z <- .s4pca_one_split(X, Z, grid, calibration_fraction, alpha, max_overlap,
 2588 |       muX, input$history_method, input$niter, scaling, intseed + 1000L * b)
 2589 |     z[["repeat"]] <- b
 2590 |     z
 2591 |   })
 2592 |   details <- do.call(rbind, split_results)
 2593 |   key <- interaction(details$lambdaX, details$gammaX, details$comp, drop = TRUE)
 2594 |   groups <- split(details, key)
 2595 |   summary <- do.call(rbind, lapply(groups, function(d) data.frame(
 2596 |     lambdaX = d$lambdaX[1], gammaX = d$gammaX[1], comp = d$comp[1],
 2612 |   if (refit) {
 2613 |     final_fit <- msma(X = X, Z = Z, comp = selected$comp,
 2614 |       lambdaX = selected$lambdaX, muX = muX, structure.method = "soft",
 2615 |       gammaX = selected$gammaX, history.method = input$history_method,
 2616 |       niterS4 = input$niter, scaling = scaling, intseed = intseed)
 2617 |   }
 2618 |   result <- list(selected = selected, summary = summary, details = details,
 2619 |     fit = final_fit, settings = list(alpha = alpha, repeats = as.integer(repeats),
 2620 |       calibration_fraction = calibration_fraction, max_overlap = max_overlap,
 2621 |       muX = muX, history.method = input$history_method,
 2622 |       niterS4 = input$niter, scaling = scaling, intseed = intseed))
 2623 |   class(result) <- "s4pca_selection"
 2624 |   result
 2625 | }
 2626 | 
 2627 | 
```

## s4 iterations

```r
   54 | #' @param con4spv a numeric vector
   55 | #' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
   56 | #' @param gammaX non-negative history-dependent penalty strength.
   57 | #' @param history.method one of \"sumabs\", \"any\", or \"count\".
   58 | #' @param niterS4 fixed number of structured PCA updates per component.
   59 | #' @param ... further arguments passed to or from other methods.
   60 | #' @return \item{dmode}{Which modes "PLS" or "PCA"}
   61 | #' @return \item{X}{Scaled X which has a list form.}
   62 | #' @return \item{Y}{Scaled Y which has a list form.}
   63 | #' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
   64 | #' @return \item{Yscale}{Scaling information for Y. The means and standard deviations for each block of Y are returned.}
   65 | #' @return \item{comp}{the number of componets}
   66 | #' @return \item{wbX}{block loading for X}
  114 |   scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  115 |   sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  116 |   nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  117 |   structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  118 |   history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
  119 | {
  120 |   structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  121 |   use_s4pca = !identical(structure.method, "none") ||
  122 |     (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  123 |   if(use_s4pca){
  124 |     if(identical(structure.method, "none")) structure.method = "soft"
  125 |     if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
  126 |     if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
  140 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  141 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  142 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  143 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  144 |       niter=niterS4,seed=intseed,verbose=verbose)
  145 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  146 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  147 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  148 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  149 |     class(fit_s4)="s4pca"
  150 |     return(fit_s4)
  151 |   }
  152 |   z_is_matrix = !is.null(Z) && !is.null(dim(Z))
 2499 |                      stringsAsFactors = FALSE))
 2500 | }
 2501 | 
 2502 | .s4pca_one_split <- function(X, Z, grid, calibration_fraction, alpha,
 2503 |                              max_overlap, muX, history.method, niterS4,
 2504 |                              scaling, seed) {
 2505 |   set.seed(seed)
 2506 |   n <- nrow(X)
 2507 |   n_cal <- max(2L, min(n - 2L, as.integer(ceiling(n * calibration_fraction))))
 2508 |   cal_idx <- sort(sample.int(n, n_cal, replace = FALSE))
 2509 |   train_idx <- setdiff(seq_len(n), cal_idx)
 2510 |   out <- vector("list", nrow(grid))
 2511 |   for (g in seq_len(nrow(grid))) {
 2513 |       X = X[train_idx, , drop = FALSE],
 2514 |       Z = if (is.null(Z)) NULL else Z[train_idx],
 2515 |       comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
 2516 |       structure.method = "soft", gammaX = grid$gammaX[g],
 2517 |       history.method = history.method, niterS4 = niterS4,
 2518 |       scaling = scaling, intseed = seed + g
 2519 |     )
 2520 |     pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
 2521 |     conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
 2522 |     q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
 2523 |     out[[g]] <- data.frame(
 2524 |       lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
 2525 |       score = q, overlap_all = fit$overlap_all,
 2547 | #' @param calibration_fraction fraction assigned to calibration in each split.
 2548 | #' @param max_overlap maximum admissible \code{overlap_selected}.
 2549 | #' @param muX supervision weight in [0,1].
 2550 | #' @param history.method one of \code{"sumabs"}, \code{"any"}, or \code{"count"}.
 2551 | #' @param niterS4 fixed S4PCA updates per component.
 2552 | #' @param scaling logical data scaling indicator.
 2553 | #' @param intseed random seed.
 2554 | #' @param refit logical; refit the selected model on all observations.
 2555 | #' @return an object of class \code{s4pca_selection}.
 2556 | #' @export
 2557 | s4pca_conformal_select <- function(
 2558 |     X, Z = NULL, lambdaX = c(0.05, 0.1, 0.2), gammaX = c(0, 0.05, 0.1),
 2559 |     comp = 2:4, alpha = 0.1, repeats = 5, calibration_fraction = 0.25,
 2560 |     max_overlap = 1, muX = 1, history.method = "sumabs", niterS4 = 30,
 2561 |     scaling = TRUE, intseed = 1, refit = TRUE) {
 2562 |   input <- .s4pca_validate_inputs(X = X, Z = Z, comp = 1, lambda = 0,
 2563 |     gamma = 0, history_method = history.method, mu = muX,
 2564 |     hard_exclusion = FALSE, niter = niterS4, seed = intseed)
 2565 |   X <- input$X; Z <- input$Z
 2566 |   if (nrow(X) < 4L) stop("X must contain at least four rows for split selection", call. = FALSE)
 2567 |   if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
 2568 |     stop("alpha must be in (0, 1)", call. = FALSE)
 2569 |   }
 2570 |   if (length(calibration_fraction) != 1L || !is.numeric(calibration_fraction) ||
 2571 |       !is.finite(calibration_fraction) || calibration_fraction <= 0 || calibration_fraction >= 1) {
 2572 |     stop("calibration_fraction must be in (0, 1)", call. = FALSE)
 2612 |   if (refit) {
 2613 |     final_fit <- msma(X = X, Z = Z, comp = selected$comp,
 2614 |       lambdaX = selected$lambdaX, muX = muX, structure.method = "soft",
 2615 |       gammaX = selected$gammaX, history.method = input$history_method,
 2616 |       niterS4 = input$niter, scaling = scaling, intseed = intseed)
 2617 |   }
 2618 |   result <- list(selected = selected, summary = summary, details = details,
 2619 |     fit = final_fit, settings = list(alpha = alpha, repeats = as.integer(repeats),
 2620 |       calibration_fraction = calibration_fraction, max_overlap = max_overlap,
 2621 |       muX = muX, history.method = input$history_method,
 2622 |       niterS4 = input$niter, scaling = scaling, intseed = intseed))
 2623 |   class(result) <- "s4pca_selection"
 2624 |   result
 2625 | }
 2626 | 
 2627 | 
```

## Required invariants for the refactor

1. Calls without structured arguments remain on the Version 4 legacy path.
2. `structure.method = "none"` and `gammaX = 0` preserve Version 4 results.
3. Positive `gammaX` retains the Version 4 implicit soft-structure opt-in behavior.
4. `structure.method = "soft"` uses the validated S4PCA engine.
5. `structure.method = "exclusive"` preserves hard exclusion.
6. Unsupported combinations fail before estimation with the same error class and message where tested.
7. Public result names and values remain unchanged.
8. Fixed-seed results remain reproducible.

## Proposed dispatcher contract

```r
.resolve_msma_engine <- function(structure.method, gammaX, ...) {
  # Return one of: legacy, soft, exclusive.
  # This resolver must contain policy only and perform no numerical estimation.
}

msma <- function(...) {
  engine <- .resolve_msma_engine(...) 
  switch(engine,
    legacy = .msma_legacy(...),
    soft = .msma_soft(...),
    exclusive = .msma_exclusive(...) 
  )
}
```

The first refactor should extract policy without changing the existing estimation bodies.
