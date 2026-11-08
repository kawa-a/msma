# msma Version 5 preprocessing audit

- Date: 2026-10-31 11:27:59 +0900
- Package version: 5.0.0.9000
- Branch: feature/v5-preprocessing-audit
- Commit: c48ff214fe30d8d4febf0ace5280fa8b2e572305

## Objective

Document preprocessing behavior in the legacy, extended, soft, and exclusive paths before refactoring.
This audit does not change numerical behavior.

## Scaling calls

Matches: 15

```r
  302 | 
  303 | if(muX < 0) stop("muX should be positive")
  304 | if(muY < 0) stop("muY should be positive")
  305 | 
  306 | ########## Start ##########
  307 | X = lapply(X, function(x) scale(x, scale=scaling))
  308 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  309 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  310 | 
  311 | Xscale = lapply(X, attributes)
  312 | Yscale = lapply(Y, attributes)
  313 | 
  314 | if(scaling){
  315 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  316 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  317 | }
  318 | 
  319 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  600 | 
  601 | if(muX < 0) stop("muX should be positive")
  602 | if(muY < 0) stop("muY should be positive")
  603 | 
  604 | ########## Start ##########
  605 | X = lapply(X, function(x) scale(x, scale=scaling))
  606 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  607 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  608 | 
  609 | Xscale = lapply(X, attributes)
  610 | Yscale = lapply(Y, attributes)
  611 | 
  612 | if(scaling){
  613 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  614 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  615 | }
  616 | 
  617 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
 1109 | 
 1110 | Ynb = length(newY)
 1111 | Xnb = length(newX)
 1112 | 
 1113 | ########## Start ##########
 1114 | #newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`, scale=object$Xscale[[x]]$`scaled:scale`))
 1115 | newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`))
 1116 | newX = lapply(newX, function(x) apply(x, 2, function(x2){ x2[is.na(x2)] = 0; x2}))
 1117 | sbX = lapply(1:Xnb, function(x) newX[[x]] %*% object$wbX[[x]] )
 1118 | predX = lapply(1:Xnb, function(x) cbind(sbX[[x]]) %*% project(newX[[x]], sbX[[x]]))
 1119 | 
 1120 | if(!(is.null(newY) | object$dmode == "PCA")) {
 1121 | #newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`, scale=object$Yscale[[x]]$`scaled:scale`))
 1122 | newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`))
 1123 | newY = lapply(newY, function(y) apply(y, 2, function(y2){ y2[is.na(y2)] = 0; y2}))
 1124 | sbY = lapply(1:Ynb, function(x) newY[[x]] %*% object$wbY[[x]] )
 1125 | predY = lapply(1:Ynb, function(x) cbind(sbY[[x]]) %*% project(newY[[x]], sbY[[x]]))
 1126 | }else
 1127 | {
 1128 | predY = sbY = NULL
 1129 | }
 1130 | 
 1131 | ##### Output #####
 1132 | list(X = predX, sbX = sbX, Y = predY, sbY = sbY)
 1252 | }
 1253 | 
 1254 | fit = msma(tmpX, tmpY, tmpZ, comp, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta, type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg, con4spv=con4spv)
 1255 | test = predict(fit, newX=testX, newY=testY)
 1256 | 
 1257 | #testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`, scale=fit$Xscale[[x]]$`scaled:scale`))
 1258 | testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`))
 1259 | Xerr = matserr(testX, test$X)
 1260 | out = Xerr
 1261 | 
 1262 | if(dmode == "PLS"){
 1263 | testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`, scale=fit$Yscale[[x]]$`scaled:scale`))
 1264 | testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`))
 1265 | Yerr = matserr(testY, test$Y)
 1266 | out = c(mean(c(Xerr, Yerr)), Xerr, Yerr)
 1267 | }
 1268 | 
 1269 | out
 1270 | })
 1271 | 
 1272 | if(dmode == "PLS"){ 
 1273 | err = rowMeans(cv.err)
 1274 | names(err) = c("Average", "X", "Y")
 2160 | UY2 = do.call(cbind, UY)
 2161 | ZcoefY = normvec(rnorm(nblockY*ncomp, ifelse(c(WY[[2]])==0, 0, abs(1/c(WY[[2]]))), 0.01)) * cz[2]
 2162 | UY2b = UY2 %*% ZcoefY
 2163 | }
 2164 | UXYb = UX2b + UY2b
 2165 | UXYb = c(scale(UXYb, scale=FALSE))
 2166 | Zprob = exp(UXYb)/(1+exp(UXYb))
 2167 | Z = sapply(Zprob, function(x) rbinom(1, 1, x))
 2168 | if(Ztype %in% "prob") Z = Zprob
 2169 | }else{
 2170 | Z = ZcoefX = ZcoefY = NULL
 2171 | }
 2172 | 
 2173 | list(X=X, Y=Y, Z=Z, ncomp=ncomp, Xps=Xps, nZeroX=nZeroX, idxZeroX=idxZeroX, nZeroY=nZeroY, idxZeroY=idxZeroY, WX=WX, WY=WY, ZcoefX=ZcoefX, ZcoefY=ZcoefY, SUX=SUX, SUY=SUY)
 2174 | }
 2175 | 
```

## Centering metadata

Matches: 14

```r
  201 |     if(!is.null(inX) || !is.null(inY) || !is.null(inXsup) || !is.null(inYsup)) stop("Forced-in variables are not supported for structured PCA", call.=FALSE)
  202 |     if(!identical(sprmethod,"PCA") || !is.null(con4spv)) stop("Structured PCA cannot be combined with NMF, sNMF, CMP, or con4spv", call.=FALSE)
  203 |     if(!is.null(Z) && !is.null(dim(Z)) && ncol(as.matrix(Z))>1L) stop("Structured PCA accepts only vector or one-column Z", call.=FALSE)
  204 |     lambda_s4 = if(is.null(lambdaX)) 0 else lambdaX
  205 |     if(length(lambda_s4)!=1L) stop("Structured PCA requires scalar lambdaX", call.=FALSE)
  206 |     X_input=X; X_center=rep(0,ncol(X)); X_scale=rep(1,ncol(X))
  207 |     if(isTRUE(scaling)){
  208 |       X_center=colMeans(X_input); X_scale=apply(sweep(X_input,2,X_center,"-"),2,stats::sd)
  209 |       X_scale[!is.finite(X_scale) | X_scale==0]=1
  210 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  211 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  212 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  213 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  214 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  215 |       niter=niterS4,seed=intseed,verbose=verbose)
  216 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  217 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  218 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  219 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  220 |     class(fit_s4)="s4pca"
  221 |     return(fit_s4)
  222 |   }
  223 |   if(identical(engine, "legacy")){
  224 |     z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  225 |     z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  226 |     # A one-column matrix is a version 3.2 input convenience, but its
 1109 | 
 1110 | Ynb = length(newY)
 1111 | Xnb = length(newX)
 1112 | 
 1113 | ########## Start ##########
 1114 | #newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`, scale=object$Xscale[[x]]$`scaled:scale`))
 1115 | newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`))
 1116 | newX = lapply(newX, function(x) apply(x, 2, function(x2){ x2[is.na(x2)] = 0; x2}))
 1117 | sbX = lapply(1:Xnb, function(x) newX[[x]] %*% object$wbX[[x]] )
 1118 | predX = lapply(1:Xnb, function(x) cbind(sbX[[x]]) %*% project(newX[[x]], sbX[[x]]))
 1119 | 
 1120 | if(!(is.null(newY) | object$dmode == "PCA")) {
 1121 | #newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`, scale=object$Yscale[[x]]$`scaled:scale`))
 1122 | newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`))
 1123 | newY = lapply(newY, function(y) apply(y, 2, function(y2){ y2[is.na(y2)] = 0; y2}))
 1124 | sbY = lapply(1:Ynb, function(x) newY[[x]] %*% object$wbY[[x]] )
 1125 | predY = lapply(1:Ynb, function(x) cbind(sbY[[x]]) %*% project(newY[[x]], sbY[[x]]))
 1126 | }else
 1127 | {
 1128 | predY = sbY = NULL
 1129 | }
 1130 | 
 1131 | ##### Output #####
 1132 | list(X = predX, sbX = sbX, Y = predY, sbY = sbY)
 1252 | }
 1253 | 
 1254 | fit = msma(tmpX, tmpY, tmpZ, comp, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta, type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg, con4spv=con4spv)
 1255 | test = predict(fit, newX=testX, newY=testY)
 1256 | 
 1257 | #testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`, scale=fit$Xscale[[x]]$`scaled:scale`))
 1258 | testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`))
 1259 | Xerr = matserr(testX, test$X)
 1260 | out = Xerr
 1261 | 
 1262 | if(dmode == "PLS"){
 1263 | testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`, scale=fit$Yscale[[x]]$`scaled:scale`))
 1264 | testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`))
 1265 | Yerr = matserr(testY, test$Y)
 1266 | out = c(mean(c(Xerr, Yerr)), Xerr, Yerr)
 1267 | }
 1268 | 
 1269 | out
 1270 | })
 1271 | 
 1272 | if(dmode == "PLS"){ 
 1273 | err = rowMeans(cv.err)
 1274 | names(err) = c("Average", "X", "Y")
 2539 |     X <- as.matrix(newdata)
 2540 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2541 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2542 |     }
 2543 |     if (isTRUE(object$scaling)) {
 2544 |       X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
 2545 |     }
 2546 |     Xhat <- (X %*% object$W) %*% B
 2547 |   }
 2548 |   colnames(Xhat) <- rownames(object$W)
 2549 |   if (!scaled && isTRUE(object$scaling)) {
 2550 |     Xhat <- sweep(sweep(Xhat, 2, object$Xscale, "*"), 2, object$Xcenter, "+")
 2551 |   }
 2552 |   Xhat
 2553 | }
 2554 | 
 2555 | .s4pca_grid <- function(lambdaX, gammaX, comp) {
 2556 |   if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
 2557 |     stop("lambdaX must contain non-negative finite values", call. = FALSE)
 2558 |   }
 2559 |   if (!is.numeric(gammaX) || !length(gammaX) || any(!is.finite(gammaX)) || any(gammaX < 0)) {
 2560 |     stop("gammaX must contain non-negative finite values", call. = FALSE)
```

## Scaling metadata

Matches: 25

```r
   58 | #' @param niterS4 fixed number of structured PCA updates per component.
   59 | #' @param ... further arguments passed to or from other methods.
   60 | #' @return \item{dmode}{Which modes "PLS" or "PCA"}
   61 | #' @return \item{X}{Scaled X which has a list form.}
   62 | #' @return \item{Y}{Scaled Y which has a list form.}
   63 | #' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
   64 | #' @return \item{Yscale}{Scaling information for Y. The means and standard deviations for each block of Y are returned.}
   65 | #' @return \item{comp}{the number of componets}
   66 | #' @return \item{wbX}{block loading for X}
   67 | #' @return \item{sbX}{block score for X}
   68 | #' @return \item{wbY}{block loading for Y}
   69 | #' @return \item{sbY}{block score for Y}
   70 | #' @return \item{ssX}{super score for X}
   71 | #' @return \item{wsX}{super loading for X}
   72 | #' @return \item{ssY}{super score for Y}
   73 | #' @return \item{wsY}{super loading for Y}
   74 | #' @return \item{nzwbX}{number of nonzeros in block loading for X}
  211 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  212 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  213 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  214 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  215 |       niter=niterS4,seed=intseed,verbose=verbose)
  216 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  217 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  218 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  219 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  220 |     class(fit_s4)="s4pca"
  221 |     return(fit_s4)
  222 |   }
  223 |   if(identical(engine, "legacy")){
  224 |     z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  225 |     z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  226 |     # A one-column matrix is a version 3.2 input convenience, but its
  306 | ########## Start ##########
  307 | X = lapply(X, function(x) scale(x, scale=scaling))
  308 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  309 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  310 | 
  311 | Xscale = lapply(X, attributes)
  312 | Yscale = lapply(Y, attributes)
  313 | 
  314 | if(scaling){
  315 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  316 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  317 | }
  318 | 
  319 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  320 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  321 | 
  322 | tvarX = lapply(X, function(x) sum(x^2))
  323 | tvarY = lapply(Y, function(x) sum(x^2))
  324 | 
  325 | ############### Iteration for components Start ###############
  326 | Xd = X; Yd = Y; out = cpevX = cpevY = list(); bic = bicX = bicY = reproduct = predictiv = rep(0, comp)
  501 | 
  502 | ##### Output #####
  503 | if(dmode == "PCA"){ 
  504 | out = list(call = match.call(), 
  505 | dmode=dmode,
  506 | X=X, Z=Z, Xscale=Xscale, comp = c(comp, comp2),
  507 | muX=muX, 
  508 | wbX=wbX, sbX=sbX, ssX=ssX, wsX=wsX,
  509 | nzwbX = nzwbX, nzwsX=nzwsX,
  510 | selectXnames = selectXnames,
  511 | cpevX = cpevX,
  512 | avX = avX,
  513 | bic = bic, bic2=bic2,
  514 | reproduct = reproduct, predictiv=predictiv
  515 | )
  516 | }else
  517 | {
  518 | out = list(call = match.call(), 
  519 | dmode=dmode,
  520 | X=X, Y=Y, Z=Z,
  521 | Xscale=Xscale, Yscale=Yscale, comp = c(comp, comp2),
  522 | muX=muX, muY=muY,
  523 | wbX=wbX, sbX=sbX, wbY=wbY, sbY=sbY, ssX=ssX, wsX=wsX, ssY=ssY, wsY=wsY, 
  524 | nzwbX = nzwbX, nzwbY = nzwbY, nzwsX=nzwsX, nzwsY=nzwsY,
  525 | selectXnames = selectXnames, selectYnames = selectYnames,
  526 | cpevX = cpevX, cpevY = cpevY,
  527 | avX = avX, avY = avY,
  528 | bic = bicY, bicX = bicX, bicY = bicY,
  529 | bic2 = bic2Y, bic2X = bic2X, bic2Y = bic2Y,
  530 | reproduct = reproduct, predictiv=predictiv
  531 | )
  604 | ########## Start ##########
  605 | X = lapply(X, function(x) scale(x, scale=scaling))
  606 | Y = lapply(Y, function(x) scale(x, scale=scaling))
  607 | if(!is.null(Z)) Z = scale(Z, scale=scaling)
  608 | 
  609 | Xscale = lapply(X, attributes)
  610 | Yscale = lapply(Y, attributes)
  611 | 
  612 | if(scaling){
  613 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  614 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  615 | }
  616 | 
  617 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  618 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  619 | 
  620 | tvarX = lapply(X, function(x) sum(x^2))
  621 | tvarY = lapply(Y, function(x) sum(x^2))
  622 | 
  623 | ############### Iteration for components Start ###############
  624 | Xd = X; Yd = Y; out = ssX = ssY = wsX = wsY = cpevX = cpevY = predictiv = list(); bic = bicX = bicY = reproduct = rep(0, comp)
  838 | 
  839 | ##### Output #####
  840 | if(dmode == "PCA"){ 
  841 | out = list(call = match.call(), 
  842 | dmode=dmode,
  843 | X=X, Z=Z, Xscale=Xscale, comp = c(comp, comp2),
  844 | muX=muX, 
  845 | wbX=wbX, sbX=sbX, ssX=ssX, wsX=wsX,
  846 | nzwbX = nzwbX, nzwsX=nzwsX,
  847 | selectXnames = selectXnames,
  848 | cpevX = cpevX,
  849 | avX = avX,
  850 | bic = bic, bic2=bic2,
  851 | reproduct = reproduct, predictiv=predictiv
  852 | )
  853 | }else
  854 | {
  855 | out = list(call = match.call(), 
  856 | dmode=dmode,
  857 | X=X, Y=Y, Z=Z,
  858 | Xscale=Xscale, Yscale=Yscale, comp = c(comp, comp2),
  859 | muX=muX, muY=muY,
  860 | wbX=wbX, sbX=sbX, wbY=wbY, sbY=sbY, ssX=ssX, wsX=wsX, ssY=ssY, wsY=wsY, 
  861 | nzwbX = nzwbX, nzwbY = nzwbY, nzwsX=nzwsX, nzwsY=nzwsY,
  862 | selectXnames = selectXnames, selectYnames = selectYnames,
  863 | cpevX = cpevX, cpevY = cpevY,
  864 | avX = avX, avY = avY,
  865 | bic = bicY, bicX = bicX, bicY = bicY,
  866 | bic2 = bic2Y, bic2X = bic2X, bic2Y = bic2Y,
  867 | reproduct = reproduct, predictiv=predictiv
  868 | )
 1109 | 
 1110 | Ynb = length(newY)
 1111 | Xnb = length(newX)
 1112 | 
 1113 | ########## Start ##########
 1114 | #newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`, scale=object$Xscale[[x]]$`scaled:scale`))
 1115 | newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`))
 1116 | newX = lapply(newX, function(x) apply(x, 2, function(x2){ x2[is.na(x2)] = 0; x2}))
 1117 | sbX = lapply(1:Xnb, function(x) newX[[x]] %*% object$wbX[[x]] )
 1118 | predX = lapply(1:Xnb, function(x) cbind(sbX[[x]]) %*% project(newX[[x]], sbX[[x]]))
 1119 | 
 1120 | if(!(is.null(newY) | object$dmode == "PCA")) {
 1121 | #newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`, scale=object$Yscale[[x]]$`scaled:scale`))
 1122 | newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`))
 1123 | newY = lapply(newY, function(y) apply(y, 2, function(y2){ y2[is.na(y2)] = 0; y2}))
 1124 | sbY = lapply(1:Ynb, function(x) newY[[x]] %*% object$wbY[[x]] )
 1125 | predY = lapply(1:Ynb, function(x) cbind(sbY[[x]]) %*% project(newY[[x]], sbY[[x]]))
 1126 | }else
 1127 | {
 1128 | predY = sbY = NULL
 1129 | }
 1130 | 
 1131 | ##### Output #####
 1132 | list(X = predX, sbX = sbX, Y = predY, sbY = sbY)
 1252 | }
 1253 | 
 1254 | fit = msma(tmpX, tmpY, tmpZ, comp, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta, type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg, con4spv=con4spv)
 1255 | test = predict(fit, newX=testX, newY=testY)
 1256 | 
 1257 | #testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`, scale=fit$Xscale[[x]]$`scaled:scale`))
 1258 | testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`))
 1259 | Xerr = matserr(testX, test$X)
 1260 | out = Xerr
 1261 | 
 1262 | if(dmode == "PLS"){
 1263 | testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`, scale=fit$Yscale[[x]]$`scaled:scale`))
 1264 | testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`))
 1265 | Yerr = matserr(testY, test$Y)
 1266 | out = c(mean(c(Xerr, Yerr)), Xerr, Yerr)
 1267 | }
 1268 | 
 1269 | out
 1270 | })
 1271 | 
 1272 | if(dmode == "PLS"){ 
 1273 | err = rowMeans(cv.err)
 1274 | names(err) = c("Average", "X", "Y")
 2539 |     X <- as.matrix(newdata)
 2540 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2541 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2542 |     }
 2543 |     if (isTRUE(object$scaling)) {
 2544 |       X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
 2545 |     }
 2546 |     Xhat <- (X %*% object$W) %*% B
 2547 |   }
 2548 |   colnames(Xhat) <- rownames(object$W)
 2549 |   if (!scaled && isTRUE(object$scaling)) {
 2550 |     Xhat <- sweep(sweep(Xhat, 2, object$Xscale, "*"), 2, object$Xcenter, "+")
 2551 |   }
 2552 |   Xhat
 2553 | }
 2554 | 
 2555 | .s4pca_grid <- function(lambdaX, gammaX, comp) {
 2556 |   if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
 2557 |     stop("lambdaX must contain non-negative finite values", call. = FALSE)
 2558 |   }
 2559 |   if (!is.numeric(gammaX) || !length(gammaX) || any(!is.finite(gammaX)) || any(gammaX < 0)) {
 2560 |     stop("gammaX must contain non-negative finite values", call. = FALSE)
```

## Manual centering and scaling

Matches: 5

```r
  203 |     if(!is.null(Z) && !is.null(dim(Z)) && ncol(as.matrix(Z))>1L) stop("Structured PCA accepts only vector or one-column Z", call.=FALSE)
  204 |     lambda_s4 = if(is.null(lambdaX)) 0 else lambdaX
  205 |     if(length(lambda_s4)!=1L) stop("Structured PCA requires scalar lambdaX", call.=FALSE)
  206 |     X_input=X; X_center=rep(0,ncol(X)); X_scale=rep(1,ncol(X))
  207 |     if(isTRUE(scaling)){
  208 |       X_center=colMeans(X_input); X_scale=apply(sweep(X_input,2,X_center,"-"),2,stats::sd)
  209 |       X_scale[!is.finite(X_scale) | X_scale==0]=1
  210 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  211 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  212 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  213 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  214 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  215 |       niter=niterS4,seed=intseed,verbose=verbose)
  216 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  217 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  218 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  219 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  220 |     class(fit_s4)="s4pca"
 2262 | R <- chol(sigma, pivot = TRUE)
 2263 | R[, order(attr(R, "pivot"))]
 2264 | }
 2265 | 
 2266 | retval <- matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = !pre0.9_9994) %*%  R
 2267 | retval <- sweep(retval, 2, mean, "+")
 2268 | colnames(retval) <- names(mean)
 2269 | retval
 2270 | }
 2271 | 
 2272 | #' @rdname msma-internal
 2273 | sprNMF <- function(X, K, lambda=NULL, ite = 500, seed=1){
 2274 |   set.seed(seed)
 2275 | 
 2276 |   N = nrow(X); M = ncol(X)
 2277 |   W<-matrix(runif(N*K), nrow = N)
 2539 |     X <- as.matrix(newdata)
 2540 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2541 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2542 |     }
 2543 |     if (isTRUE(object$scaling)) {
 2544 |       X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
 2545 |     }
 2546 |     Xhat <- (X %*% object$W) %*% B
 2547 |   }
 2548 |   colnames(Xhat) <- rownames(object$W)
 2549 |   if (!scaled && isTRUE(object$scaling)) {
 2550 |     Xhat <- sweep(sweep(Xhat, 2, object$Xscale, "*"), 2, object$Xcenter, "+")
 2551 |   }
 2552 |   Xhat
 2553 | }
 2554 | 
 2555 | .s4pca_grid <- function(lambdaX, gammaX, comp) {
 2556 |   if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
 2557 |     stop("lambdaX must contain non-negative finite values", call. = FALSE)
 2558 |   }
 2559 |   if (!is.numeric(gammaX) || !length(gammaX) || any(!is.finite(gammaX)) || any(gammaX < 0)) {
 2560 |     stop("gammaX must contain non-negative finite values", call. = FALSE)
```

## Missing-value handling

Matches: 21

```r
  250 | # number of nested componets
  251 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  252 | comp = comp[1]
  253 | 
  254 | ##### Requirement #####
  255 | if(missing(X)) stop("X should be specified")
  256 | if(is.null(Y)){ 
  257 | dmode = "PCA"
  258 | Y = X
  259 | if(!is.null(lambdaY)) stop("lambdaY can not be specified")
  260 | }else
  261 | {
  262 | dmode = "PLS"
  263 | }
  264 | 
  265 | if(!inherits(X, "list")) X = list(X)
  314 | if(scaling){
  315 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  316 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  317 | }
  318 | 
  319 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  320 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  321 | 
  322 | tvarX = lapply(X, function(x) sum(x^2))
  323 | tvarY = lapply(Y, function(x) sum(x^2))
  324 | 
  325 | ############### Iteration for components Start ###############
  326 | Xd = X; Yd = Y; out = cpevX = cpevY = list(); bic = bicX = bicY = reproduct = predictiv = rep(0, comp)
  327 | bic2 = bic2X = bic2Y = lapply(1:comp, function(x) {tb=rep(0, comp2); names(tb)=paste0("comp", 1:comp2); tb})
  328 | out2 = lapply(1:comp, function(x) lapply(1:comp2, function(y) NULL))
  329 | 
  330 | for(ncomp in 1:comp){
  539 | # number of nested componets
  540 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  541 | comp = comp[1]
  542 | 
  543 | ##### Requirement #####
  544 | if(missing(X)) stop("X should be specified")
  545 | if(is.null(Y)){ 
  546 | dmode = "PCA"
  547 | Y = X
  548 | if(!is.null(lambdaY)) stop("lambdaY can not be specified")
  549 | }else
  550 | {
  551 | dmode = "PLS"
  552 | }
  553 | 
  554 | if(!inherits(X, "list")) X = list(X)
  612 | if(scaling){
  613 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  614 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  615 | }
  616 | 
  617 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  618 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  619 | 
  620 | tvarX = lapply(X, function(x) sum(x^2))
  621 | tvarY = lapply(Y, function(x) sum(x^2))
  622 | 
  623 | ############### Iteration for components Start ###############
  624 | Xd = X; Yd = Y; out = ssX = ssY = wsX = wsY = cpevX = cpevY = predictiv = list(); bic = bicX = bicY = reproduct = rep(0, comp)
  625 | bic2 = bic2X = bic2Y = lapply(1:comp, function(x) {tb=rep(0, comp2); names(tb)=paste0("comp", 1:comp2); tb})
  626 | out2 = lapply(1:comp, function(x) lapply(1:comp2, function(y) NULL))
  627 | 
  628 | for(ncomp in 1:comp){
 1094 | #' ##### Predict #####
 1095 | #' test = predict(fit2, newX=X, newY=Y)
 1096 | #' 
 1097 | predict.msma = function(object, newX, newY=NULL,...){
 1098 | ##### Requirement #####
 1099 | if(missing(newX)) stop("newX should be specified")
 1100 | #if(missing(newY)) stop("newY should be specified")
 1101 | 
 1102 | if(is.null(newY) | (object$dmode == "PCA")) newY = newX
 1103 | 
 1104 | if(!inherits(newX, "list")) newX = list(newX)
 1105 | if(!inherits(newY, "list")) newY = list(newY)
 1106 | 
 1107 | if(!inherits(newX, "list")) stop("newX should be a list")
 1108 | if(!inherits(newY, "list")) stop("newY should be a list")
 1109 | 
 1110 | Ynb = length(newY)
 1111 | Xnb = length(newX)
 1112 | 
 1113 | ########## Start ##########
 1114 | #newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`, scale=object$Xscale[[x]]$`scaled:scale`))
 1115 | newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`))
 1116 | newX = lapply(newX, function(x) apply(x, 2, function(x2){ x2[is.na(x2)] = 0; x2}))
 1117 | sbX = lapply(1:Xnb, function(x) newX[[x]] %*% object$wbX[[x]] )
 1118 | predX = lapply(1:Xnb, function(x) cbind(sbX[[x]]) %*% project(newX[[x]], sbX[[x]]))
 1119 | 
 1120 | if(!(is.null(newY) | object$dmode == "PCA")) {
 1121 | #newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`, scale=object$Yscale[[x]]$`scaled:scale`))
 1122 | newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`))
 1123 | newY = lapply(newY, function(y) apply(y, 2, function(y2){ y2[is.na(y2)] = 0; y2}))
 1124 | sbY = lapply(1:Ynb, function(x) newY[[x]] %*% object$wbY[[x]] )
 1125 | predY = lapply(1:Ynb, function(x) cbind(sbY[[x]]) %*% project(newY[[x]], sbY[[x]]))
 1126 | }else
 1127 | {
 1128 | predY = sbY = NULL
 1129 | }
 1130 | 
 1131 | ##### Output #####
 1132 | list(X = predX, sbX = sbX, Y = predY, sbY = sbY)
 1133 | }
 1182 | #'cv2 = cvmsma(X, Y, comp = 2, lambdaX=2, lambdaY=1:3, nfold=5, seed=1)
 1183 | #'cv2
 1184 | #'
 1185 | cvmsma = function(X, Y=NULL, Z=NULL, comp=1, lambdaX, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, nfold=5, seed=1, intseed=1, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1], con4spv=NULL){
 1186 | ##### Requirement #####
 1187 | if(missing(X)) stop("X should be specified")
 1188 | if(is.null(Y)){ 
 1189 | dmode = "PCA"
 1190 | Y = X
 1191 | if(!is.null(lambdaY)) stop("lambdaY can not be specified")
 1192 | }else{
 1193 | dmode = "PLS"
 1194 | }
 1195 | 
 1196 | if(!inherits(X, "list")) X = list(X)
 1197 | if(!inherits(Y, "list")) Y = list(Y)
 1477 | #' fit4
 1478 | #' summary(fit4)
 1479 | #'
 1480 | regparasearch = function(X, Y=NULL, Z=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, comp=1, nfold=5, maxrep=3, minpct=0, maxpct=1, criterion=c("CV", "BIC")[1], whichselect=NULL, homo=NULL, intseed=1, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1]){
 1481 | ##### Requirement #####
 1482 | if(missing(X)) stop("X should be specified")
 1483 | if(minpct>=1 | minpct<0) stop("minpct should be 0 <= minpct < 1")
 1484 | if(maxpct>1 | maxpct<=0) stop("maxpct should be 0 < maxpct <= 1")
 1485 | 
 1486 | abnames = c("X", "Y", "Xsup", "Ysup")
 1487 | if(length(comp)<2) comp[2] = 1
 1488 | 
 1489 | ##### Candidates for Regularized Parameters #####
 1490 | intfit = msma(X, Y, Z, comp=1, lambdaX=rep(NULL, length(X)), lambdaY=rep(NULL, length(Y)), lambdaXsup=NULL, lambdaYsup=NULL, eta, type, inX, inY, inXsup=NULL, inYsup=NULL, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg)
 1491 | 
 1492 | intweight = list(X=intfit$wbX, Y=intfit$wbY, Xsup=intfit$wsX, Ysup=intfit$wsY)
 1755 | itr = 1
 1756 | repeat{
 1757 | 
 1758 | ### Xside (block) ###
 1759 | XwsX = lapply(1:length(X), function(x) X[[x]] * wsX[x]) #XwsX = X
 1760 | argus = lapply(XwsX, function(x){ tmp = c(t(x) %*% (muXY * ssY + muX * Z)); tmp[is.na(tmp)] = 0; tmp })
 1761 | wbX = lapply(1:Xnb, function(x) normvec(sparse(normvec(argus[[x]]), lambdaX[x], eta, type, inX[[x]])))
 1762 | sbX = lapply(1:Xnb, function(x) c(X[[x]] %*% wbX[[x]]) )
 1763 | sbX2 = do.call(cbind, sbX)
 1764 | 
 1765 | ### Xside (super) ###
 1766 | argusups = t(sbX2) %*% (muXY * ssY + muX * Z)
 1767 | wsX = normvec(sparse(normvec(argusups), lambdaXsup, eta, type, inXsup))
 1768 | ssX = sbX2 %*% wsX
 1769 | 
 1770 | ### Yside (block) ###
 1771 | YwsY = lapply(1:length(Y), function(x) Y[[x]] * wsY[x]) #YwsY = Y
 1772 | argvs = lapply(YwsY, function(x){ tmp = c(t(x) %*% (muXY * ssX + muY * Z)); tmp[is.na(tmp)] = 0; tmp})
 1773 | wbY = lapply(1:Ynb, function(y) normvec(sparse(normvec(argvs[[y]]), lambdaY[y], eta, type, inY[[y]])))
 1774 | sbY = lapply(1:Ynb, function(x) c(Y[[x]] %*% wbY[[x]]) )
 1775 | sbY2 = do.call(cbind, sbY)
 1776 | 
 1777 | ### Yside (super) ###
 1778 | #wsY = normvec(t(sbY2) %*% (muXY * ssX + muY * Z))
 1779 | argvsups = t(sbY2) %*% (muXY * ssX + muY * Z)
 1780 | wsY = normvec(sparse(normvec(argvsups), lambdaYsup, eta, type, inYsup))
 1781 | ssY = sbY2 %*% wsY
 1782 | 
 1825 | itr = 1
 1826 | repeat{
 1827 | 
 1828 | ### Xside (block) ###
 1829 | XwsX = lapply(1:length(X), function(x) X[[x]] * wsX[x]) #XwsX = X
 1830 | argus = lapply(XwsX, function(x){ tmp = c(t(x) %*% (muXY * ssY + muX * Z %*% con4spv)); tmp[is.na(tmp)] = 0; tmp })
 1831 | wbX = lapply(1:Xnb, function(x) normvec(sparse(normvec(argus[[x]]), lambdaX[x], eta, type, inX[[x]])))
 1832 | sbX = lapply(1:Xnb, function(x) c(X[[x]] %*% wbX[[x]]) )
 1833 | sbX2 = do.call(cbind, sbX)
 1834 | 
 1835 | ### Xside (super) ###
 1836 | argusups = t(sbX2) %*% (muXY * ssY + muX * Z %*% con4spv)
 1837 | wsX = normvec(sparse(normvec(argusups), lambdaXsup, eta, type, inXsup))
 1838 | ssX = sbX2 %*% wsX
 1839 | 
 1840 | ### Yside (block) ###
 1841 | YwsY = lapply(1:length(Y), function(x) Y[[x]] * wsY[x]) #YwsY = Y
 1842 | argvs = lapply(YwsY, function(x){ tmp = c(t(x) %*% (muXY * ssX + muY * Z %*% con4spv)); tmp[is.na(tmp)] = 0; tmp})
 1843 | wbY = lapply(1:Ynb, function(y) normvec(sparse(normvec(argvs[[y]]), lambdaY[y], eta, type, inY[[y]])))
 1844 | sbY = lapply(1:Ynb, function(x) c(Y[[x]] %*% wbY[[x]]) )
 1845 | sbY2 = do.call(cbind, sbY)
 1846 | 
 1847 | ### Yside (super) ###
 1848 | #wsY = normvec(t(sbY2) %*% (muXY * ssX + muY * Z))
 1849 | argvsups = t(sbY2) %*% (muXY * ssX + muY * Z %*% con4spv)
 1850 | wsY = normvec(sparse(normvec(argvsups), lambdaYsup, eta, type, inYsup))
 1851 | ssY = sbY2 %*% wsY
 1852 | 
 2435 |   }
 2436 |   if (nrow(X) < 2L || ncol(X) < 1L) {
 2437 |     stop("X must contain at least two rows and one column", call. = FALSE)
 2438 |   }
 2439 |   if (any(!is.finite(X))) {
 2440 |     stop("X must not contain NA, NaN, or Inf", call. = FALSE)
 2441 |   }
 2442 |   if (!is.null(Z)) {
 2443 |     if (is.matrix(Z) && ncol(Z) == 1L) Z <- drop(Z)
 2444 |     if (!is.numeric(Z) || !is.null(dim(Z))) {
 2445 |       stop("Z must be a numeric vector or a one-column numeric matrix", call. = FALSE)
 2446 |     }
 2447 |     if (length(Z) != nrow(X)) {
 2448 |       stop("length(Z) must equal nrow(X)", call. = FALSE)
 2449 |     }
 2450 |     if (any(!is.finite(Z))) {
 2451 |       stop("Z must not contain NA, NaN, or Inf", call. = FALSE)
 2452 |     }
 2453 |   }
 2454 |   scalar_integer <- function(x, name, lower) {
 2455 |     if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < lower || x != as.integer(x)) {
 2456 |       stop(name, " must be a single integer >= ", lower, call. = FALSE)
 2457 |     }
 2458 |     as.integer(x)
 2459 |   }
 2460 |   scalar_nonnegative <- function(x, name) {
 2461 |     if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 0) {
 2469 |   gamma <- scalar_nonnegative(gamma, "gamma")
 2470 |   if (length(mu) != 1L || !is.numeric(mu) || !is.finite(mu) || mu < 0 || mu > 1) {
 2471 |     stop("mu must be a single finite number in [0, 1]", call. = FALSE)
 2472 |   }
 2473 |   history_method <- match.arg(history_method, c("sumabs", "any", "count"))
 2474 |   if (length(hard_exclusion) != 1L || !is.logical(hard_exclusion) || is.na(hard_exclusion)) {
 2475 |     stop("hard_exclusion must be TRUE or FALSE", call. = FALSE)
 2476 |   }
 2477 |   if (!is.null(seed)) seed <- scalar_integer(seed, "seed", 0L)
 2478 |   list(X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
 2479 |        history_method = history_method, mu = as.numeric(mu),
 2480 |        hard_exclusion = hard_exclusion, niter = niter, seed = seed)
 2481 | }
 2482 | 
 2483 | .s4pca_fit <- function(X, Z = NULL, comp = 6, lambda = 0.02, gamma = 0,
 2484 |                        history_method = "sumabs", mu = 1,
 2526 | #' @param scaled logical; if TRUE, return reconstruction on the standardized scale.
 2527 | #' @return a reconstructed numeric matrix.
 2528 | #' @export
 2529 | s4pca_reconstruct <- function(object, newdata = NULL, scaled = FALSE) {
 2530 |   if (!inherits(object, "s4pca")) stop("object must inherit from 's4pca'", call. = FALSE)
 2531 |   if (length(scaled) != 1L || !is.logical(scaled) || is.na(scaled)) {
 2532 |     stop("scaled must be TRUE or FALSE", call. = FALSE)
 2533 |   }
 2534 |   B <- object$reconstruction_loadings
 2535 |   if (is.null(B)) stop("object does not contain reconstruction coefficients", call. = FALSE)
 2536 |   if (is.null(newdata)) {
 2537 |     Xhat <- object$S %*% B
 2538 |   } else {
 2539 |     X <- as.matrix(newdata)
 2540 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2541 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2645 |   }
 2646 |   if (length(repeats) != 1L || !is.numeric(repeats) || !is.finite(repeats) ||
 2647 |       repeats < 1 || repeats != as.integer(repeats)) {
 2648 |     stop("repeats must be a positive integer", call. = FALSE)
 2649 |   }
 2650 |   if (length(refit) != 1L || !is.logical(refit) || is.na(refit)) {
 2651 |     stop("refit must be TRUE or FALSE", call. = FALSE)
 2652 |   }
 2653 |   grid <- .s4pca_grid(lambdaX, gammaX, comp)
 2654 |   split_results <- lapply(seq_len(as.integer(repeats)), function(b) {
 2655 |     z <- .s4pca_one_split(X, Z, grid, calibration_fraction, alpha, max_overlap,
 2656 |       muX, input$history_method, input$niter, scaling, intseed + 1000L * b)
 2657 |     z[["repeat"]] <- b
 2658 |     z
 2659 |   })
 2660 |   details <- do.call(rbind, split_results)
```

## Zero-variance and non-finite handling

Matches: 26

```r
  127 |   )
  128 | 
  129 |   gamma_activates_soft <- (
  130 |     length(gammaX) == 1L &&
  131 |       is.numeric(gammaX) &&
  132 |       is.finite(gammaX) &&
  133 |       gammaX > 0
  134 |   )
  135 | 
  136 |   if (identical(structure.method, "exclusive")) {
  137 |     return("exclusive")
  138 |   }
  139 | 
  140 |   if (
  141 |     identical(structure.method, "soft") ||
  142 |       gamma_activates_soft
  204 |     lambda_s4 = if(is.null(lambdaX)) 0 else lambdaX
  205 |     if(length(lambda_s4)!=1L) stop("Structured PCA requires scalar lambdaX", call.=FALSE)
  206 |     X_input=X; X_center=rep(0,ncol(X)); X_scale=rep(1,ncol(X))
  207 |     if(isTRUE(scaling)){
  208 |       X_center=colMeans(X_input); X_scale=apply(sweep(X_input,2,X_center,"-"),2,stats::sd)
  209 |       X_scale[!is.finite(X_scale) | X_scale==0]=1
  210 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  211 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  212 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  213 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  214 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  215 |       niter=niterS4,seed=intseed,verbose=verbose)
  216 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  217 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  218 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  219 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  310 | 
  311 | Xscale = lapply(X, attributes)
  312 | Yscale = lapply(Y, attributes)
  313 | 
  314 | if(scaling){
  315 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  316 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  317 | }
  318 | 
  319 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  320 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  321 | 
  322 | tvarX = lapply(X, function(x) sum(x^2))
  323 | tvarY = lapply(Y, function(x) sum(x^2))
  324 | 
  325 | ############### Iteration for components Start ###############
  326 | Xd = X; Yd = Y; out = cpevX = cpevY = list(); bic = bicX = bicY = reproduct = predictiv = rep(0, comp)
  608 | 
  609 | Xscale = lapply(X, attributes)
  610 | Yscale = lapply(Y, attributes)
  611 | 
  612 | if(scaling){
  613 | for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
  614 | for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
  615 | }
  616 | 
  617 | X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
  618 | Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))
  619 | 
  620 | tvarX = lapply(X, function(x) sum(x^2))
  621 | tvarY = lapply(Y, function(x) sum(x^2))
  622 | 
  623 | ############### Iteration for components Start ###############
  624 | Xd = X; Yd = Y; out = ssX = ssY = wsX = wsY = cpevX = cpevY = predictiv = list(); bic = bicX = bicY = reproduct = rep(0, comp)
 1867 | ### Output ###
 1868 | list(wbX=wbX, sbX=sbX, wbY=wbY, sbY=sbY, ssX=ssX, wsX=wsX, ssY=ssY, wsY=wsY)
 1869 | }
 1870 | 
 1871 | #' @rdname msma-internal
 1872 | normvec = function(a){if(all(a == 0)){a}else{ c(a) / sqrt(drop(crossprod(c(a))))}}
 1873 | 
 1874 | #' @rdname msma-internal
 1875 | sparse = function(x, lam, eta=1, type="lasso", inidx=NULL){
 1876 | if(all(x == 0)){ lam = 0 }else if(lam >= max(abs(x))){ 
 1877 | tmpdiff = diff(sort(unique(abs(x)))); tmpdiff = tmpdiff[tmpdiff > 1e-8]
 1878 | lam = max(abs(x)) - tmpdiff[length(tmpdiff)]/2
 1879 | }
 1880 | lam = rep(lam, length(x))
 1881 | if(!is.null(inidx)) lam[inidx] = 0
 1882 | sout = sapply(1:length(x), function(x1) {
 1883 | if(type=="lasso"){
 1884 | sign(x[x1]) * max(c(abs(x[x1]) - lam[x1], 0))
 1885 | }else 
 1886 | if(type=="hard"){
 2070 | }else{
 2071 | if(inherits(WX, "matrix")){ WX = list(WX)}
 2072 | ncomp=unlist(lapply(WX, function(wx) ncol(wx[[1]])))
 2073 | Xps = unlist(lapply(WX[[1]], function(wx2)nrow(wx2)))
 2074 | idxZeroX = lapply(1:length(WX), function(wi)
 2075 | {if(wi==1){NULL}else{lapply(WX[[wi]], function(wx){ lapply(1:ncol(wx), function(c1) which(wx[,c1] == 0 )) }) 
 2076 | }})
 2077 | }
 2078 | 
 2079 | #####
 2080 | nZeroX = lapply(idxZeroX, function(x) do.call(rbind, lapply(x, function(x1) unlist(lapply(x1, function(x2){ iz=length(x2); ifelse(is.null(iz),0,iz)})) )))
 2081 | 
 2082 | #####
 2083 | SUX = lapply(1:ncomp[1], function(y){
 2084 | out = sapply(1:ncomp[2], function(x) rnorm(n, 0, 1)) 
 2085 | out = out[,rank(-diag(var(out))),drop=FALSE]
 2150 | 
 2151 | ##### Generate Z #####
 2152 | if(!(Ztype %in% "none")){
 2153 | UX2 = do.call(cbind, UX)
 2154 | ZcoefXmean0 = unlist(WX[[2]])#WX[[2]]#unlist(lapply(WX[[2]], function(x) x[,ncomp]))
 2155 | ZcoefXmean = ifelse(ZcoefXmean0==0, 0, abs(1/ZcoefXmean0))
 2156 | ZcoefX = normvec(rnorm(nblockX*ncomp[1], ZcoefXmean, 0.01)) * cz[1]
 2157 | UX2b = UX2 %*% ZcoefX
 2158 | UY2b = 0; ZcoefY = NULL
 2159 | if(Yps[1]){
 2160 | UY2 = do.call(cbind, UY)
 2161 | ZcoefY = normvec(rnorm(nblockY*ncomp, ifelse(c(WY[[2]])==0, 0, abs(1/c(WY[[2]]))), 0.01)) * cz[2]
 2162 | UY2b = UY2 %*% ZcoefY
 2163 | }
 2164 | UXYb = UX2b + UY2b
 2165 | UXYb = c(scale(UXYb, scale=FALSE))
 2166 | Zprob = exp(UXYb)/(1+exp(UXYb))
 2167 | Z = sapply(Zprob, function(x) rbinom(1, 1, x))
 2168 | if(Ztype %in% "prob") Z = Zprob
 2169 | }else{
 2170 | Z = ZcoefX = ZcoefY = NULL
 2171 | }
 2303 | # This internal function intentionally preserves the manuscript-generating
 2304 | # algorithm before later validation, diagnostics, and public API integration.
 2305 | ####################################################################################################
 2306 | 
 2307 | .s4pca_norm_reference <- function(a) {
 2308 |   if (all(a == 0)) a else c(a) / sqrt(drop(crossprod(c(a))))
 2309 | }
 2310 | 
 2311 | .s4pca_sparse_reference <- function(x, lam, eta = 1, type = "lasso", inidx = NULL) {
 2312 |   if (all(x == 0)) {
 2313 |     lam <- 0
 2314 |   } else if (isTRUE(length(lam) == 1) && lam >= max(abs(x))) {
 2315 |     tmpdiff <- diff(sort(unique(abs(x))))
 2316 |     tmpdiff <- tmpdiff[tmpdiff > 1e-8]
 2317 |     lam <- max(abs(x)) - ifelse(length(tmpdiff) > 0, utils::tail(tmpdiff, 1) / 2, 1e-6)
 2318 |   }
 2319 |   lam <- rep(lam, length(x))
 2320 |   if (!is.null(inidx)) lam[inidx] <- 0
 2321 |   sapply(seq_along(x), function(j) {
 2322 |     if (type == "lasso") {
 2434 |     stop("X must be a numeric matrix", call. = FALSE)
 2435 |   }
 2436 |   if (nrow(X) < 2L || ncol(X) < 1L) {
 2437 |     stop("X must contain at least two rows and one column", call. = FALSE)
 2438 |   }
 2439 |   if (any(!is.finite(X))) {
 2440 |     stop("X must not contain NA, NaN, or Inf", call. = FALSE)
 2441 |   }
 2442 |   if (!is.null(Z)) {
 2443 |     if (is.matrix(Z) && ncol(Z) == 1L) Z <- drop(Z)
 2444 |     if (!is.numeric(Z) || !is.null(dim(Z))) {
 2445 |       stop("Z must be a numeric vector or a one-column numeric matrix", call. = FALSE)
 2446 |     }
 2447 |     if (length(Z) != nrow(X)) {
 2448 |       stop("length(Z) must equal nrow(X)", call. = FALSE)
 2449 |     }
 2450 |     if (any(!is.finite(Z))) {
 2451 |       stop("Z must not contain NA, NaN, or Inf", call. = FALSE)
 2452 |     }
 2453 |   }
 2454 |   scalar_integer <- function(x, name, lower) {
 2455 |     if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < lower || x != as.integer(x)) {
 2456 |       stop(name, " must be a single integer >= ", lower, call. = FALSE)
 2457 |     }
 2458 |     as.integer(x)
 2459 |   }
 2460 |   scalar_nonnegative <- function(x, name) {
 2461 |     if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 0) {
 2462 |       stop(name, " must be a single non-negative finite number", call. = FALSE)
 2463 |     }
 2464 |     as.numeric(x)
 2465 |   }
 2466 |   comp <- scalar_integer(comp, "comp", 1L)
 2467 |   niter <- scalar_integer(niter, "niter", 1L)
 2468 |   lambda <- scalar_nonnegative(lambda, "lambda")
 2469 |   gamma <- scalar_nonnegative(gamma, "gamma")
 2470 |   if (length(mu) != 1L || !is.numeric(mu) || !is.finite(mu) || mu < 0 || mu > 1) {
 2471 |     stop("mu must be a single finite number in [0, 1]", call. = FALSE)
 2472 |   }
 2473 |   history_method <- match.arg(history_method, c("sumabs", "any", "count"))
 2474 |   if (length(hard_exclusion) != 1L || !is.logical(hard_exclusion) || is.na(hard_exclusion)) {
 2475 |     stop("hard_exclusion must be TRUE or FALSE", call. = FALSE)
 2476 |   }
 2477 |   if (!is.null(seed)) seed <- scalar_integer(seed, "seed", 0L)
 2478 |   list(X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
 2479 |        history_method = history_method, mu = as.numeric(mu),
 2480 |        hard_exclusion = hard_exclusion, niter = niter, seed = seed)
 2535 |   if (is.null(B)) stop("object does not contain reconstruction coefficients", call. = FALSE)
 2536 |   if (is.null(newdata)) {
 2537 |     Xhat <- object$S %*% B
 2538 |   } else {
 2539 |     X <- as.matrix(newdata)
 2540 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2541 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2542 |     }
 2543 |     if (isTRUE(object$scaling)) {
 2544 |       X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
 2545 |     }
 2546 |     Xhat <- (X %*% object$W) %*% B
 2547 |   }
 2548 |   colnames(Xhat) <- rownames(object$W)
 2549 |   if (!scaled && isTRUE(object$scaling)) {
 2550 |     Xhat <- sweep(sweep(Xhat, 2, object$Xscale, "*"), 2, object$Xcenter, "+")
 2551 |   }
 2552 |   Xhat
 2553 | }
 2554 | 
 2555 | .s4pca_grid <- function(lambdaX, gammaX, comp) {
 2556 |   if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
 2557 |     stop("lambdaX must contain non-negative finite values", call. = FALSE)
 2558 |   }
 2559 |   if (!is.numeric(gammaX) || !length(gammaX) || any(!is.finite(gammaX)) || any(gammaX < 0)) {
 2560 |     stop("gammaX must contain non-negative finite values", call. = FALSE)
 2561 |   }
 2562 |   if (!is.numeric(comp) || !length(comp) || any(!is.finite(comp)) || any(comp < 1) || any(comp != as.integer(comp))) {
 2563 |     stop("comp must contain positive integers", call. = FALSE)
 2564 |   }
 2565 |   unique(expand.grid(lambdaX = as.numeric(lambdaX), gammaX = as.numeric(gammaX),
 2566 |                      comp = as.integer(comp), KEEP.OUT.ATTRS = FALSE,
 2567 |                      stringsAsFactors = FALSE))
 2568 | }
 2569 | 
 2570 | .s4pca_one_split <- function(X, Z, grid, calibration_fraction, alpha,
 2571 |                              max_overlap, muX, history.method, niterS4,
 2572 |                              scaling, seed) {
 2630 |   input <- .s4pca_validate_inputs(X = X, Z = Z, comp = 1, lambda = 0,
 2631 |     gamma = 0, history_method = history.method, mu = muX,
 2632 |     hard_exclusion = FALSE, niter = niterS4, seed = intseed)
 2633 |   X <- input$X; Z <- input$Z
 2634 |   if (nrow(X) < 4L) stop("X must contain at least four rows for split selection", call. = FALSE)
 2635 |   if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
 2636 |     stop("alpha must be in (0, 1)", call. = FALSE)
 2637 |   }
 2638 |   if (length(calibration_fraction) != 1L || !is.numeric(calibration_fraction) ||
 2639 |       !is.finite(calibration_fraction) || calibration_fraction <= 0 || calibration_fraction >= 1) {
 2640 |     stop("calibration_fraction must be in (0, 1)", call. = FALSE)
 2641 |   }
 2642 |   if (length(max_overlap) != 1L || !is.numeric(max_overlap) ||
 2643 |       !is.finite(max_overlap) || max_overlap < 0 || max_overlap > 1) {
 2644 |     stop("max_overlap must be in [0, 1]", call. = FALSE)
 2645 |   }
 2646 |   if (length(repeats) != 1L || !is.numeric(repeats) || !is.finite(repeats) ||
 2647 |       repeats < 1 || repeats != as.integer(repeats)) {
 2648 |     stop("repeats must be a positive integer", call. = FALSE)
 2649 |   }
 2650 |   if (length(refit) != 1L || !is.logical(refit) || is.na(refit)) {
 2651 |     stop("refit must be TRUE or FALSE", call. = FALSE)
 2652 |   }
 2653 |   grid <- .s4pca_grid(lambdaX, gammaX, comp)
 2654 |   split_results <- lapply(seq_len(as.integer(repeats)), function(b) {
 2655 |     z <- .s4pca_one_split(X, Z, grid, calibration_fraction, alpha, max_overlap,
 2656 |       muX, input$history_method, input$niter, scaling, intseed + 1000L * b)
```

## Reconstruction and new-data paths

Matches: 15

```r
  212 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  213 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  214 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  215 |       niter=niterS4,seed=intseed,verbose=verbose)
  216 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  217 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  218 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  219 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  220 |     class(fit_s4)="s4pca"
  221 |     return(fit_s4)
  222 |   }
  223 |   if(identical(engine, "legacy")){
  224 |     z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  225 |     z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  226 |     # A one-column matrix is a version 3.2 input convenience, but its
  227 |     # numerical behavior and returned object must agree with vector-valued Z.
 2511 |   fit$settings$interface <- "validated"
 2512 |   fit
 2513 | }
 2514 | 
 2515 | ####################################################################################################
 2516 | # S4PCA Milestone 5: reconstruction and repeated split conformal model selection.
 2517 | ####################################################################################################
 2518 | 
 2519 | #' Reconstruct data from an S4PCA fit
 2520 | #'
 2521 | #' Reconstructs the training data or a new matrix using the score subspace and
 2522 | #' reconstruction coefficients learned by a public S4PCA call to \code{msma}.
 2523 | #'
 2524 | #' @param object an object of class \code{s4pca}.
 2525 | #' @param newdata optional numeric matrix with the same variables as the fitted X.
 2526 | #' @param scaled logical; if TRUE, return reconstruction on the standardized scale.
 2527 | #' @return a reconstructed numeric matrix.
 2528 | #' @export
 2529 | s4pca_reconstruct <- function(object, newdata = NULL, scaled = FALSE) {
 2530 |   if (!inherits(object, "s4pca")) stop("object must inherit from 's4pca'", call. = FALSE)
 2531 |   if (length(scaled) != 1L || !is.logical(scaled) || is.na(scaled)) {
 2532 |     stop("scaled must be TRUE or FALSE", call. = FALSE)
 2533 |   }
 2534 |   B <- object$reconstruction_loadings
 2535 |   if (is.null(B)) stop("object does not contain reconstruction coefficients", call. = FALSE)
 2536 |   if (is.null(newdata)) {
 2537 |     Xhat <- object$S %*% B
 2538 |   } else {
 2539 |     X <- as.matrix(newdata)
 2540 |     if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
 2541 |       stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
 2542 |     }
 2543 |     if (isTRUE(object$scaling)) {
 2544 |       X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
 2545 |     }
 2546 |     Xhat <- (X %*% object$W) %*% B
 2547 |   }
 2548 |   colnames(Xhat) <- rownames(object$W)
 2549 |   if (!scaled && isTRUE(object$scaling)) {
 2550 |     Xhat <- sweep(sweep(Xhat, 2, object$Xscale, "*"), 2, object$Xcenter, "+")
 2551 |   }
 2583 |       comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
 2584 |       structure.method = "soft", gammaX = grid$gammaX[g],
 2585 |       history.method = history.method, niterS4 = niterS4,
 2586 |       scaling = scaling, intseed = seed + g
 2587 |     )
 2588 |     pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
 2589 |     conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
 2590 |     q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
 2591 |     out[[g]] <- data.frame(
 2592 |       lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
 2593 |       score = q, overlap_all = fit$overlap_all,
 2594 |       overlap_selected = fit$overlap_selected,
 2595 |       admissible = fit$overlap_selected <= max_overlap
 2596 |     )
 2597 |   }
 2598 |   do.call(rbind, out)
 2599 | }
 2600 | 
 2601 | #' Repeated split conformal selection for S4PCA
 2602 | #'
 2603 | #' Evaluates combinations of sparsity, overlap penalty, and component count by
 2604 | #' reconstruction error on repeated calibration splits. The selected candidate
 2605 | #' minimizes the median conformal reconstruction quantile subject to an optional
 2606 | #' overlap constraint.
 2607 | #'
 2608 | #' @param X numeric data matrix.
 2609 | #' @param Z optional numeric supervision vector or one-column matrix.
 2610 | #' @param lambdaX candidate sparsity parameters.
 2611 | #' @param gammaX candidate soft-structure parameters.
 2612 | #' @param comp candidate component counts.
 2613 | #' @param alpha miscoverage level in (0,1).
 2614 | #' @param repeats positive number of repeated splits.
 2615 | #' @param calibration_fraction fraction assigned to calibration in each split.
```

## Legacy engine preprocessing context

Matches: 2

```r
  224 |     z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  225 |     z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  226 |     # A one-column matrix is a version 3.2 input convenience, but its
  227 |     # numerical behavior and returned object must agree with vector-valued Z.
  228 |     Z_legacy = if(z_is_matrix && z_ncol == 1L) drop(Z) else Z
  229 |     return(.msma_default_legacy(
  230 |       X=X, Y=Y, Z=Z_legacy, comp=comp, lambdaX=lambdaX, lambdaY=lambdaY,
  231 |       lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type,
  232 |       inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX=muX, muY=muY,
  233 |       defmethod=defmethod, scaling=scaling, verbose=verbose,
  234 |       intseed=intseed, ceps=ceps, ...
  235 |     ))
  236 |   }
  237 | 
  238 |   .msma_default_extended(
  239 |     X=X, Y=Y, Z=Z, comp=comp, lambdaX=lambdaX, lambdaY=lambdaY,
  243 |     intseed=intseed, ceps=ceps, sprmethod=sprmethod, nneg=nneg,
  244 |     con4spv=con4spv, ...
  245 |   )
  246 | }
  247 | 
  248 | .msma_default_legacy = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, defmethod = "canonical", scaling = TRUE, verbose=FALSE, intseed=1, ceps=0.0001, ...)
  249 | {
  250 | # number of nested componets
  251 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  252 | comp = comp[1]
  253 | 
  254 | ##### Requirement #####
  255 | if(missing(X)) stop("X should be specified")
  256 | if(is.null(Y)){ 
  257 | dmode = "PCA"
  258 | Y = X
```

## Extended engine preprocessing context

Matches: 2

```r
  233 |       defmethod=defmethod, scaling=scaling, verbose=verbose,
  234 |       intseed=intseed, ceps=ceps, ...
  235 |     ))
  236 |   }
  237 | 
  238 |   .msma_default_extended(
  239 |     X=X, Y=Y, Z=Z, comp=comp, lambdaX=lambdaX, lambdaY=lambdaY,
  240 |     lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type,
  241 |     inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX=muX, muY=muY,
  242 |     defmethod=defmethod, scaling=scaling, verbose=verbose,
  243 |     intseed=intseed, ceps=ceps, sprmethod=sprmethod, nneg=nneg,
  244 |     con4spv=con4spv, ...
  245 |   )
  246 | }
  247 | 
  248 | .msma_default_legacy = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, defmethod = "canonical", scaling = TRUE, verbose=FALSE, intseed=1, ceps=0.0001, ...)
  532 | }
  533 | class(out) = "msma"
  534 | out
  535 | }
  536 | 
  537 | .msma_default_extended = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, defmethod = "canonical", scaling = TRUE, verbose=FALSE, intseed=1, ceps=0.0001, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1], con4spv=NULL,...)
  538 | {
  539 | # number of nested componets
  540 | comp2 = ifelse(length(comp)<2, 1, comp[2])
  541 | comp = comp[1]
  542 | 
  543 | ##### Requirement #####
  544 | if(missing(X)) stop("X should be specified")
  545 | if(is.null(Y)){ 
  546 | dmode = "PCA"
  547 | Y = X
```

## Structured engine preprocessing context

Matches: 4

```r
  208 |       X_center=colMeans(X_input); X_scale=apply(sweep(X_input,2,X_center,"-"),2,stats::sd)
  209 |       X_scale[!is.finite(X_scale) | X_scale==0]=1
  210 |       X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
  211 |     } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
  212 |     Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
  213 |     fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
  214 |       history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
  215 |       niter=niterS4,seed=intseed,verbose=verbose)
  216 |     fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
  217 |     fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
  218 |     fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
  219 |     fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
  220 |     class(fit_s4)="s4pca"
  221 |     return(fit_s4)
  222 |   }
  223 |   if(identical(engine, "legacy")){
 2353 |     }
 2354 |     invcrossy %*% t(y) %*% x
 2355 |   }
 2356 | }
 2357 | 
 2358 | .s4pca_fit_reference <- function(X, Z = NULL, comp = 6, lambda = 0.02,
 2359 |                                  gamma = 0, history_method = "sumabs", mu = 1.0,
 2360 |                                  hard_exclusion = FALSE, niter = 30, seed = NULL,
 2361 |                                  verbose = FALSE) {
 2362 |   if (!is.null(seed)) set.seed(seed)
 2363 |   history_method <- match.arg(history_method, c("sumabs", "any", "count"))
 2364 |   Xs <- X
 2365 |   Zs <- Z
 2366 |   n <- nrow(Xs)
 2367 |   p <- ncol(Xs)
 2368 |   s <- .s4pca_norm_reference(rnorm(n))
 2478 |   list(X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
 2479 |        history_method = history_method, mu = as.numeric(mu),
 2480 |        hard_exclusion = hard_exclusion, niter = niter, seed = seed)
 2481 | }
 2482 | 
 2483 | .s4pca_fit <- function(X, Z = NULL, comp = 6, lambda = 0.02, gamma = 0,
 2484 |                        history_method = "sumabs", mu = 1,
 2485 |                        hard_exclusion = FALSE, niter = 30, seed = NULL,
 2486 |                        verbose = FALSE) {
 2487 |   input <- .s4pca_validate_inputs(
 2488 |     X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
 2489 |     history_method = history_method, mu = mu,
 2490 |     hard_exclusion = hard_exclusion, niter = niter, seed = seed
 2491 |   )
 2492 |   fit <- .s4pca_fit_reference(
 2493 |     X = input$X, Z = input$Z, comp = input$comp,
 2494 |     lambda = input$lambda, gamma = input$gamma,
 2495 |     history_method = input$history_method, mu = input$mu,
 2496 |     hard_exclusion = input$hard_exclusion, niter = input$niter,
 2497 |     seed = input$seed, verbose = verbose
 2498 |   )
 2499 |   fit$diagnostics <- list(
 2500 |     engine = "reference-compatible",
 2501 |     iterations_per_component = rep(input$niter, ncol(fit$W)),
 2502 |     extracted_components = ncol(fit$W),
```

## Required invariants

1. Version 4 results remain numerically unchanged.
2. Legacy X, Y, and Z preprocessing remains unchanged.
3. Structured X preprocessing and reconstruction metadata remain unchanged.
4. Extended NMF, sNMF, CMP, multivariate-Z, and con4spv behavior remains unchanged.
5. NA handling remains path-specific until explicitly redesigned.
6. Existing public fields such as Xcenter, Xscale, and legacy scale attributes remain available.
7. New preprocessing metadata is additive and internal during the first refactor.

## Proposed first-stage design

Introduce metadata helpers before moving numerical transformations:

```r
.new_msma_preprocess <- function(engine, scaling) {
  list(
    engine = engine,
    scaling = scaling,
    X = NULL,
    Y = NULL,
    Z = NULL,
    missing = NULL
  )
}
```

Do not initially replace existing scale() or sweep() calls. First attach equivalent metadata and verify all regression tests.
