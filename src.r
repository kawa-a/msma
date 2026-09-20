#' Multiblock Sparse Matrix Analysis Package
#'
#' A package for multiblock sparse multivariable analysis.
#'
#' @keywords internal
#' @author Atsushi Kawaguchi. \email{kawa_a24@@yahoo.co.jp}
#' @seealso \code{\link{msma}}
#' @references
#' Kawaguchi A, Yamashita F (2017). Supervised Multiblock Sparse
#' Multivariable Analysis with Application to Multimodal Brain Imaging
#' Genetics. Biostatistics, 18(4), 651-665.
#' @importFrom grDevices gray
#' @importFrom graphics abline matplot plot barplot par text
#' @importFrom stats cor cor.test var predict quantile rbinom rnorm runif cutree dist hclust lm rect.hclust
"_PACKAGE"

##########################################################################################################################################
#' Multiblock Sparse Partial Least Squares
#'
#' This is a function for a matrix decomposition method incorporating sparse and supervised modeling for a multiblock multivariable data analysis 
#' 
#' \code{msma} requires at least one input X (a matrix or list). In this case, (multiblock) PCA is conducted. If Y is also specified, then a PLS is conducted using X as explanatory variables and Y as objective variables. This function scales each data matrix to a mean of 0 and variance of 1 in the default. The block structure can be represented as a list. If Z is also specified, a supervised version is implemented, and the degree is controlled by muX or muY, where 0 <= muX <= 1, 0 <= muY <= 1, and 0 <= muX + muY < 1. If a positive lambdaX or lambdaY is specified, then a sparse estimation based on the L1 penalty is implemented.
#'
#' @name msma
#' @aliases msma
#' @rdname msma
#' @docType methods
#' @export
#'
#' @param X a matrix or list of matrices indicating the explanatory variable(s). This parameter is required.
#' @param Y a matrix or list of matrices indicating objective variable(s). This is optional. If there is no input for Y, then PCA is implemented.
#' @param Z a vector or matrix, response variable(s) for implementing the supervised version of (multiblock) PCA or PLS. This is optional. The (row) length of Z is the number of subjects. If there is no input for Z, then unsupervised PLS/PCA is implemented.
#' @param comp numeric scalar for the maximum number of componets to be considered.
#' @param lambdaX numeric vector of regularized parameters for X, with a length equal to the number of blocks. If lambdaX is omitted, no regularization is conducted.
#' @param lambdaY numeric vector of regularized parameters for Y, with a length equal to the number of blocks. If lambdaY is omitted, no regularization is conducted.
#' @param lambdaXsup numeric vector of regularized parameters for the super weight of X with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param lambdaYsup numeric vector of regularized parameters for the super weight of Y with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param eta numeric scalar indicating the parameter indexing the penalty family that can be set with the argument type. 
#' @param type a character, indicating the penalty family. In this version, the following choices are available: "lasso", "hard", "scad" and "mcp."
#' @param inX a vector or list of numeric vectors specifying the variables in X, always included in the model
#' @param inY a vector or list of numeric vectors specifying the variables in Y, always included in the model
#' @param inXsup a (list of) numeric vector to specify the blocks of X which are always in the model. 
#' @param inYsup a (list of) numeric vector to specify the blocks of Y which are always in the model. 
#' @param muX a numeric scalar for the weight of X for the supervised case. 0 <= muX <= 1.
#' @param muY a numeric scalar for the weight of Y for the supervised case. 0 <= muY <= 1. 
#' @param defmethod a character representing the deflation method. This version has only the choice "canonical." 
#' @param scaling a logical, indicating whether or not data scaling is performed. The default is TRUE.
#' @param verbose information
#' @param intseed seed number for the random number in the parameter estimation algorithm.
#' @param x an object of class "\code{msma}", usually, a result of a call to \code{\link{msma}}
#' @param ceps a numeric scalar for the convergence condition of the algorithm
#' @param sprmethod a character, 
#' @param nneg a character 
#' @param con4spv a numeric vector
#' @param structure.method one of \"none\", \"soft\", or \"exclusive\".
#' @param gammaX non-negative history-dependent penalty strength.
#' @param history.method one of \"sumabs\", \"any\", or \"count\".
#' @param niterS4 fixed number of structured PCA updates per component.
#' @param ... further arguments passed to or from other methods.
#' @return \item{dmode}{Which modes "PLS" or "PCA"}
#' @return \item{X}{Scaled X which has a list form.}
#' @return \item{Y}{Scaled Y which has a list form.}
#' @return \item{Xscale}{Scaling information for X. The means and standard deviations for each block of X are returned.}
#' @return \item{Yscale}{Scaling information for Y. The means and standard deviations for each block of Y are returned.}
#' @return \item{comp}{the number of componets}
#' @return \item{wbX}{block loading for X}
#' @return \item{sbX}{block score for X}
#' @return \item{wbY}{block loading for Y}
#' @return \item{sbY}{block score for Y}
#' @return \item{ssX}{super score for X}
#' @return \item{wsX}{super loading for X}
#' @return \item{ssY}{super score for Y}
#' @return \item{wsY}{super loading for Y}
#' @return \item{nzwbX}{number of nonzeros in block loading for X}
#' @return \item{nzwbY}{number of nonzeros in block loading for Y}
#' @return \item{nzwsX}{number of nonzeros in super loading for X}
#' @return \item{nzwsY}{number of nonzeros in super loading for Y}
#' @return \item{selectXnames}{names of selected variables for X}
#' @return \item{selectYnames}{names of selected variables for Y}
#' @return \item{avX}{the adjusted variance of the score for X}
#' @return \item{avY}{the adjusted variance of the score for Y}
#' @return \item{cpevX}{the cumulative percentage of the explained variance for X}
#' @return \item{cpevY}{the cumulative percentage of the explained variance for Y}
#' @return \item{reproduct}{Predictivity. Correlation between Y and the predicted Y}
#' @return \item{predictiv}{Reproductivity. Correlation between the score for Y and the outcome Z}
#'
#' @examples
#' ##### data #####
#' tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = 20, seed=1)
#' X = tmpdata$X; Y = tmpdata$Y 
#' 
#' ##### One Component #####
#' fit1 = msma(X, Y, comp=1, lambdaX=2, lambdaY=1:3)
#' fit1
#' 
#' ##### Two Component #####
#' fit2 = msma(X, Y, comp=2, lambdaX=2, lambdaY=1:3)
#' fit2
#' 
#' ##### Sparse Principal Component Analysis #####
#' fit3 = msma(X, comp=5, lambdaX=2.5)
#' summary(fit3)
#' 
msma = function(X, ...) UseMethod("msma")

#' @rdname msma
#' @method msma default
#' @export
# Backward-compatible dispatcher for msma 3.2.
# The exact version 3.1 engine is used unless a version 3.2 feature is requested.
msma.default = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL,
  lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL,
  inXsup=NULL, inYsup=NULL, muX=0, muY=0, defmethod="canonical",
  scaling=TRUE, verbose=FALSE, intseed=1, ceps=0.0001,
  sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1],
  nneg=c("posneg", "absolute", "min")[1], con4spv=NULL,
  structure.method=c("none", "soft", "exclusive")[1], gammaX=0,
  history.method=c("sumabs", "any", "count")[1], niterS4=30, ...)
{
  structure.method = match.arg(structure.method, c("none", "soft", "exclusive"))
  use_s4pca = !identical(structure.method, "none") ||
    (length(gammaX)==1L && is.numeric(gammaX) && is.finite(gammaX) && gammaX > 0)
  if(use_s4pca){
    if(identical(structure.method, "none")) structure.method = "soft"
    if(!is.matrix(X) || is.list(X)) stop("Structured PCA requires X to be a single numeric matrix", call.=FALSE)
    if(!is.null(Y)) stop("Structured PCA with Y is not supported in msma 4.0", call.=FALSE)
    if(length(comp)!=1L) stop("Structured PCA requires a single component count", call.=FALSE)
    if(!is.null(lambdaY) || !is.null(lambdaXsup) || !is.null(lambdaYsup)) stop("Y-side and super-level penalties are not supported for structured PCA", call.=FALSE)
    if(!identical(type,"lasso")) stop("Structured PCA supports type = \"lasso\" only", call.=FALSE)
    if(!is.null(inX) || !is.null(inY) || !is.null(inXsup) || !is.null(inYsup)) stop("Forced-in variables are not supported for structured PCA", call.=FALSE)
    if(!identical(sprmethod,"PCA") || !is.null(con4spv)) stop("Structured PCA cannot be combined with NMF, sNMF, CMP, or con4spv", call.=FALSE)
    if(!is.null(Z) && !is.null(dim(Z)) && ncol(as.matrix(Z))>1L) stop("Structured PCA accepts only vector or one-column Z", call.=FALSE)
    lambda_s4 = if(is.null(lambdaX)) 0 else lambdaX
    if(length(lambda_s4)!=1L) stop("Structured PCA requires scalar lambdaX", call.=FALSE)
    X_input=X; X_center=rep(0,ncol(X)); X_scale=rep(1,ncol(X))
    if(isTRUE(scaling)){
      X_center=colMeans(X_input); X_scale=apply(sweep(X_input,2,X_center,"-"),2,stats::sd)
      X_scale[!is.finite(X_scale) | X_scale==0]=1
      X_input=sweep(sweep(X_input,2,X_center,"-"),2,X_scale,"/")
    } else if(!identical(scaling,FALSE)) stop("scaling must be TRUE or FALSE", call.=FALSE)
    Z_input=if(!is.null(Z) && is.matrix(Z) && ncol(Z)==1L) drop(Z) else Z
    fit_s4=.s4pca_fit(X_input,Z_input,comp=comp,lambda=lambda_s4,gamma=gammaX,
      history_method=history.method,mu=muX,hard_exclusion=identical(structure.method,"exclusive"),
      niter=niterS4,seed=intseed,verbose=verbose)
    fit_s4$call=match.call(); fit_s4$Xcenter=X_center; fit_s4$Xscale=X_scale
    fit_s4$reconstruction_loadings=.s4pca_project_reference(X_input, fit_s4$S)
    fit_s4$scaling=scaling; fit_s4$structure.method=structure.method
    fit_s4$gammaX=gammaX; fit_s4$lambdaX=lambda_s4; fit_s4$muX=muX; fit_s4$dmode="PCA"
    class(fit_s4)="s4pca"
    return(fit_s4)
  }
  z_is_matrix = !is.null(Z) && !is.null(dim(Z))
  z_ncol = if(z_is_matrix) ncol(as.matrix(Z)) else if(is.null(Z)) 0L else 1L
  z_is_multivariate = z_ncol > 1L
  use_extended = !identical(sprmethod, "PCA") || z_is_multivariate || !is.null(con4spv)

  if(!use_extended){
    # A one-column matrix is a version 3.2 input convenience, but its
    # numerical behavior and returned object must agree with vector-valued Z.
    Z_legacy = if(z_is_matrix && z_ncol == 1L) drop(Z) else Z
    return(.msma_default_legacy(
      X=X, Y=Y, Z=Z_legacy, comp=comp, lambdaX=lambdaX, lambdaY=lambdaY,
      lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type,
      inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX=muX, muY=muY,
      defmethod=defmethod, scaling=scaling, verbose=verbose,
      intseed=intseed, ceps=ceps, ...
    ))
  }

  .msma_default_extended(
    X=X, Y=Y, Z=Z, comp=comp, lambdaX=lambdaX, lambdaY=lambdaY,
    lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type,
    inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX=muX, muY=muY,
    defmethod=defmethod, scaling=scaling, verbose=verbose,
    intseed=intseed, ceps=ceps, sprmethod=sprmethod, nneg=nneg,
    con4spv=con4spv, ...
  )
}

.msma_default_legacy = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, defmethod = "canonical", scaling = TRUE, verbose=FALSE, intseed=1, ceps=0.0001, ...)
{
# number of nested componets
comp2 = ifelse(length(comp)<2, 1, comp[2])
comp = comp[1]

##### Requirement #####
if(missing(X)) stop("X should be specified")
if(is.null(Y)){ 
dmode = "PCA"
Y = X
if(!is.null(lambdaY)) stop("lambdaY can not be specified")
}else
{
dmode = "PLS"
}

if(!inherits(X, "list")) X = list(X)
if(!inherits(Y, "list")) Y = list(Y)

if(!inherits(X, "list")) stop("X should be a list")
if(!inherits(Y, "list")) stop("Y should be a list")

Ynb = length(Y)
Xnb = length(X)

if(length(lambdaX) == 1) lambdaX = rep(lambdaX, Xnb)
if(length(lambdaY) == 1) lambdaY = rep(lambdaY, Ynb)

if(!is.null(lambdaX)){if(length(lambdaX) != Xnb) stop("lambdaX should be same length as the number of blocks of X")}
if(!is.null(lambdaY)){if(length(lambdaY) != Ynb) stop("lambdaY should be same length as the number of blocks of Y")}

if(!is.null(inX)){if(!inherits(inX, "list")) inX = list(inX)}
if(!is.null(inY)){if(!inherits(inY, "list")) inY = list(inY)}

Yps = unlist(lapply(Y, ncol))
Xps = unlist(lapply(X, ncol))

Yns = unlist(lapply(Y, nrow))
Xns = unlist(lapply(X, nrow))

if(length(unique(Yns)) == 1){Yns = unique(Yns)}else{stop("numbers of rows should be same among Y")}
if(length(unique(Xns)) == 1){Xns = unique(Xns)}else{stop("numbers of rows should be same among X")}
if(Yns == Xns){n = Xns}else{stop("numbers of rows should be same between X and Y")}
if(!is.null(Z)){ if(length(Z) != n) stop("length of Z should be same as row of X or Y")}

if(!is.null(inX)){
extest = unlist(lapply(1:Xnb, function(a) inX[[a]] %in% (1:Xps[a]) ))
if(!(all(extest))) stop("inX should be specified the column number of X")
}
if(!is.null(inY)){
extest = unlist(lapply(1:Ynb, function(a) inY[[a]] %in% (1:Yps[a]) ))
if(!(all(extest))) stop("inY should be specified the column number of Y")
}

if(muX < 0) stop("muX should be positive")
if(muY < 0) stop("muY should be positive")

########## Start ##########
X = lapply(X, function(x) scale(x, scale=scaling))
Y = lapply(Y, function(x) scale(x, scale=scaling))
if(!is.null(Z)) Z = scale(Z, scale=scaling)

Xscale = lapply(X, attributes)
Yscale = lapply(Y, attributes)

if(scaling){
for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
}

X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))

tvarX = lapply(X, function(x) sum(x^2))
tvarY = lapply(Y, function(x) sum(x^2))

############### Iteration for components Start ###############
Xd = X; Yd = Y; out = cpevX = cpevY = list(); bic = bicX = bicY = reproduct = predictiv = rep(0, comp)
bic2 = bic2X = bic2Y = lapply(1:comp, function(x) {tb=rep(0, comp2); names(tb)=paste0("comp", 1:comp2); tb})
out2 = lapply(1:comp, function(x) lapply(1:comp2, function(y) NULL))

for(ncomp in 1:comp){

##### Fit #####
out[[ncomp]] = out1 = .msma_onecomp_legacy(Xd, Yd, Z, lambdaX, lambdaY, lambdaXsup, lambdaYsup, eta, type, inX, inY, inXsup, inYsup, muX, muY, dmode, verbose, intseed, ceps)

##### Nest #####
sbXd = sbX0 = list(do.call(cbind, out1$sbX))
sbYd = sbY0 = list(do.call(cbind, out1$sbY))

for(ncomp2 in 1:comp2){

if(ncomp2 == 1){
out21 = out1
out21$wbX[[1]] = out1$wsX; out21$wbY[[1]] = out1$wsY
out2[[ncomp]][[ncomp2]] = out21
}else{
out2[[ncomp]][[ncomp2]] = out21 = .msma_onecomp_legacy(sbXd, sbYd, Z, lambdaXsup, lambdaYsup, NULL, NULL, eta, type, inXsup, inYsup, NULL, NULL, muX, muY, dmode, verbose, intseed, ceps)
}

# arrange 
ssX = do.call(cbind, lapply(out2[[ncomp]], function(y) y$ssX))
ssY = do.call(cbind, lapply(out2[[ncomp]], function(y) y$ssY))

# Prediction 
predSX = ssX %*% project(sbX0[[1]], ssX)
predSY = ssY %*% project(sbY0[[1]], ssY)

# Deflation
sbXd = lapply(sbX0, function(x) x - predSX)
sbYd = lapply(sbY0, function(x) x - predSY)

# arrange 
wsX = lapply(out2[1:ncomp], function(x) do.call(cbind, lapply(x, function(y) y$wbX[[1]])))
wsY = lapply(out2[1:ncomp], function(x) do.call(cbind, lapply(x, function(y) y$wbY[[1]])))

nzwsX = lapply(wsX, function(x) apply(x, 2, function(z2) sum(z2!=0)) )
nzwsY = lapply(wsY, function(x) apply(x, 2, function(z2) sum(z2!=0)) )

# Information Criteria 
if(dmode == "PCA"){ 
tmpse = sum(unlist(lapply(sbXd, function(x) sum(x^2))))
#print(tmpse)
bic2[[ncomp]][ncomp2] = log(tmpse/(n*Xnb)) + log(n*Xnb)/(n*Xnb)*(sum(unlist(nzwsX)))
}else
{
tmpse = sum(unlist(lapply(sbYd, function(x) sum(x^2))))
bic2Y[[ncomp]][ncomp2] = log(tmpse/(n*Ynb)) + log(n*Ynb)/(n*Ynb)*(sum(unlist(nzwsY)))
tmpse = sum(unlist(lapply(sbXd, function(x) sum(x^2))))
bic2X[[ncomp]][ncomp2] = log(tmpse/(n*Xnb)) + log(n*Xnb)/(n*Xnb)*(sum(unlist(nzwsX)))
}

}

##### arrange #####
sbX = lapply(1:Xnb, function(x){ do.call(cbind, lapply(out, function(y) y$sbX[[x]]))})
sbY = lapply(1:Ynb, function(x){ do.call(cbind, lapply(out, function(y) y$sbY[[x]]))})

wbX = lapply(1:Xnb, function(x){ 
tmp = do.call(cbind, lapply(out, function(y) y$wbX[[x]]))
colnames(tmp) = paste("comp", 1:ncol(tmp), sep="")
if(is.null(colnames(X[[x]]))){rownames(tmp) = paste("X", x, 1:ncol(X[[x]]), sep=".")}else{rownames(tmp) = colnames(X[[x]])}
tmp
})
names(sbX) = names(wbX) = paste("block", 1:Xnb, sep="")

wbY = lapply(1:Ynb, function(x){ 
tmp = do.call(cbind, lapply(out, function(y) y$wbY[[x]]))
colnames(tmp) = paste("comp", 1:ncol(tmp), sep="")
if(is.null(colnames(Y[[x]]))){rownames(tmp) = paste("Y", x, 1:ncol(Y[[x]]), sep=".")}else{rownames(tmp) = colnames(Y[[x]])}
tmp
})
names(sbY) = names(wbY) = paste("block", 1:Ynb, sep="")

##### Number of nonzero #####
nzwbX = do.call(rbind, lapply(wbX, function(z) apply(z, 2, function(z2) sum(z2!=0))))
nzwbY = do.call(rbind, lapply(wbY, function(z) apply(z, 2, function(z2) sum(z2!=0))))

##### Prediction #####
predX = lapply(1:Xnb, function(x) sbX[[x]] %*% project(X[[x]], sbX[[x]]))
predY = lapply(1:Ynb, function(x) sbY[[x]] %*% project(Y[[x]], sbY[[x]]))

cpevX[[ncomp]] = sapply(1:Xnb, function(x) sum(predX[[x]]^2)/tvarX[[x]] )
cpevY[[ncomp]] = sapply(1:Ynb, function(x) sum(predY[[x]]^2)/tvarY[[x]] )

##### Deflation #####
if(defmethod == "canonical"){
Xd = lapply(1:Xnb, function(x) X[[x]] - predX[[x]])
Yd = lapply(1:Ynb, function(x) Y[[x]] - predY[[x]])
}else
if(defmethod == "canonical2"){
Xd = lapply(1:Xnb, function(x) Xd[[x]] - predX[[x]])
Yd = lapply(1:Ynb, function(x) Yd[[x]] - predY[[x]])
}else
if(defmethod == "pls"){
Xd = lapply(1:Xnb, function(x) X[[x]] - predX[[x]])
Yd = lapply(1:Ynb, function(x) Y[[x]] - predY[[x]])
}

##### Predictivity and Reproductivity #####
reproduct[ncomp] = cor(unlist(lapply(Y, c)), unlist(lapply(predY, c)))^2
predictiv[ncomp] = cor(out1$ssY, Z)^2

##### Information Criteria #####
if(dmode == "PCA"){ 
tmpse = sum(unlist(lapply(Xd, function(x) sum(x^2))))
#print(tmpse)
bic[ncomp] = log(tmpse/(n*sum(Xps))) + log(n*sum(Xps))/(n*sum(Xps))*(sum(nzwbX))
}else
{
#tmpse = sum(unlist(lapply(Xd, function(x) sum(x^2)))) + sum(unlist(lapply(Yd, function(x) sum(x^2))))
tmpse = sum(unlist(lapply(Yd, function(x) sum(x^2))))
bicY[ncomp] = log(tmpse/(n*sum(Yps))) + log(n*sum(Yps))/(n*sum(Yps))*(sum(nzwbY))
#bic[ncomp] = log(tmpse/(n*sum(c(Xps,Yps)))) + log(n*sum(c(Xps,Yps)))/(n*sum(c(Xps,Yps)))*(sum(c(nzwbX,nzwbY)))
tmpse = sum(unlist(lapply(Xd, function(x) sum(x^2))))
bicX[ncomp] = log(tmpse/(n*sum(Xps))) + log(n*sum(Xps))/(n*sum(Xps))*(sum(nzwbX))
}

}
############### Iteration for components End ###############

##### arrange #####
ssX = lapply(out2, function(x) do.call(cbind, lapply(x, function(y) y$ssX)))
ssY = lapply(out2, function(x) do.call(cbind, lapply(x, function(y) y$ssY)))

cpevX = do.call(cbind, cpevX)
cpevY = do.call(cbind, cpevY)

if(ncomp==1){
avX = cpevX; avY = cpevY
}else
if(ncomp==2){
avX = cbind(cpevX[,1], cpevX[,2]-cpevX[,1])
avY = cbind(cpevY[,1], cpevY[,2]-cpevY[,1])
}else{
avX = cbind(cpevX[,1], t(apply(cpevX, 1, diff)))
avY = cbind(cpevY[,1], t(apply(cpevY, 1, diff)))
}
colnames(cpevY) = colnames(cpevX) = colnames(avY) = colnames(avX) = paste("comp", 1:comp, sep="")
rownames(cpevX) = rownames(avX) = paste("block", 1:nrow(nzwbX), sep="")
rownames(cpevY) = rownames(avY) = paste("block", 1:nrow(nzwbY), sep="")

colnames(nzwbX) = colnames(nzwbY) = paste("comp", 1:comp, sep="")
rownames(nzwbX) = paste("block", 1:nrow(nzwbX), sep="")
rownames(nzwbY) = paste("block", 1:nrow(nzwbY), sep="")

names(wsY) = names(wsX) = paste("comp", 1:comp, sep="")
for(i in 1:length(wsY)){ 
colnames(wsY[[i]]) = paste("comp", 1:comp2, sep="")
rownames(wsY[[i]]) = paste("block", 1:nrow(nzwbY), sep="")
}
for(i in 1:length(wsX)){ 
colnames(wsX[[i]]) = paste("comp", 1:comp2, sep="")
rownames(wsX[[i]]) = paste("block", 1:nrow(nzwbX), sep="")
}

names(nzwsX) = names(nzwsY) = paste("comp", 1:comp, sep="")
for(i in 1:length(nzwsX)) names(nzwsX[[i]]) = paste("comp", i, "-", 1:comp2, sep="") 
for(i in 1:length(nzwsY)) names(nzwsY[[i]]) = paste("comp", i, "-", 1:comp2, sep="") 

names(bic) = names(bicX) = names(bicY) = names(bic2) = paste("comp", 1:comp, sep="")
if(comp2 == 1) bic2 = unlist(bic2)

##### Names of variables #####
selectXnames = lapply(1:length(wbX), function(z) apply(wbX[[z]], 2, function(z2){ 
if(is.null(colnames(X[[z]]))) colnames(X[[z]]) = paste("X", z, 1:ncol(X[[z]]), sep=".")
colnames(X[[z]])[z2!=0]
}))
selectYnames = lapply(1:length(wbY), function(z) apply(wbY[[z]], 2, function(z2){ 
if(is.null(colnames(Y[[z]]))) colnames(Y[[z]]) = paste("Y", z, 1:ncol(Y[[z]]), sep=".")
colnames(Y[[z]])[z2!=0]
}))

##### Output #####
if(dmode == "PCA"){ 
out = list(call = match.call(), 
dmode=dmode,
X=X, Z=Z, Xscale=Xscale, comp = c(comp, comp2),
muX=muX, 
wbX=wbX, sbX=sbX, ssX=ssX, wsX=wsX,
nzwbX = nzwbX, nzwsX=nzwsX,
selectXnames = selectXnames,
cpevX = cpevX,
avX = avX,
bic = bic, bic2=bic2,
reproduct = reproduct, predictiv=predictiv
)
}else
{
out = list(call = match.call(), 
dmode=dmode,
X=X, Y=Y, Z=Z,
Xscale=Xscale, Yscale=Yscale, comp = c(comp, comp2),
muX=muX, muY=muY,
wbX=wbX, sbX=sbX, wbY=wbY, sbY=sbY, ssX=ssX, wsX=wsX, ssY=ssY, wsY=wsY, 
nzwbX = nzwbX, nzwbY = nzwbY, nzwsX=nzwsX, nzwsY=nzwsY,
selectXnames = selectXnames, selectYnames = selectYnames,
cpevX = cpevX, cpevY = cpevY,
avX = avX, avY = avY,
bic = bicY, bicX = bicX, bicY = bicY,
bic2 = bic2Y, bic2X = bic2X, bic2Y = bic2Y,
reproduct = reproduct, predictiv=predictiv
)
}
class(out) = "msma"
out
}

.msma_default_extended = function(X, Y=NULL, Z=NULL, comp=2, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, defmethod = "canonical", scaling = TRUE, verbose=FALSE, intseed=1, ceps=0.0001, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1], con4spv=NULL,...)
{
# number of nested componets
comp2 = ifelse(length(comp)<2, 1, comp[2])
comp = comp[1]

##### Requirement #####
if(missing(X)) stop("X should be specified")
if(is.null(Y)){ 
dmode = "PCA"
Y = X
if(!is.null(lambdaY)) stop("lambdaY can not be specified")
}else
{
dmode = "PLS"
}

if(!inherits(X, "list")) X = list(X)
if(!inherits(Y, "list")) Y = list(Y)

if(!inherits(X, "list")) stop("X should be a list")
if(!inherits(Y, "list")) stop("Y should be a list")

Ynb = length(Y)
Xnb = length(X)

Ybnames = names(Y)
Xbnames = names(X)

if(is.null(Ybnames)){Ybnames = paste("block", 1:Ynb, sep="")}
if(is.null(Xbnames)){Xbnames = paste("block", 1:Xnb, sep="")}

if(length(lambdaX) == 1) lambdaX = rep(lambdaX, Xnb)
if(length(lambdaY) == 1) lambdaY = rep(lambdaY, Ynb)

if(!is.null(lambdaX)){if(length(lambdaX) != Xnb) stop("lambdaX should be same length as the number of blocks of X")}
if(!is.null(lambdaY)){if(length(lambdaY) != Ynb) stop("lambdaY should be same length as the number of blocks of Y")}

if(!is.null(inX)){if(!inherits(inX, "list")) inX = list(inX)}
if(!is.null(inY)){if(!inherits(inY, "list")) inY = list(inY)}

Yps = unlist(lapply(Y, ncol))
Xps = unlist(lapply(X, ncol))

Yns = unlist(lapply(Y, nrow))
Xns = unlist(lapply(X, nrow))

if(length(unique(Yns)) == 1){Yns = unique(Yns)}else{stop("numbers of rows should be same among Y")}
if(length(unique(Xns)) == 1){Xns = unique(Xns)}else{stop("numbers of rows should be same among X")}
if(Yns == Xns){n = Xns}else{stop("numbers of rows should be same between X and Y")}
if(!is.null(Z)){ 
Z = cbind(Z)
if(nrow(Z) != n) stop("length of Z should be same as row of X or Y")
}

if(!is.null(inX)){
extest = unlist(lapply(1:Xnb, function(a) inX[[a]] %in% (1:Xps[a]) ))
if(!(all(extest))) stop("inX should be specified the column number of X")
}
if(!is.null(inY)){
extest = unlist(lapply(1:Ynb, function(a) inY[[a]] %in% (1:Yps[a]) ))
if(!(all(extest))) stop("inY should be specified the column number of Y")
}

if(muX < 0) stop("muX should be positive")
if(muY < 0) stop("muY should be positive")

########## Start ##########
X = lapply(X, function(x) scale(x, scale=scaling))
Y = lapply(Y, function(x) scale(x, scale=scaling))
if(!is.null(Z)) Z = scale(Z, scale=scaling)

Xscale = lapply(X, attributes)
Yscale = lapply(Y, attributes)

if(scaling){
for(x in 1:length(Xscale)) Xscale[[x]]$`scaled:scale` = ifelse(Xscale[[x]]$`scaled:scale`==0, 1, Xscale[[x]]$`scaled:scale`)
for(x in 1:length(Yscale)) Yscale[[x]]$`scaled:scale` = ifelse(Yscale[[x]]$`scaled:scale`==0, 1, Yscale[[x]]$`scaled:scale`)
}

X = lapply(X, function(x) apply(x, 2, function(x2){x2[is.na(x2)] = 0; x2}))
Y = lapply(Y, function(y) apply(y, 2, function(y2){y2[is.na(y2)] = 0; y2}))

tvarX = lapply(X, function(x) sum(x^2))
tvarY = lapply(Y, function(x) sum(x^2))

############### Iteration for components Start ###############
Xd = X; Yd = Y; out = ssX = ssY = wsX = wsY = cpevX = cpevY = predictiv = list(); bic = bicX = bicY = reproduct = rep(0, comp)
bic2 = bic2X = bic2Y = lapply(1:comp, function(x) {tb=rep(0, comp2); names(tb)=paste0("comp", 1:comp2); tb})
out2 = lapply(1:comp, function(x) lapply(1:comp2, function(y) NULL))

for(ncomp in 1:comp){

##### Fit #####
out[[ncomp]] = out1 = .msma_onecomp_extended(Xd, Yd, Z, lambdaX, lambdaY, lambdaXsup, lambdaYsup, eta, type, inX, inY, inXsup, inYsup, muX, muY, dmode, verbose, intseed, ceps, con4spv)

##### Nest #####
sbXd = sbX0 = list(do.call(cbind, out1$sbX))
sbYd = sbY0 = list(do.call(cbind, out1$sbY))

if(sprmethod=="PCA"){

for(ncomp2 in 1:comp2){

if(ncomp2 == 1){
out21 = out1
out21$wbX[[1]] = out1$wsX; out21$wbY[[1]] = out1$wsY
out2[[ncomp]][[ncomp2]] = out21
}else{
out2[[ncomp]][[ncomp2]] = out21 = .msma_onecomp_extended(sbXd, sbYd, Z, lambdaXsup, lambdaYsup, NULL, NULL, eta, type, inXsup, inYsup, NULL, NULL, muX, muY, dmode, verbose, intseed, ceps, con4spv)
}

# arrange 
ssX = do.call(cbind, lapply(out2[[ncomp]], function(y) y$ssX))
ssY = do.call(cbind, lapply(out2[[ncomp]], function(y) y$ssY))

# Prediction 
predSX = ssX %*% project(sbX0[[1]], ssX)
predSY = ssY %*% project(sbY0[[1]], ssY)

# Deflation
sbXd = lapply(sbX0, function(x) x - predSX)
sbYd = lapply(sbY0, function(x) x - predSY)

# arrange 
wsX = lapply(out2[1:ncomp], function(x) do.call(cbind, lapply(x, function(y) y$wbX[[1]])))
wsY = lapply(out2[1:ncomp], function(x) do.call(cbind, lapply(x, function(y) y$wbY[[1]])))

nzwsX = lapply(wsX, function(x) apply(x, 2, function(z2) sum(z2!=0)) )
nzwsY = lapply(wsY, function(x) apply(x, 2, function(z2) sum(z2!=0)) )

# Information Criteria 
if(dmode == "PCA"){ 
tmpse = sum(unlist(lapply(sbXd, function(x) sum(x^2))))
#print(tmpse)
bic2[[ncomp]][ncomp2] = log(tmpse/(n*Xnb)) + log(n*Xnb)/(n*Xnb)*(sum(unlist(nzwsX)))
}else
{
tmpse = sum(unlist(lapply(sbYd, function(x) sum(x^2))))
bic2Y[[ncomp]][ncomp2] = log(tmpse/(n*Ynb)) + log(n*Ynb)/(n*Ynb)*(sum(unlist(nzwsY)))
tmpse = sum(unlist(lapply(sbXd, function(x) sum(x^2))))
bic2X[[ncomp]][ncomp2] = log(tmpse/(n*Xnb)) + log(n*Xnb)/(n*Xnb)*(sum(unlist(nzwsX)))
}
}

}else if(sprmethod%in%c("NMF","sNMF")){

V0 = sbXd[[1]]
if(nneg == "posneg"){ V1 = do.call(cbind, lapply(c(1, -1), function(s1) apply(V0, 2, function(v){ ifelse(sign(v)==s1, v*s1, 0) }))) 
}else if(nneg == "absolute"){ V1 = abs(V0)
}else if(nneg == "min"){ V1 = apply(V0, 2, function(v){ x=v-min(v); x/max(x)}) }

if(sprmethod=="NMF"){
NMFout = sprNMF(V1, comp2, seed=intseed)
}else{
NMFout = sprNMF(V1, comp2, lambdaXsup, seed=intseed)
}
ssX[[ncomp]] = ssY[[ncomp]] = NMFout$Basis
wsX[[ncomp]] = wsY[[ncomp]] = t(NMFout$Encoding)

nzwsX = lapply(wsX, function(x) apply(x, 2, function(z2) sum(z2!=0)) )
nzwsY = lapply(wsY, function(x) apply(x, 2, function(z2) sum(z2!=0)) )

for(ncomp2 in 1:comp2){
predSX = ssX[[ncomp]][,1:ncomp2,drop=FALSE] %*% project(V0, ssX[[ncomp]][,1:ncomp2,drop=FALSE])
sbXd = lapply(sbX0, function(x) x - predSX)
tmpse = sum(unlist(lapply(sbXd, function(x) sum(x^2))))
bic2[[ncomp]][ncomp2] = log(tmpse/(n*Xnb)) + log(n*Xnb)/(n*Xnb)*(sum(unlist(nzwsX)[1:ncomp2]))
}

}


##### arrange #####
sbX = lapply(1:Xnb, function(x){ do.call(cbind, lapply(out, function(y) y$sbX[[x]]))})
sbY = lapply(1:Ynb, function(x){ do.call(cbind, lapply(out, function(y) y$sbY[[x]]))})

wbX = lapply(1:Xnb, function(x){ 
tmp = do.call(cbind, lapply(out, function(y) y$wbX[[x]]))
colnames(tmp) = paste("comp", 1:ncol(tmp), sep="")
if(is.null(colnames(X[[x]]))){rownames(tmp) = paste("X", x, 1:ncol(X[[x]]), sep=".")}else{rownames(tmp) = colnames(X[[x]])}
tmp
})
names(sbX) = names(wbX) = Xbnames

wbY = lapply(1:Ynb, function(x){ 
tmp = do.call(cbind, lapply(out, function(y) y$wbY[[x]]))
colnames(tmp) = paste("comp", 1:ncol(tmp), sep="")
if(is.null(colnames(Y[[x]]))){rownames(tmp) = paste("Y", x, 1:ncol(Y[[x]]), sep=".")}else{rownames(tmp) = colnames(Y[[x]])}
tmp
})
names(sbY) = names(wbY) = Ybnames

##### Number of nonzero #####
nzwbX = do.call(rbind, lapply(wbX, function(z) apply(z, 2, function(z2) sum(z2!=0))))
nzwbY = do.call(rbind, lapply(wbY, function(z) apply(z, 2, function(z2) sum(z2!=0))))

##### Prediction #####
predX = lapply(1:Xnb, function(x) sbX[[x]] %*% project(X[[x]], sbX[[x]]))
predY = lapply(1:Ynb, function(x) sbY[[x]] %*% project(Y[[x]], sbY[[x]]))

cpevX[[ncomp]] = sapply(1:Xnb, function(x) sum(predX[[x]]^2)/tvarX[[x]] )
cpevY[[ncomp]] = sapply(1:Ynb, function(x) sum(predY[[x]]^2)/tvarY[[x]] )

##### Deflation #####
if(defmethod == "canonical"){
Xd = lapply(1:Xnb, function(x) X[[x]] - predX[[x]])
Yd = lapply(1:Ynb, function(x) Y[[x]] - predY[[x]])
}else
if(defmethod == "canonical2"){
Xd = lapply(1:Xnb, function(x) Xd[[x]] - predX[[x]])
Yd = lapply(1:Ynb, function(x) Yd[[x]] - predY[[x]])
}else
if(defmethod == "pls"){
Xd = lapply(1:Xnb, function(x) X[[x]] - predX[[x]])
Yd = lapply(1:Ynb, function(x) Y[[x]] - predY[[x]])
}

##### Predictivity and Reproductivity #####
reproduct[ncomp] = cor(unlist(lapply(Y, c)), unlist(lapply(predY, c)))^2
predictiv[[ncomp]] = cor(out1$ssY, Z)^2

##### Information Criteria #####
if(dmode == "PCA"){ 
tmpse = sum(unlist(lapply(Xd, function(x) sum(x^2))))
#print(tmpse)
bic[ncomp] = log(tmpse/(n*sum(Xps))) + log(n*sum(Xps))/(n*sum(Xps))*(sum(nzwbX))
}else
{
#tmpse = sum(unlist(lapply(Xd, function(x) sum(x^2)))) + sum(unlist(lapply(Yd, function(x) sum(x^2))))
tmpse = sum(unlist(lapply(Yd, function(x) sum(x^2))))
bicY[ncomp] = log(tmpse/(n*sum(Yps))) + log(n*sum(Yps))/(n*sum(Yps))*(sum(nzwbY))
#bic[ncomp] = log(tmpse/(n*sum(c(Xps,Yps)))) + log(n*sum(c(Xps,Yps)))/(n*sum(c(Xps,Yps)))*(sum(c(nzwbX,nzwbY)))
tmpse = sum(unlist(lapply(Xd, function(x) sum(x^2))))
bicX[ncomp] = log(tmpse/(n*sum(Xps))) + log(n*sum(Xps))/(n*sum(Xps))*(sum(nzwbX))
}

}
############### Iteration for components End ###############

##### arrange #####
if(sprmethod=="PCA"){
ssX = lapply(out2, function(x) do.call(cbind, lapply(x, function(y) y$ssX)))
ssY = lapply(out2, function(x) do.call(cbind, lapply(x, function(y) y$ssY)))
}

names(ssY) = names(ssX) = paste("comp", 1:comp, sep="")
for(i in 1:length(wsY)){ colnames(ssY[[i]]) = paste("comp", 1:comp2, sep="") }
for(i in 1:length(wsX)){ colnames(ssX[[i]]) = paste("comp", 1:comp2, sep="") }

cpevX = do.call(cbind, cpevX)
cpevY = do.call(cbind, cpevY)

if(ncomp==1){
avX = cpevX; avY = cpevY
}else
if(ncomp==2){
avX = cbind(cpevX[,1], cpevX[,2]-cpevX[,1])
avY = cbind(cpevY[,1], cpevY[,2]-cpevY[,1])
}else{
avX = cbind(cpevX[,1], t(apply(cpevX, 1, diff)))
avY = cbind(cpevY[,1], t(apply(cpevY, 1, diff)))
}
colnames(cpevY) = colnames(cpevX) = colnames(avY) = colnames(avX) = colnames(nzwbX) = colnames(nzwbY) = paste("comp", 1:comp, sep="")
rownames(cpevX) = rownames(avX) = rownames(nzwbX) = Xbnames
rownames(cpevY) = rownames(avY) = rownames(nzwbY) = Ybnames

names(wsY) = names(wsX) = paste("comp", 1:comp, sep="")
for(i in 1:length(wsY)){ 
colnames(wsY[[i]]) = paste("comp", 1:comp2, sep="")
if(sprmethod%in%c("NMF","sNMF") & nneg == "posneg"){
rownames(wsY[[i]]) = paste(Ybnames, rep(c("pos", "neg"),each=Ynb), sep="")
}else{
rownames(wsY[[i]]) = Ybnames
}
}
for(i in 1:length(wsX)){ 
colnames(wsX[[i]]) = paste("comp", 1:comp2, sep="")
if(sprmethod%in%c("NMF","sNMF") & nneg == "posneg"){
rownames(wsX[[i]]) = paste(Xbnames, rep(c("pos", "neg"),each=Xnb), sep="")
}else{
rownames(wsX[[i]]) = Xbnames
}
}

names(nzwsX) = names(nzwsY) = paste("comp", 1:comp, sep="")
for(i in 1:length(nzwsX)) names(nzwsX[[i]]) = paste("comp", i, "-", 1:comp2, sep="") 
for(i in 1:length(nzwsY)) names(nzwsY[[i]]) = paste("comp", i, "-", 1:comp2, sep="") 

names(bic) = names(bicX) = names(bicY) = names(bic2) = paste("comp", 1:comp, sep="")
if(comp2 == 1) bic2 = unlist(bic2)

##### Names of variables #####
selectXnames = lapply(1:length(wbX), function(z) apply(wbX[[z]], 2, function(z2){ 
if(is.null(colnames(X[[z]]))) colnames(X[[z]]) = paste("X", z, 1:ncol(X[[z]]), sep=".")
colnames(X[[z]])[z2!=0]
}))
selectYnames = lapply(1:length(wbY), function(z) apply(wbY[[z]], 2, function(z2){ 
if(is.null(colnames(Y[[z]]))) colnames(Y[[z]]) = paste("Y", z, 1:ncol(Y[[z]]), sep=".")
colnames(Y[[z]])[z2!=0]
}))

##### Output #####
if(dmode == "PCA"){ 
out = list(call = match.call(), 
dmode=dmode,
X=X, Z=Z, Xscale=Xscale, comp = c(comp, comp2),
muX=muX, 
wbX=wbX, sbX=sbX, ssX=ssX, wsX=wsX,
nzwbX = nzwbX, nzwsX=nzwsX,
selectXnames = selectXnames,
cpevX = cpevX,
avX = avX,
bic = bic, bic2=bic2,
reproduct = reproduct, predictiv=predictiv
)
}else
{
out = list(call = match.call(), 
dmode=dmode,
X=X, Y=Y, Z=Z,
Xscale=Xscale, Yscale=Yscale, comp = c(comp, comp2),
muX=muX, muY=muY,
wbX=wbX, sbX=sbX, wbY=wbY, sbY=sbY, ssX=ssX, wsX=wsX, ssY=ssY, wsY=wsY, 
nzwbX = nzwbX, nzwbY = nzwbY, nzwsX=nzwsX, nzwsY=nzwsY,
selectXnames = selectXnames, selectYnames = selectYnames,
cpevX = cpevX, cpevY = cpevY,
avX = avX, avY = avY,
bic = bicY, bicX = bicX, bicY = bicY,
bic2 = bic2Y, bic2X = bic2X, bic2Y = bic2Y,
reproduct = reproduct, predictiv=predictiv
)
}
class(out) = "msma"
out
}

#' @rdname msma
#' @method print msma
#' @export
print.msma = function(x, ...)
{
cat("Call:\n")
print(x$call)
cat("\n")
cat("Numbers of non-zeros for X block: \n")
print(x$nzwbX)
cat("\n")
cat("Numbers of non-zeros for X super: \n")
nzwsX = do.call(cbind,x$nzwsX)
print(nzwsX)
cat("\n")
if(x$dmode == "PLS"){
cat("Numbers of non-zeros for Y block: \n")
print(x$nzwbY)
cat("\n")
cat("Numbers of non-zeros for Y super: \n")
nzwsY = do.call(cbind,x$nzwsY)
print(nzwsY)
cat("\n")
}
}

#' Summarizing Fits
#'
#' summary method for class "msma". 
#'
#' This function provide the summary of results .
#'
#' @name summary.msma
#' @aliases summary.msma
#' @rdname summary.msma
#' @method summary msma
#' @docType methods
#' @export
#'
#' @param object,x an object of class "\code{msma}", usually, a result of a call to \code{\link{msma}}
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#'##### data #####
#'tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = 20, seed=1)
#'X = tmpdata$X; Y = tmpdata$Y 
#'
#'##### One Component #####
#'fit1 = msma(X, Y, comp=1, lambdaX=2, lambdaY=1:3)
#'summary(fit1)
#'
summary.msma = function(object, ...)
{
if(object$dmode == "PLS"){
test = predict(object, newX=object$X, newY=object$Y)
err = (matserr(object$X, test$X) + matserr(object$Y, test$Y))/2
}else{
test = predict(object, newX=object$X, newY=NULL)
err = matserr(object$X, test$X)
}

res = list(call=object$call, err=err)
class(res) = "summary.msma"
res
}

#' @rdname summary.msma
#' @method print summary.msma
#' @family print
#' @export
print.summary.msma = function(x, ...)
{
cat("Call:\n")
print(x$call)
cat("\n")
cat("Error : ")
cat(round(x$err, 3))
cat("\n")
}

#' Plot msma
#'
#' plot method for class "msma". 
#'
#' This function provides a plot of results.
#'
#' @name plot.msma
#' @aliases plot.msma
#' @rdname plot.msma
#' @method plot msma
#' @docType methods
#' @export
#'
#' @param x an object of class "\code{msma}." Usually, a result of a call to \code{\link{msma}}
#' @param v a character, "weight" for the weight, "score" for the score, and "cpev" for the cumulative percentage of explained variance (CPEV) .
#' @param axes a numeric (or vector), specifying the root component(s) to plot. 
#' @param axes2 a numeric (or vector), specifying the nested component(s) to plot. 
#' @param block a character, indicating which the "block" or "super" is used. 
#' @param plottype a character, indicating the plot type. "bar" for the bar plot, "scatter" for the scatter plot.
#' @param XY a character, indicating "X" or "Y". "XY" for the scatter plots using X and Y scores from PLS.
#' @param col a color vector.
#' @param signflip a numeric vector if the sign in the block is flipped to pose the super as possitive.
#' @param xlim a numeric vector x coordinate ranges.
#' @param ylim a numeric vector y coordinate ranges.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#'tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = 20, seed=1)
#'X = tmpdata$X; Y = tmpdata$Y 
#'
#'fit1 = msma(X, Y, comp=1, lambdaX=2, lambdaY=1:3)
#'#plot(fit1)
#'
plot.msma = function(x, v=c("weight", "score", "cpev")[1], axes = 1, axes2 = 1, block=c("block", "super")[1], plottype=c("bar", "scatter")[1], XY=c("X", "Y", "XY")[1], col=NULL, signflip=FALSE, xlim=NULL, ylim=NULL,...) {

### ###
if(length(axes) > 2) stop("the length of axes should be less than 2.")
if(length(axes2) > 2) stop("the length of axes should be less than 2.")

if(x$comp[1] == 1){
axes = 1
if(plottype == "scatter") stop("number of components should be more than 2")
}

### ###
if(XY == "XY"){
if(length(axes) != 1) stop("the length of axes should be 1.")
v1="s"
s1 = do.call(cbind, lapply(x[[paste0(v1, "sX")]][axes], function(x2) x2[, axes2,drop=FALSE]))
s2 = do.call(cbind, lapply(x[[paste0(v1, "sY")]][axes], function(x2) x2[, axes2,drop=FALSE]))
cor1 = cor.test(s1, s2)
m1 = round(max(abs(range(c(s1, s2)))))
if(is.null(xlim)) xlim=c(-m1, m1)
if(is.null(ylim)) ylim=c(-m1, m1)
plot(s1, s2, xlab="X score", ylab="Y score", main=paste("Component", axes[1]), sub=paste0("r=", round(cor1$estimate,2), " (p", ifelse(cor1$p.value>0.0001, "=", ""), format.pval(round(cor1$p.value, 4), eps=0.0001, scientific=FALSE), ")"), xlim=xlim, ylim=ylim, ...)
abline(lm(s2 ~ s1), col=2)
}else{
### ###
if(v == "cpev"){
if(is.null(ylim)) ylim=c(0,1)
matplot(t(x$cpevX), type="b", xlab="Components", ylab=v, ylim=ylim, ...)
if(x$dmode == "PLS"){
matplot(t(x$cpevY), type="b", lty=2, add = TRUE)
}
}else{
### ###
if(v == "weight"){v1="w"; t1="n"}else{v1="s"; t1="p"}
nv1 = unlist(lapply(x[[paste0(v1, "b", XY)]], nrow))
b1 = lapply(x[[paste0(v1, "b", XY)]], function(b10) b10[, axes,drop=FALSE])
s1 = do.call(cbind, lapply(x[[paste0(v1, "s", XY)]][axes], function(x2) x2[, axes2,drop=FALSE]))
if(signflip){ for(i in 1:length(b1)){ for(j in 1:length(axes)){ b1[[i]][,j] = sign(s1)[i,j] * b1[[i]][,j] }} }

### ###
if(block == "block"){
tmpvar = do.call(rbind, b1)
width1 = rep(1, nrow(tmpvar))
if(is.null(col) | length(col)!=length(nv1)){col1 = rep(gray(1:length(nv1)/length(nv1)), nv1)}else{col1 = rep(col, nv1)}
}else{
tmpvar = s1
if(signflip) tmpvar = abs(tmpvar)
width1 = nv1
if(is.null(col) | length(col)!=length(nv1)){col1 = rep("gray", nrow(tmpvar))}else{col1 = col}
}

### ###
m1 = round(max(abs(range(tmpvar))))
if(is.null(xlim)) xlim=c(-m1, m1)
if(is.null(ylim)) ylim=c(-m1, m1)
if(plottype == "scatter"){
if(length(axes) != 2) stop("the length of axes should be 2.")
if(is.null(col)){ col1=1 }else{ col1=col}
plot(tmpvar, xlab=paste("Component", axes[1]), ylab=paste("Component", axes[2]), xlim=xlim, ylim=ylim, type=t1,  main=block, col=col1,...)
abline(h = 0, lty=2); abline(v = 0, lty=2);
if(v == "weight"){for(i in 1:nrow(tmpvar)) text(tmpvar[i,1], tmpvar[i,2], rownames(tmpvar)[i])}
}else if(plottype == "bar"){
#par(mfrow=c(1, length(axes)), mar = c(4,max(nchar(rownames(tmpvar))),4,4))
if(x$comp[2] > 1){ main1 = paste("(", "Nest", axes2, ")")}else{main1 = ""}
mar1 = ifelse(is.null(rownames(tmpvar)), 4, max(nchar(rownames(tmpvar))))
par(mar = c(3, mar1, 3, 2))
for(i in 1:length(axes)){ 
barplot(tmpvar[,i], width1, main=paste("Component", axes[i], main1), ylab=paste(block, v), ylim=ylim, las=1, col=col1, space=0.1,...)
}
}

}
}
}

#' Prediction
#'
#' predict method for class "msma". 
#'
#' This function produces a prediction from new data based on \code{\link{msma}} fit. It is mainly used in cross-validation
#'
#' @name predict.msma
#' @aliases predict.msma
#' @rdname predict.msma
#' @method predict msma
#' @docType methods
#' @export
#'
#' @param object an object of class "\code{msma}", usually, a result of a call to \code{\link{msma}}
#' @param newX a matrix in which to look for variables with which to predict X.
#' @param newY a matrix in which to look for variables with which to predict Y.
#' @param ... further arguments passed to or from other methods.
#'
#' @return \item{X}{predicted X}
#' @return \item{sbX}{block score for X}
#' @return \item{Y}{predicted Y}
#' @return \item{sbY}{block score for Y}
#'
#' @examples
#' ##### data #####
#' tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = 20, seed=1)
#' X = tmpdata$X; Y = tmpdata$Y 
#' 
#' ##### Two Component #####
#' fit2 = msma(X, Y, comp=2, lambdaX=2, lambdaY=1:3)
#' summary(fit2)
#' 
#' ##### Predict #####
#' test = predict(fit2, newX=X, newY=Y)
#' 
predict.msma = function(object, newX, newY=NULL,...){
##### Requirement #####
if(missing(newX)) stop("newX should be specified")
#if(missing(newY)) stop("newY should be specified")

if(is.null(newY) | (object$dmode == "PCA")) newY = newX

if(!inherits(newX, "list")) newX = list(newX)
if(!inherits(newY, "list")) newY = list(newY)

if(!inherits(newX, "list")) stop("newX should be a list")
if(!inherits(newY, "list")) stop("newY should be a list")

Ynb = length(newY)
Xnb = length(newX)

########## Start ##########
#newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`, scale=object$Xscale[[x]]$`scaled:scale`))
newX = lapply(1:length(newX), function(x) scale(newX[[x]], center=object$Xscale[[x]]$`scaled:center`))
newX = lapply(newX, function(x) apply(x, 2, function(x2){ x2[is.na(x2)] = 0; x2}))
sbX = lapply(1:Xnb, function(x) newX[[x]] %*% object$wbX[[x]] )
predX = lapply(1:Xnb, function(x) cbind(sbX[[x]]) %*% project(newX[[x]], sbX[[x]]))

if(!(is.null(newY) | object$dmode == "PCA")) {
#newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`, scale=object$Yscale[[x]]$`scaled:scale`))
newY = lapply(1:length(newY), function(x) scale(newY[[x]], center=object$Yscale[[x]]$`scaled:center`))
newY = lapply(newY, function(y) apply(y, 2, function(y2){ y2[is.na(y2)] = 0; y2}))
sbY = lapply(1:Ynb, function(x) newY[[x]] %*% object$wbY[[x]] )
predY = lapply(1:Ynb, function(x) cbind(sbY[[x]]) %*% project(newY[[x]], sbY[[x]]))
}else
{
predY = sbY = NULL
}

##### Output #####
list(X = predX, sbX = sbX, Y = predY, sbY = sbY)
}

#' Cross-Validation
#'
#' cross-validated method to evaluate the fit of "msma". 
#'
#' k-fold cross-validation for \code{msma}
#'
#' @name cvmsma
#' @aliases cvmsma
#' @rdname cvmsma
#' @docType methods
#' @export
#'
#' @param X a (list of) matrix, explanatory variable(s).
#' @param Y a (list of) matrix, objective variable(s).
#' @param Z a (list of) matrix, response variable(s).
#' @param muX a numeric scalar for the weight of X for the supervised. 
#' @param muY a numeric scalar for the weight of Y for the supervised. 
#' @param comp numeric scalar for the maximum number of componets to be considered.
#' @param lambdaX numeric vector of regularized parameters for X with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param lambdaY numeric vector of regularized parameters for Y with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param lambdaXsup numeric vector of regularized parameters for the super weight of X with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param lambdaYsup numeric vector of regularized parameters for the super weight of Y with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param eta numeric scalar the parameter indexing the penalty family.
#' @param type a character. 
#' @param inX a (list of) numeric vector to specify the variables of X which are always in the model. 
#' @param inY a (list of) numeric vector to specify the variables of X which are always in the model. 
#' @param inXsup a (list of) numeric vector to specify the blocks of X which are always in the model. 
#' @param inYsup a (list of) numeric vector to specify the blocks of Y which are always in the model. 
#' @param nfold number of folds - default is 5. 
#' @param seed number of seed for the random number. 
#' @param intseed seed number for the random number in the parameter estimation algorithm.
#' @param sprmethod a character specifying the super-level decomposition method: PCA, NMF, sNMF, or CMP.
#' @param nneg a character specifying the non-negative transformation: posneg, absolute, or min.
#' @param con4spv a numeric vector of weights used to combine multiple columns of Z. If NULL, equal weights are used.
#' 
#' @return \item{err}{The mean cross-validated errors which has three elements consisting of the mean of errors for X and Y, the errors for X and for Y.}
#'
#' @examples
#'##### data #####
#'tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = 20, seed=1)
#'X = tmpdata$X; Y = tmpdata$Y 
#'
#'##### One Component CV #####
#'cv1 = cvmsma(X, Y, comp = 1, lambdaX=2, lambdaY=1:3, nfold=5, seed=1)
#'cv1
#'
#'##### Two Component CV #####
#'cv2 = cvmsma(X, Y, comp = 2, lambdaX=2, lambdaY=1:3, nfold=5, seed=1)
#'cv2
#'
cvmsma = function(X, Y=NULL, Z=NULL, comp=1, lambdaX, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, nfold=5, seed=1, intseed=1, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1], con4spv=NULL){
##### Requirement #####
if(missing(X)) stop("X should be specified")
if(is.null(Y)){ 
dmode = "PCA"
Y = X
if(!is.null(lambdaY)) stop("lambdaY can not be specified")
}else{
dmode = "PLS"
}

if(!inherits(X, "list")) X = list(X)
if(!inherits(Y, "list")) Y = list(Y)
if(!inherits(X, "list")) stop("X should be a list")
if(!inherits(Y, "list")) stop("Y should be a list")

Ynb = length(Y)
Xnb = length(X)

if(length(lambdaX) == 1) lambdaX = rep(lambdaX, Xnb)
if(length(lambdaY) == 1) lambdaY = rep(lambdaY, Ynb)

if(!is.null(lambdaX)){if(length(lambdaX) != Xnb) stop("lambdaX should be same length as the number of blocks of X")}
if(!is.null(lambdaY)){if(length(lambdaY) != Ynb) stop("lambdaY should be same length as the number of blocks of Y")}

if(!is.null(inX)){if(!inherits(inX, "list")) inX = list(inX)}
if(!is.null(inY)){if(!inherits(inY, "list")) inY = list(inY)}

Yps = unlist(lapply(Y, ncol))
Xps = unlist(lapply(X, ncol))

if(!is.null(inX)){
extest = unlist(lapply(1:Xnb, function(a) inX[[a]] %in% (1:Xps[a]) ))
if(!(all(extest))) stop("inX should be specified the column number of X")
}
if(!is.null(inY)){
extest = unlist(lapply(1:Ynb, function(a) inY[[a]] %in% (1:Yps[a]) ))
if(!(all(extest))) stop("inY should be specified the column number of Y")
}

########## Start ##########
##### Set Up #####
n = nrow(Y[[1]])
id = 1:n
divid = cut(id, c(-Inf, floor(quantile(id, seq(1/nfold, 1-1/nfold, length=(nfold-1)))), Inf), labels = FALSE)
set.seed(seed); fold = sample(divid)

##### Cross-Validation #####
cv.err = sapply(1:nfold, function(i){

tmpX = lapply(X, function(tmpset) tmpset[!(fold == i),])
testX = lapply(X, function(testset) testset[(fold == i),])

if(dmode == "PCA"){
tmpY = testY = NULL
}else
{
tmpY = lapply(Y, function(tmpset) tmpset[!(fold == i),])
testY = lapply(Y, function(testset) testset[(fold == i),])
}

if(is.null(Z)){
tmpZ = testZ = NULL
}else{
if(!inherits(Z, "matrix")) dim(Z) = c(length(Z),1)
tmpZ = Z[!(fold == i),,drop=FALSE]
testZ = Z[(fold == i),,drop=FALSE]
}

fit = msma(tmpX, tmpY, tmpZ, comp, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta, type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg, con4spv=con4spv)
test = predict(fit, newX=testX, newY=testY)

#testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`, scale=fit$Xscale[[x]]$`scaled:scale`))
testX = lapply(1:length(testX), function(x) scale(testX[[x]], center=fit$Xscale[[x]]$`scaled:center`))
Xerr = matserr(testX, test$X)
out = Xerr

if(dmode == "PLS"){
testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`, scale=fit$Yscale[[x]]$`scaled:scale`))
testY = lapply(1:length(testY), function(x) scale(testY[[x]], center=fit$Yscale[[x]]$`scaled:center`))
Yerr = matserr(testY, test$Y)
out = c(mean(c(Xerr, Yerr)), Xerr, Yerr)
}

out
})

if(dmode == "PLS"){ 
err = rowMeans(cv.err)
names(err) = c("Average", "X", "Y")
}else
{
err = mean(cv.err)
}
err
##### Output #####
list(err = err)
}


#' Search for Number of Components
#'
#' Determination of the number of components based on cross-validated method or Bayesian information criterion (BIC)
#'
#' This function searches for the optimal number of components.
#'
#' @name ncompsearch
#' @aliases ncompsearch
#' @rdname ncompsearch
#' @docType methods
#' @export
#'
#' @param X a matrix or list of matrices indicating the explanatory variable(s). This parameter is required.
#' @param Y a matrix or list of matrices indicating objective variable(s). This is optional. If there is no input for Y, then PCA is implemented.
#' @param Z a vector, response variable(s) for implementing the supervised version of (multiblock) PCA or PLS. This is optional. The length of Z is the number of subjects. If there is no input for Z, then unsupervised PLS/PCA is implemented.
#' @param comps numeric vector for the maximum numbers of componets to be considered.
#' @param lambdaX numeric vector of regularized parameters for X, with a length equal to the number of blocks. If lambdaX is omitted, no regularization is conducted.
#' @param lambdaY numeric vector of regularized parameters for Y, with a length equal to the number of blocks. If lambdaY is omitted, no regularization is conducted.
#' @param lambdaXsup numeric vector of regularized parameters for the super weight of X with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param lambdaYsup numeric vector of regularized parameters for the super weight of Y with length equal to the number of blocks. If omitted, no regularization is conducted.
#' @param eta numeric scalar indicating the parameter indexing the penalty family. This version contains only choice 1.
#' @param type a character, indicating the penalty family. In this version, only one choice is available: "lasso."
#' @param inX a (list of) numeric vector to specify the variables of X which are always in the model. 
#' @param inY a (list of) numeric vector to specify the variables of X which are always in the model. 
#' @param inXsup a (list of) numeric vector to specify the blocks of X which are always in the model. 
#' @param inYsup a (list of) numeric vector to specify the blocks of Y which are always in the model. 
#' @param muX a numeric scalar for the weight of X for the supervised case. 0 <= muX <= 1.
#' @param muY a numeric scalar for the weight of Y for the supervised case. 0 <= muY <= 1. 
#' @param nfold number of folds - default is 5. 
#' @param x an object of class "\code{ncompsearch}", usually, a result of a call to \code{\link{ncompsearch}}
#' @param regpara logical, If TRUE, the regularized parameters search is also conducted simultaneously.
#' @param maxrep numeric scalar for the number of iteration.
#' @param minpct minimum candidate parameters defined as a percentile of automatically determined (possible) candidates.
#' @param maxpct maximum candidate parameters defined as a percentile of automatically determined (possible) candidates.
#' @param criterion a character, the evaluation criterion, "CV" for cross-validation, based on a matrix element-wise error, and "BIC" for Bayesian information criteria. The "BIC" is the default.
#' @param whichselect which blocks selected.
#' @param intseed seed number for the random number in the parameter estimation algorithm.
#' @param sprmethod a character specifying the super-level decomposition method: PCA, NMF, sNMF, or CMP.
#' @param nneg a character specifying the non-negative transformation: posneg, absolute, or min.
#' @param cidx Parameters used in the plot function to specify whether block or super is used. 1=block (default), 2=super.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return \item{comps}{numbers of components}
#' @return \item{mincriterion}{minimum criterion values}
#' @return \item{criterions}{criterion values}
#' @return \item{optncomp}{optimal number of components based on minimum cross-validation error}
#' 
#' @examples
#' ##### data #####
#' tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = 20, seed=1)
#' X = tmpdata$X; Y = tmpdata$Y 
#' 
#' ##### number of components search #####
#' ncomp1 = ncompsearch(X, Y, comps = c(1, 5, 10*(1:2)), nfold=5)
#' #plot(ncomp1)
#'
ncompsearch = function(X, Y=NULL, Z=NULL, comps=1:3, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, nfold=5, regpara=FALSE, maxrep=3, minpct=0, maxpct=1, criterion=c("CV", "BIC")[1], whichselect=NULL, intseed=1, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1]){

if(!inherits(comps, "list")){ comps = list(comps)}
if(length(comps)<2) comps[[2]] = 1
if(criterion=="CV") comps[[2]] = 1
comps2 = expand.grid(comps)

if(criterion=="BIC" & !regpara){
ncomps = unlist(lapply(comps, max))
msmaout = msma(X, Y, Z, comp = ncomps, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg)
cves = msmaout[c("bic","bic2")]
comps = lapply(ncomps, function(nc) 1:nc)
}else{
cv2 = apply(comps2, 1, function(x) {
if(regpara){
regpout = regparasearch(X, Y, Z, eta=eta, type=type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, comp=x, nfold=nfold, maxrep=maxrep, minpct=minpct, maxpct=maxpct, criterion=criterion, whichselect=whichselect, intseed=intseed, sprmethod=sprmethod, nneg=nneg)
cve = regpout$mincriterion; lambdaX = regpout$optlambdaX; lambdaY = regpout$optlambdaY; lambdaXsup = regpout$optlambdaXsup; lambdaYsup = regpout$optlambdaYsup
}else
{
if(criterion=="CV"){
cve = cvmsma(X, Y, Z, comp=x, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, nfold=nfold, intseed=intseed, sprmethod=sprmethod, nneg=nneg)$err[1]
}else if(criterion=="BIC"){
msmaout = msma(X, Y, Z, comp=x, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup, eta=eta, type=type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg)
cve = unlist(lapply(msmaout[c("bic","bic2")], function(bic) bic[length(bic)]))
}
}
list(cve = cve, lambdaX=lambdaX, lambdaY=lambdaY, lambdaXsup=lambdaXsup, lambdaYsup=lambdaYsup)
})
if(criterion=="CV"){
cves = list(unlist(lapply(cv2, function(x) x$cve)), rep(1,length(cv2)))
}else{
cves = lapply(1:2, function(x) unlist(lapply(cv2, function(y) y[x])))
}
}

optidx = which.min(cves[[1]])
optidx = c(optidx, which.min(cves[[2]][[optidx]]))
optncomp = unlist(lapply(1:2, function(c1) comps[[c1]][optidx[c1]]))
mincriterion = c(cves[[1]][optidx[1]], cves[[2]][optidx[2]])

#RRC = exp(diff(log(exp(diff(log(cves))))))

if(criterion=="BIC" & !regpara){
optlambdaX = lambdaX; optlambdaY = lambdaY; optlambdaXsup = lambdaXsup; optlambdaYsup=lambdaYsup
}else{
optidx2 = which(apply(comps2,1,paste,collapse="-")==paste(optncomp,collapse="-"))
optlambdaX = cv2[[optidx2]]$lambdaX; optlambdaY = cv2[[optidx2]]$lambdaY
optlambdaXsup = cv2[[optidx2]]$lambdaXsup; optlambdaYsup = cv2[[optidx2]]$lambdaYsup
}

out=list(criterion=criterion, comps=comps, mincriterion=mincriterion, criterions=cves, optncomp=optncomp, optlambdaX = optlambdaX, optlambdaY = optlambdaY, optlambdaXsup=optlambdaXsup, optlambdaYsup=optlambdaYsup)
class(out) = "ncompsearch"
out
}

#' @rdname ncompsearch
#' @method print ncompsearch
#' @export
print.ncompsearch = function(x, ...)
{
cat("Optimal number of components: ")
cat(paste(paste0("(", c("block", "super"), ")"), x$optncomp, collapse=", "))
cat("\n")
cat("Criterion: ")
cat(x$criterion)
cat("\n")
}

#' @rdname ncompsearch
#' @method plot ncompsearch
#' @export
plot.ncompsearch = function(x, cidx=1,...){
#ylim = range(pretty(x$criterions))
ylab1 = ifelse(x$criterion == "CV", "CV error", "BIC")
main1 = ifelse(cidx==1, "Block Components", "Super Components")
if(cidx==1){ e = x$criterions[[cidx]] }else{e = x$criterions[[cidx]][[paste0("comp",x$optncomp[1])]]}
es1 = cbind(comp=x$comps[[cidx]], e=e)
plot(es1[,1], es1[,2], type="b", xlab="Number of Components", ylab=ylab1, main=main1, ...)
abline(v=x$optncomp[cidx], lty=2, col=2)
}


#' Regularized Parameters Search
#'
#' Regularized parameters search method for "msma". 
#'
#' This is a function for identifying the regularized parameters of sparseness lambdaX and lambdaY for \code{msma}. The initial range of candidates is computed based on fit, with regularized parameter values of 0. A binary search is conducted for dividing the parameter range into two regions. The representative value for the region is a median value, and the optimal region is selected using the minimum criteria obtained from the fit with that median value. The CV error or BIC can be used as criteria. The selected region is also divided into two region and the same process is iterated by maxrep times. Thus, the final median value in the selected region is set to be the optimal regularized parameter. The search is conducted with combinations of parameters for X and Y. The range of candidates for regularized parameters can be restricted, with a percentile of the limit (minimum or maximum) for the range.
#'
#' @name regparasearch
#' @aliases regparasearch
#' @rdname regparasearch
#' @docType methods
#' @export
#'
#' @param X a matrix or list of matrices indicating the explanatory variable(s). This parameter is required.
#' @param Y a matrix or list of matrices indicating objective variable(s). This is optional. If there is no input for Y, then PCA is implemented.
#' @param Z a vector, response variable(s) for implementing the supervised version of (multiblock) PCA or PLS. This is optional. The length of Z is the number of subjects. If there is no input for Z, then unsupervised PLS/PCA is implemented.
#' @param eta numeric scalar indicating the parameter indexing the penalty family. This version contains only choice 1.
#' @param type a character, indicating the penalty family. In this version, only one choice is available: "lasso."
#' @param inX a (list of) numeric vector to specify the variables of X which are always in the model. 
#' @param inY a (list of) numeric vector to specify the variables of X which are always in the model. 
#' @param inXsup a (list of) numeric vector to specify the blocks of X which are always in the model. 
#' @param inYsup a (list of) numeric vector to specify the blocks of Y which are always in the model. 
#' @param muX a numeric scalar for the weight of X for the supervised case. 0 <= muX <= 1.
#' @param muY a numeric scalar for the weight of Y for the supervised case. 0 <= muY <= 1. 
#' @param comp numeric scalar for the maximum number of componets to be considered.
#' @param nfold number of folds. Default is 5.
#' @param maxrep numeric scalar for the number of iteration.
#' @param minpct percent of minimum candidate parameters.
#' @param maxpct percent of maximum candidate parameters.
#' @param criterion a character, the evaluation criterion, "CV" for cross-validation, based on a matrix element-wise error, and "BIC" for Bayesian information criteria. The "BIC" is the default.
#' @param whichselect which blocks selected.
#' @param homo same parameters.
#' @param x an object of class "\code{regparasearch}", usually, a result of a call to \code{\link{regparasearch}}
#' @param intseed seed number for the random number in the parameter estimation algorithm.
#' @param sprmethod a character specifying the super-level decomposition method: PCA, NMF, sNMF, or CMP.
#' @param nneg a character specifying the non-negative transformation: posneg, absolute, or min.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return \item{optlambdaX}{Optimal parameters for X}
#' @return \item{optlambdaY}{Optimal parameters for Y}
#' @return \item{mincriterion}{Minimum of criterion values}
#' @return \item{criterions}{Resulting criterion value}
#' @return \item{pararange}{Range of candidates parameters}
#'
#' @examples
#' ##### data #####
#' tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = c(20, 15), seed=1)
#' X = tmpdata$X; Y = tmpdata$Y 
#' 
#' ##### Regularized parameters search #####
#' opt1 = regparasearch(X, Y, comp=1, criterion="BIC", maxrep=2, 
#' whichselect=c("X", "Y", "Xsup", "Ysup"))
#' opt1
#' fit4 = msma(X, Y, comp=1, lambdaX=opt1$optlambdaX, lambdaY=opt1$optlambdaY, 
#' lambdaXsup=opt1$optlambdaXsup, lambdaYsup=opt1$optlambdaYsup)
#' fit4
#' summary(fit4)
#'
regparasearch = function(X, Y=NULL, Z=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, comp=1, nfold=5, maxrep=3, minpct=0, maxpct=1, criterion=c("CV", "BIC")[1], whichselect=NULL, homo=NULL, intseed=1, sprmethod=c("PCA", "NMF", "sNMF", "CMP")[1], nneg = c("posneg", "absolute", "min")[1]){
##### Requirement #####
if(missing(X)) stop("X should be specified")
if(minpct>=1 | minpct<0) stop("minpct should be 0 <= minpct < 1")
if(maxpct>1 | maxpct<=0) stop("maxpct should be 0 < maxpct <= 1")

abnames = c("X", "Y", "Xsup", "Ysup")
if(length(comp)<2) comp[2] = 1

##### Candidates for Regularized Parameters #####
intfit = msma(X, Y, Z, comp=1, lambdaX=rep(NULL, length(X)), lambdaY=rep(NULL, length(Y)), lambdaXsup=NULL, lambdaYsup=NULL, eta, type, inX, inY, inXsup=NULL, inYsup=NULL, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg)

intweight = list(X=intfit$wbX, Y=intfit$wbY, Xsup=intfit$wsX, Ysup=intfit$wsY)
if(!is.null(homo)){ 
for(h1 in homo){
intweight[[h1]] = list(unlist(intweight[[h1]]))
}}

if(is.null(whichselect)) whichselect = names(which(unlist( lapply(intweight, function(x) (!is.null(unlist(x))) & length(unlist(x))>1)
 )))
intweight[!(names(intweight) %in% whichselect)] = NULL

lambdaXYcands = lapply(intweight, function(y){
do.call(rbind, lapply(y, function(x){if(is.null(x)){NULL}else{cand4lambda(x, 2)[2]}}))
})

nblocks = unlist(lapply(lambdaXYcands, nrow))
colidxend = cumsum(nblocks)
colidxstart = c(1, colidxend[-length(colidxend)]+1)
colidx = lapply(1:length(nblocks), function(x) colidxstart[x]:colidxend[x])
names(colidx) = names(nblocks)

totalnblocks = sum(nblocks)
idx = as.matrix(expand.grid(lapply(1:totalnblocks, function(x) c(2, 4))))

pararange = lapply(lambdaXYcands, function(y) apply(y, 1, function(y1) c(y1*minpct, y1*maxpct)))
range1s = do.call(cbind, pararange)

##### Iteration Start #####
criterions = list(); length(criterions) = maxrep
for(repidx in 1:maxrep){
quans = apply(range1s, 2, function(x){quantile(seq(x[1], x[2], length=10000))})
cands = apply(idx, 1, function(x){
unlist(lapply(colidx, function(y){if(length(y)==1){quans[x[y], y]}else{diag(quans[x[y], y])}}))
})

if(is.null(dim(cands))) dim(cands) = c(1, length(cands))

colnames(quans) = rownames(cands) = unlist(lapply(1:length(nblocks), function(x) paste0("lambda", names(nblocks)[x], 1:nblocks[x])))

##### CV for number of Regularized Parameters #####
cv3 = apply(cands, 2, function(x){
lambdas = lapply(abnames, function(y){z=colidx[[y]]; if(is.null(z)){NULL}else{x[z]}})
names(lambdas) = abnames
if(criterion=="CV"){
cvmsma(X, Y, Z, comp = comp, lambdaX=lambdas$X, lambdaY=lambdas$Y, lambdaXsup=lambdas$Xsup, lambdaYsup=lambdas$Ysup, eta, type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, nfold=nfold, intseed=intseed, sprmethod=sprmethod, nneg=nneg)$err[1]
}else if(criterion=="BIC"){
msma(X, Y, Z, comp = comp, lambdaX=lambdas$X, lambdaY=lambdas$Y, lambdaXsup=lambdas$Xsup, lambdaYsup=lambdas$Ysup, eta, type, inX=inX, inY=inY, inXsup=inXsup, inYsup=inYsup, muX = muX, muY = muY, intseed=intseed, sprmethod=sprmethod, nneg=nneg)$bic[comp[1]]
}
})

##### #####
optidx = which.min(cv3)

criterions[[repidx]] = rbind(cands, criterion=cv3)

ranidx = sapply(idx[optidx,], function(x) c(x-1, x+1)); 
range1s = sapply(1:totalnblocks, function(x) quans[ranidx[,x], x])

}

##### #####
optidx = which.min(unlist(lapply(criterions, function(x) min(x["criterion",]))))
cands = criterions[[optidx]]; 
optidx = which.min(cands["criterion",])
mincriterion = cands["criterion",optidx]
cands = cands[!(rownames(cands) %in% "criterion"), ]
if(is.null(dim(cands))) dim(cands) = c(1, length(cands))
optlambda = lapply(abnames, function(y){z=colidx[[y]]; if(is.null(z)){NULL}else{cands[z, optidx]}})
names(optlambda) = abnames

names(criterions) = paste("Step", 1:length(criterions))

##### #####
out=list(criterion=criterion, optlambdaX = optlambda$X, optlambdaY = optlambda$Y, optlambdaXsup = optlambda$Xsup, optlambdaYsup = optlambda$Ysup, mincriterion=mincriterion, criterions=criterions, pararange=pararange)
class(out) = "regparasearch"
out
}

#' @rdname regparasearch
#' @method print regparasearch
#' @export
print.regparasearch = function(x, ...)
{
abnames = c("X", "Y", "Xsup", "Ysup")
optlambda = x[paste0("optlambda", abnames)]
cat("Optimal parameters: \n")
cat("\n")
lapply(optlambda, function(y){
if(!is.null(y)){ print(round(y, 3))
cat("\n")
}})
}

#' Parameters Search
#'
#' Combined method for optimizing the number of components and regularized parameters for "msma". 
#'
#' A function for identifying the regularized sparseness parameters lambdaX and lambdaY and the number of components for \code{msma}. Four search methods are available. The "simultaneous" method identifies the number of components by searching the regularized parameters in each component. The "regpara1st" identifies the regularized parameters by fixing the number of components, then searching for the number of components with the selected regularized parameters. The "ncomp1st" method identifies the number of components with a regularized parameter of 0, then searches for the regularized parameters with the selected number of components. The "regparaonly" method searches for the regularized parameters with a fixed number of components.
#'
#' @name optparasearch
#' @aliases optparasearch
#' @rdname optparasearch
#' @docType methods
#' @export
#'
#' @param X a matrix or list of matrices indicating the explanatory variable(s). This parameter is required.
#' @param Y a matrix or list of matrices indicating objective variable(s). This is optional. If there is no input for Y, then PCA is implemented.
#' @param Z a vector, response variable(s) for implementing the supervised version of (multiblock) PCA or PLS. This is optional. The length of Z is the number of subjects. If there is no input for Z, then unsupervised PLS/PCA is implemented.
#' @param search.method a character indicationg search methods, see Details. Default is "ncomp1st" (this is version 3.0 or later).
#' @param comp numeric scalar for the number of components to be considered or the maximum canditate number of components.
#' @param eta numeric scalar indicating the parameter indexing the penalty family. This version contains only choice 1.
#' @param type a character, indicating the penalty family. In this version, only one choice is available: "lasso."
#' @param inX a vector or list of numeric vectors specifying the variables in X, always included in the model
#' @param inY a vector or list of numeric vectors specifying the variables in Y, always included in the model
#' @param muX a numeric scalar for the weight of X for the supervised case. 0 <= muX <= 1.
#' @param muY a numeric scalar for the weight of Y for the supervised case. 0 <= muY <= 1. 
#' @param nfold number of folds - default is 5. 
#' @param maxrep numeric scalar for the number of iteration.
#' @param minpct minimum candidate parameters defined as a percentile of automatically determined (possible) candidates.
#' @param maxpct maximum candidate parameters defined as a percentile of automatically determined (possible) candidates.
#' @param maxpct4ncomp maximum candidate parameters defined as a percentile of automatically determined (possible) candidates.
#' @param criterion a character, the evaluation criterion, "CV" for cross-validation, based on a matrix element-wise error, and "BIC" for Bayesian information criteria. The "BIC" is the default.
#' @param criterion4ncomp a character, the evaluation criterion for the selection of the number of components, "CV" for cross-validation, based on a matrix element-wise error, and "BIC" for Bayesian information criteria. 
#' @param whichselect which blocks selected.
#' @param homo same parameters.
#' @param x an object of class "\code{optparasearch}", usually, a result of a call to \code{optparasearch}
#' @param intseed seed number for the random number in the parameter estimation algorithm.
#' @param ... further arguments passed to or from other methods.
#' 
#' @return \item{optncomp}{Optimal number of components}
#' @return \item{optlambdaX}{Optimal parameters for X}
#' @return \item{optlambdaY}{Optimal parameters for Y}
#' @return \item{mincriterion}{Minimum criterion value}
#' @return \item{criteria}{All resulting criterion values in the process}
#' @return \item{pararange}{Range of candidates parameters}
#'
#' @examples
#' ##### data #####
#' tmpdata = simdata(n = 50, rho = 0.8, Yps = c(10, 12, 15), Xps = 20, seed=1)
#' X = tmpdata$X; Y = tmpdata$Y 
#' 
#' ##### Regularized parameters search #####
#' opt1 = optparasearch(X, Y, search.method = "regparaonly", comp=1, nfold=5, maxrep=2)
#' opt1
#' fit4 = msma(X, Y, comp=opt1$optncomp, lambdaX=opt1$optlambdaX, lambdaY=opt1$optlambdaY)
#' fit4
#' summary(fit4)
#'
#' ##### Restrict search range #####
#' opt2 = optparasearch(X, Y, comp=3, nfold=5, maxrep=2, minpct=0.5)
#' opt2
#'
optparasearch = function(X, Y=NULL, Z=NULL, search.method = c("ncomp1st", "regpara1st", "regparaonly", "simultaneous")[1], eta=1, type="lasso", inX=NULL, inY=NULL, muX = 0, muY = 0, comp=10, nfold=5, maxrep=3, minpct=0, maxpct=1, maxpct4ncomp=NULL, criterion=c("BIC","CV")[1], criterion4ncomp=NULL, whichselect=NULL, homo=NULL, intseed=1){

if(length(comp)<2) comp[2] = 1
comps1 = lapply(comp, function(c1) 1:c1)

if(is.null(criterion4ncomp)) criterion4ncomp=criterion

##### both (regpara first) #####
if(search.method == "regpara1st"){
regpara1 = regparasearch(X=X, Y=Y, Z=Z, comp=comp, muX = muX, muY = muY, nfold=nfold, minpct=minpct, maxpct=maxpct, maxrep=maxrep, criterion=criterion, whichselect=whichselect, homo=homo, intseed=intseed)
params = ncompsearch(X=X, Y=Y, Z=Z, comps = comps1, lambdaX=regpara1$optlambdaX, lambdaY=regpara1$optlambdaY, lambdaXsup=regpara1$optlambdaXsup, lambdaYsup=regpara1$optlambdaYsup, muX = muX, muY = muY, nfold=nfold, regpara=FALSE, criterion=criterion4ncomp, whichselect=NULL)
##### both (ncomp first) #####
}else if(search.method == "ncomp1st"){
#params = ncompsearch(X=X, Y=Y, Z=Z, comps = min(c(min(unlist(lapply(X,dim))), min(unlist(lapply(Y,dim))))), muX = muX, muY = muY, nfold=nfold1, regpara=FALSE, criterion=criterion)
if(is.null(maxpct4ncomp)){
params = ncompsearch(X=X, Y=Y, Z=Z, comps = comps1, muX = muX, muY = muY, nfold=nfold, regpara=FALSE, criterion=criterion4ncomp, whichselect=NULL)
}else{
params = ncompsearch(X=X, Y=Y, Z=Z, comps = comps1, muX = muX, muY = muY, nfold=nfold, maxpct=maxpct4ncomp, regpara=TRUE, criterion=criterion4ncomp, whichselect=NULL)
}
regpara1 = regparasearch(X=X, Y=Y, Z=Z, comp=params$optncomp, muX = muX, muY = muY, nfold=nfold, minpct=minpct, maxpct=maxpct, maxrep=maxrep, criterion=criterion, whichselect=whichselect, homo=homo, intseed=intseed)
params$optlambdaX = regpara1$optlambdaX
params$optlambdaY = regpara1$optlambdaY
params$optlambdaXsup = regpara1$optlambdaXsup
params$optlambdaYsup = regpara1$optlambdaYsup
##### both (simultaneous, pct=1/ncomp) #####
}else if(search.method == "simultaneous"){
params = ncompsearch(X=X, Y=Y, Z=Z, comps = comps1, muX = muX, muY = muY, nfold=nfold, regpara=TRUE, maxrep=maxrep, minpct=minpct, maxpct=maxpct, criterion=criterion4ncomp, whichselect=NULL)
##### regpara only (ncomp prespecified) #####
}else if(search.method == "regparaonly"){
params = list(optncomp = comp)
regpara1 = regparasearch(X=X, Y=Y, Z=Z, comp=params$optncomp, muX = muX, muY = muY, nfold=nfold, minpct=minpct, maxpct=maxpct, maxrep=maxrep, criterion=criterion, whichselect=whichselect, homo=homo, intseed=intseed)
params$optlambdaX = regpara1$optlambdaX
params$optlambdaY = regpara1$optlambdaY
params$optlambdaXsup = regpara1$optlambdaXsup
params$optlambdaYsup = regpara1$optlambdaYsup
params$cverrs = regpara1$cverrs
params$mincverr = regpara1$mincverr
}

params$search.method = search.method
params$criterion = criterion
params$criterion4ncomp = criterion4ncomp
class(params) = "optparasearch"

params
}

#' @rdname optparasearch
#' @method print optparasearch
#' @export
print.optparasearch = function(x, ...)
{
cat("Search method: ")
cat(paste(x$search.method))
cat(", ")
cat("Search criterion: ")
cat(paste(x$criterion))
cat("\n\n")
cat("Optimal number of components: ")
cat(paste(x$optncomp, collapse=", "))
cat("\n\n")
cat("Optimal parameters: \n")
cat("\n")
if(!is.null(x[["optlambdaX"]])){
print(round(x$optlambdaX, 3))
cat("\n")
}
if(!is.null(x[["optlambdaY"]])){
print(round(x$optlambdaY, 3))
cat("\n")
}
if(!is.null(x$optlambdaXsup)){
print(round(x$optlambdaXsup, 3))
cat("\n")
}
if(!is.null(x$optlambdaYsup)){
print(round(x$optlambdaYsup, 3))
cat("\n")
}
}



#' Internal functions
#'
#' These are internal functions for \code{msma}
#' 
#' These are not intended for use by users.
#'
#' @rdname msma-internal
#' @keywords internal
.msma_onecomp_legacy = function(X, Y, Z=NULL, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, dmode = "PLS", verbose=FALSE, intseed=1, ceps=0.0001){

Ynb = length(Y); Xnb = length(X)

if(any(is.null(lambdaX))){ lambdaX = rep(0, Xnb)}
if(any(is.null(lambdaY))){ lambdaY = rep(0, Ynb)}
if(any(is.null(lambdaXsup))){ lambdaXsup = 0}
if(any(is.null(lambdaYsup))){ lambdaYsup = 0}

if(is.null(Z)) Z = rep(0, nrow(X[[1]]))
muXY = 1 - muX - muY
if(muXY < 0) stop("muX or muY should be smaller values to be 0 <= 1 - muX - muY <= 1.")

#svd0 = svd(do.call(cbind, Y)); ssY = normvec(svd0$u[,1])
#ssY = normvec(rep(1, nrow(Y[[1]])))
set.seed(intseed); ssY = normvec(rnorm(nrow(X[[1]])))

if(length(X) == 1){wsX = 1}else{ set.seed(intseed); wsX = normvec(rnorm(length(X)))}
if(length(Y) == 1){wsY = 1}else{ set.seed(intseed); wsY = normvec(rnorm(length(Y)))}

##### Iteration #####
itr = 1
repeat{

### Xside (block) ###
XwsX = lapply(1:length(X), function(x) X[[x]] * wsX[x]) #XwsX = X
argus = lapply(XwsX, function(x){ tmp = c(t(x) %*% (muXY * ssY + muX * Z)); tmp[is.na(tmp)] = 0; tmp })
wbX = lapply(1:Xnb, function(x) normvec(sparse(normvec(argus[[x]]), lambdaX[x], eta, type, inX[[x]])))
sbX = lapply(1:Xnb, function(x) c(X[[x]] %*% wbX[[x]]) )
sbX2 = do.call(cbind, sbX)

### Xside (super) ###
argusups = t(sbX2) %*% (muXY * ssY + muX * Z)
wsX = normvec(sparse(normvec(argusups), lambdaXsup, eta, type, inXsup))
ssX = sbX2 %*% wsX

### Yside (block) ###
YwsY = lapply(1:length(Y), function(x) Y[[x]] * wsY[x]) #YwsY = Y
argvs = lapply(YwsY, function(x){ tmp = c(t(x) %*% (muXY * ssX + muY * Z)); tmp[is.na(tmp)] = 0; tmp})
wbY = lapply(1:Ynb, function(y) normvec(sparse(normvec(argvs[[y]]), lambdaY[y], eta, type, inY[[y]])))
sbY = lapply(1:Ynb, function(x) c(Y[[x]] %*% wbY[[x]]) )
sbY2 = do.call(cbind, sbY)

### Yside (super) ###
#wsY = normvec(t(sbY2) %*% (muXY * ssX + muY * Z))
argvsups = t(sbY2) %*% (muXY * ssX + muY * Z)
wsY = normvec(sparse(normvec(argvsups), lambdaYsup, eta, type, inYsup))
ssY = sbY2 %*% wsY

### Convergence ###
if(itr > 2){
#dif = drop(crossprod(ssX - oldssX))
difX = drop(crossprod(ssX - oldssX))
difY = drop(crossprod(ssY - oldssY))
dif = max(difX, difY)
if(verbose) print(c(itr, dif))
if(dif < ceps | itr > 20){break}
}
oldssX = ssX
oldssY = ssY
itr = itr + 1
}

### Output ###
list(wbX=wbX, sbX=sbX, wbY=wbY, sbY=sbY, ssX=ssX, wsX=wsX, ssY=ssY, wsY=wsY)
}

.msma_onecomp_extended = function(X, Y, Z=NULL, lambdaX=NULL, lambdaY=NULL, lambdaXsup=NULL, lambdaYsup=NULL, eta=1, type="lasso", inX=NULL, inY=NULL, inXsup=NULL, inYsup=NULL, muX = 0, muY = 0, dmode = "PLS", verbose=FALSE, intseed=1, ceps=0.0001, con4spv=NULL){

Ynb = length(Y); Xnb = length(X)

if(any(is.null(lambdaX))){ lambdaX = rep(0, Xnb)}
if(any(is.null(lambdaY))){ lambdaY = rep(0, Ynb)}
if(any(is.null(lambdaXsup))){ lambdaXsup = 0}
if(any(is.null(lambdaYsup))){ lambdaYsup = 0}

if(is.null(Z)) Z = matrix(0, nrow(X[[1]]), 1)
if(is.null(con4spv)){ con4spv = rep(1, ncol(Z))}
con4spv=con4spv/sum(abs(con4spv))

muXY = 1 - muX - muY
if(muXY < 0) stop("muX or muY should be smaller values to be 0 <= 1 - muX - muY <= 1.")

#svd0 = svd(do.call(cbind, Y)); ssY = normvec(svd0$u[,1])
#ssY = normvec(rep(1, nrow(Y[[1]])))
set.seed(intseed); ssY = normvec(rnorm(nrow(X[[1]])))

if(length(X) == 1){wsX = 1}else{ set.seed(intseed); wsX = normvec(rnorm(length(X)))}
if(length(Y) == 1){wsY = 1}else{ set.seed(intseed); wsY = normvec(rnorm(length(Y)))}

##### Iteration #####
itr = 1
repeat{

### Xside (block) ###
XwsX = lapply(1:length(X), function(x) X[[x]] * wsX[x]) #XwsX = X
argus = lapply(XwsX, function(x){ tmp = c(t(x) %*% (muXY * ssY + muX * Z %*% con4spv)); tmp[is.na(tmp)] = 0; tmp })
wbX = lapply(1:Xnb, function(x) normvec(sparse(normvec(argus[[x]]), lambdaX[x], eta, type, inX[[x]])))
sbX = lapply(1:Xnb, function(x) c(X[[x]] %*% wbX[[x]]) )
sbX2 = do.call(cbind, sbX)

### Xside (super) ###
argusups = t(sbX2) %*% (muXY * ssY + muX * Z %*% con4spv)
wsX = normvec(sparse(normvec(argusups), lambdaXsup, eta, type, inXsup))
ssX = sbX2 %*% wsX

### Yside (block) ###
YwsY = lapply(1:length(Y), function(x) Y[[x]] * wsY[x]) #YwsY = Y
argvs = lapply(YwsY, function(x){ tmp = c(t(x) %*% (muXY * ssX + muY * Z %*% con4spv)); tmp[is.na(tmp)] = 0; tmp})
wbY = lapply(1:Ynb, function(y) normvec(sparse(normvec(argvs[[y]]), lambdaY[y], eta, type, inY[[y]])))
sbY = lapply(1:Ynb, function(x) c(Y[[x]] %*% wbY[[x]]) )
sbY2 = do.call(cbind, sbY)

### Yside (super) ###
#wsY = normvec(t(sbY2) %*% (muXY * ssX + muY * Z))
argvsups = t(sbY2) %*% (muXY * ssX + muY * Z %*% con4spv)
wsY = normvec(sparse(normvec(argvsups), lambdaYsup, eta, type, inYsup))
ssY = sbY2 %*% wsY

### Convergence ###
if(itr > 2){
#dif = drop(crossprod(ssX - oldssX))
difX = drop(crossprod(ssX - oldssX))
difY = drop(crossprod(ssY - oldssY))
dif = max(difX, difY)
if(verbose) print(c(itr, dif))
if(dif < ceps | itr > 20){break}
}
oldssX = ssX
oldssY = ssY
itr = itr + 1
}

### Output ###
list(wbX=wbX, sbX=sbX, wbY=wbY, sbY=sbY, ssX=ssX, wsX=wsX, ssY=ssY, wsY=wsY)
}

#' @rdname msma-internal
normvec = function(a){if(all(a == 0)){a}else{ c(a) / sqrt(drop(crossprod(c(a))))}}

#' @rdname msma-internal
sparse = function(x, lam, eta=1, type="lasso", inidx=NULL){
if(all(x == 0)){ lam = 0 }else if(lam >= max(abs(x))){ 
tmpdiff = diff(sort(unique(abs(x)))); tmpdiff = tmpdiff[tmpdiff > 1e-8]
lam = max(abs(x)) - tmpdiff[length(tmpdiff)]/2
}
lam = rep(lam, length(x))
if(!is.null(inidx)) lam[inidx] = 0
sout = sapply(1:length(x), function(x1) {
if(type=="lasso"){
sign(x[x1]) * max(c(abs(x[x1]) - lam[x1], 0))
}else 
if(type=="hard"){
x[x1] * (abs(x[x1]) > lam[x1])
}else
if(type=="scad"){
if(eta==2) stop("eta=2 not available for scad")
ifelse(abs(x[x1]) > 2*lam[x1], ifelse(abs(x[x1]) <= eta*lam[x1], ((eta-1)*x[x1] - sign(x[x1])*eta*lam[x1])/(eta-2), x[x1]), 
sign(x[x1]) * max(c(abs(x[x1]) - lam[x1], 0)))
}else
if(type=="mcp"){
if(eta==1) stop("eta=1 not available for mcp")
ifelse(abs(x[x1]) <= eta*lam[x1], (sign(x[x1]) * max(c(abs(x[x1]) - lam[x1], 0)))/(1-1/eta), x[x1])
}
})
sout
}

#' @rdname msma-internal
cand4lambda = function(x, len){
max1 = max(abs(x)) - diff(sort(abs(x)))[length(x)-1]/2
seq(0, max1, length=len)
}

#' @rdname msma-internal
searchseq = function(orgseq, opt){
idx = which(orgseq == opt)
if(idx == 1){x0 = orgseq[idx]; x1 = orgseq[idx+1]; p1 = c(0.25, 0.5)}else
if(idx == length(orgseq)){x0 = orgseq[idx-1]; x1 = orgseq[idx]; p1 = c(0.5, 0.75)}else
{x0 = orgseq[idx-1]; x1 = orgseq[idx+1]; p1 = c(0.25, 0.75)}
quantile(seq(x0, x1, length=10000), probs = p1)
}

#' @rdname msma-internal
project = function(x, y){
if(is.null(ncol(y)))
{c(x %*% cbind(y) / drop(crossprod(y)))}else
{
crossy = t(y) %*% y
i = 0; judge = TRUE
while(judge){
invcrossy = try(solve(crossy + 1e-18*10^i * diag(ncol(crossy))), silent = TRUE)
judge = (inherits(invcrossy, "try-error"))
i = i + 1
}
invcrossy %*% t(y) %*% x
}
}

#' @rdname msma-internal
matserr = function(X1, X2) mean(sapply(1:length(X1), function(x) mean((X1[[x]]-X2[[x]])^2)))

#' Simulate Data sets
#'
#' This is a function for generating multiblock data based on the multivariable normal distribusion
#' 
#' The output is a list of matrics.
#'
#' @name simdata
#' @aliases simdata
#' @rdname simdata
#' @docType methods
#' @export
#'
#' @param n a numeric scalar, sample size.
#' @param rho a numeric scalar, correlation coefficient.
#' @param Yps a numeric vector, numbers of columns for Y. The length of vector corresponds to the number of blocks.
#' @param Xps a numeric vector, numbers of columns for X. The length of vector corresponds to the number of blocks.
#' @param seed a seed number for generating random numbers.
#'
#' @return \item{X}{Simulated X which has a list form}
#' @return \item{Y}{Simulated Y which has a list form}
simdata = function(n = 100, rho = 0.8, Yps = c(100, 120, 150), Xps = 500, seed=1){

if(length(n) > 1) stop("n should be scalar")
if(length(rho) > 1) stop("rho should be scalar")

#require("mvtnorm")

set.seed(seed)
Y = lapply(Yps, function(p){
sigma = matrix(rho, p, p)
diag(sigma) = 1
rmnorm(n, rep(0, p), sigma)
})

X = lapply(Xps, function(p){
sigma = matrix(rho, p, p)
diag(sigma) = 1
rmnorm(n, rep(0, p), sigma)
})

list(X=X, Y=Y)
}

#' Structured Simulate Data sets
#'
#' This is a function for generating multiblock data based on the multivariable normal distribusion
#' 
#' The output is a list of matrics.
#'
#' @name strsimdata
#' @aliases strsimdata
#' @rdname strsimdata
#' @docType methods
#' @export
#'
#' @param n a numeric scalar, sample size.
#' @param WX a matrix or a list, weights.
#' @param ncomp a numeric scalar, number of components.
#' @param Yps a numeric vector, numbers of columns for Y. The length of vector corresponds to the number of blocks.
#' @param Xps a numeric vector, numbers of columns for X. The length of vector corresponds to the number of blocks.
#' @param rho a numeric, correlation
#' @param Ztype a character, outcome type ("none", "binary", "prob").
#' @param cz a numeric vector, scale for outcome
#' @param cwx a numeric vector, scale for weights of X
#' @param cwy a numeric vector, scale for weights of Y
#' @param ncomp number of components
#' @param seed a seed number for generating random numbers.
#' @param minpct minimum percent of nonzero
#' @param maxpct maximum percent of nonzero
#'
#' @return \item{X}{Simulated X which has a list form}
#' @return \item{Y}{Simulated Y which has a list form}
#' @return \item{Z}{Simulated Z which has a vector form}
#' @return \item{ncomp}{}
#' @return \item{Xps}{}
#' @return \item{nZeroX}{}
#' @return \item{idxZeroX}{}
#' @return \item{Yps}{}
#' @return \item{nZeroY}{}
#' @return \item{idxZeroY}{}
#' @return \item{WX}{}
#' @return \item{WY}{}
#' @return \item{ZcoefX}{}
#' @return \item{ZcoefY}{}
strsimdata = function(n = 100, WX=NULL, ncomp=5, Xps = 10, Yps = FALSE, rho=0.8, Ztype=c("none", "binary", "prob")[1], cz=c(1,1), cwx=c(0.1, 0.1), cwy=c(0.1, 0.1), seed=1, minpct = 0.25, maxpct = 0.75){
#tmp = list(n = 20, ncomp=c(2, 3), Xps=c(5,5,5,5), Yps=c(3,4), Ztype=c("none", "binary", "prob")[2], cz=c(1,1), seed=1, rho=0.8, minpct = 0.25, maxpct = 0.75, WX=NULL);cwx=c(0.1, 0.1); cwy=c(0.1, 0.1); attach(tmp)

ncomp[2] = ifelse(length(ncomp)<2, 1, ncomp[2])

nblockX = length(Xps); nblockY = length(Yps); 
set.seed(seed)

########## Generate X ##########
if(is.null(WX)){
Xps2 = list(Xps, rep(nblockX, ncomp[1])) #list(block, super)
names(Xps2) = c("block", "super")

#####
idxZeroX = lapply(1:length(Xps2), function(i){
xps = Xps2[[i]]
if(length(xps)==1 & xps[1]==1){ out = list(list(NULL)) }else{
out=lapply(xps, function(p){ 
tmp = lapply(1:ncomp[i], function(x) sort(sample(p, p*runif(1, minpct, maxpct))))
names(tmp) = paste0(ifelse(i==1, "rtcomp", "ntcomp"), 1:ncomp[i])
allzero = as.numeric(names(which(sort(table(unlist(tmp))) == ncomp[i])))
k = which.max(unlist(lapply(tmp, length)))
tmp[[k]] = tmp[[k]][!(tmp[[k]] %in% allzero)]
tmp
})
}
names(out) = paste0(ifelse(i==1, "block", "comp"), 1:length(xps))
out
})
names(idxZeroX) = c("block", "super")

#####
WX = list(
block=lapply(1:nblockX, function(x){ 
out = do.call(cbind, lapply(1:ncomp[1], function(y){ v=rnorm(Xps[x]); v[idxZeroX[[1]][[x]][[y]]]=0; normvec(v)}))
colnames(out) = paste0("rtcomp", 1:ncomp[1])
rownames(out) = paste0("v", 1:Xps[x])
out
}), 
super=lapply(1:ncomp[1], function(x){
out = do.call(cbind, lapply(1:ncomp[2], function(y){ v=rnorm(nblockX); v[idxZeroX[[2]][[x]][[y]]]=0; normvec(v)}))
colnames(out) = paste0("ntcomp", 1:ncomp[2])
rownames(out) = paste0("block", 1:nblockX)
out
})
)
names(WX$super) = paste0("rtcomp", 1:ncomp[1])
names(WX$block) = paste0("block", 1:nblockX)

#####
}else{
if(inherits(WX, "matrix")){ WX = list(WX)}
ncomp=unlist(lapply(WX, function(wx) ncol(wx[[1]])))
Xps = unlist(lapply(WX[[1]], function(wx2)nrow(wx2)))
idxZeroX = lapply(1:length(WX), function(wi)
{if(wi==1){NULL}else{lapply(WX[[wi]], function(wx){ lapply(1:ncol(wx), function(c1) which(wx[,c1] == 0 )) }) 
}})
}

#####
nZeroX = lapply(idxZeroX, function(x) do.call(rbind, lapply(x, function(x1) unlist(lapply(x1, function(x2){ iz=length(x2); ifelse(is.null(iz),0,iz)})) )))

#####
SUX = lapply(1:ncomp[1], function(y){
out = sapply(1:ncomp[2], function(x) rnorm(n, 0, 1)) 
out = out[,rank(-diag(var(out))),drop=FALSE]
colnames(out) = paste0("ntcomp", 1:ncomp[2])
out
})
names(SUX) = paste0("rtcomp", 1:ncomp[1])

UX0 = lapply(1:ncomp[1], function(x) SUX[[x]] %*% ginv(WX$super[[x]]*cwx[2]))
names(UX0) = paste0("rtcomp", 1:ncomp[1])
UX = lapply(1:nblockX, function(x) do.call(cbind, lapply(UX0, function(y) y[,x])))

X = lapply(1:nblockX, function(x) UX[[x]] %*% ginv(WX$block[[x]]*cwx[1]))
names(X) = names(UX) = paste0("block", 1:nblockX)

########## Generate Y ##########
if(!Yps[1]){idxZeroY = NULL; nZeroY = NULL; Y=NULL; WY=NULL; SUY=NULL}else
{
Yps2 = list(Yps, rep(nblockY, ncomp[1])) #list(block, super)

#####
idxZeroY = lapply(1:length(Yps2), function(i){
xps = Yps2[[i]]
if(length(xps)==1 & xps[1]==1){ out = list(list(NULL)) }else{
out=lapply(xps, function(p){ 
tmp = lapply(1:ncomp[i], function(x) sort(sample(p, p*runif(1, minpct, maxpct))))
names(tmp) = paste0("comp", 1:ncomp[i])
allzero = as.numeric(names(which(sort(table(unlist(tmp))) == ncomp[i])))
k = which.max(unlist(lapply(tmp, length)))
tmp[[k]] = tmp[[k]][!(tmp[[k]] %in% allzero)]
tmp
})
}
names(out) = paste0(ifelse(i==1, "block", "comp"), 1:length(xps))
out
})
names(idxZeroY) = c("block", "super")

#####
WY = list(
block=lapply(1:nblockY, function(x){ 
out = do.call(cbind, lapply(1:ncomp[1], function(y){ v=normvec(rnorm(Yps[x])); v[idxZeroY[[1]][[x]][[y]]]=0; v}))
colnames(out) = paste0("comp", 1:ncomp[1])
rownames(out) = paste0("v", 1:Yps[x])
out
}), 
super=lapply(1:ncomp[1], function(x){
out = do.call(cbind, lapply(1:ncomp[2], function(y){ v=normvec(rnorm(nblockY)); v[idxZeroY[[2]][[x]][[y]]]=0; v}))
colnames(out) = paste0("comp", 1:ncomp[2])
rownames(out) = paste0("block", 1:nblockY)
out
})
)
names(WY$super) = paste0("rtcomp", 1:ncomp[1])
names(WY$block) = paste0("block", 1:nblockY)

#####
nZeroY = lapply(idxZeroY, function(x) lapply(x, function(x1) unlist(lapply(x1, function(x2){ iz=length(x2); ifelse(is.null(iz),0,iz)})) ))

#####
SUY = lapply(SUX, function(y) apply(y, 2, function(x) rnorm(n, rho*x, sqrt(1-rho^2))))
UY0 = lapply(1:ncomp[1], function(x) SUY[[x]] %*% ginv(WY$super[[x]]))
names(UY0) = paste0("rtcomp", 1:ncomp[1])
UY = lapply(1:nblockY, function(x) do.call(cbind, lapply(UY0, function(y) y[,x])))
Y = lapply(1:nblockY, function(x) UY[[x]] %*% ginv(WY$block[[x]]))
names(Y) = names(UY) = paste0("block", 1:nblockY)
}

##### Generate Z #####
if(!(Ztype %in% "none")){
UX2 = do.call(cbind, UX)
ZcoefXmean0 = unlist(WX[[2]])#WX[[2]]#unlist(lapply(WX[[2]], function(x) x[,ncomp]))
ZcoefXmean = ifelse(ZcoefXmean0==0, 0, abs(1/ZcoefXmean0))
ZcoefX = normvec(rnorm(nblockX*ncomp[1], ZcoefXmean, 0.01)) * cz[1]
UX2b = UX2 %*% ZcoefX
UY2b = 0; ZcoefY = NULL
if(Yps[1]){
UY2 = do.call(cbind, UY)
ZcoefY = normvec(rnorm(nblockY*ncomp, ifelse(c(WY[[2]])==0, 0, abs(1/c(WY[[2]]))), 0.01)) * cz[2]
UY2b = UY2 %*% ZcoefY
}
UXYb = UX2b + UY2b
UXYb = c(scale(UXYb, scale=FALSE))
Zprob = exp(UXYb)/(1+exp(UXYb))
Z = sapply(Zprob, function(x) rbinom(1, 1, x))
if(Ztype %in% "prob") Z = Zprob
}else{
Z = ZcoefX = ZcoefY = NULL
}

list(X=X, Y=Y, Z=Z, ncomp=ncomp, Xps=Xps, nZeroX=nZeroX, idxZeroX=idxZeroX, nZeroY=nZeroY, idxZeroY=idxZeroY, WX=WX, WY=WY, ZcoefX=ZcoefX, ZcoefY=ZcoefY, SUX=SUX, SUY=SUY)
}



#' @rdname msma-internal
ginv=function(X, tol = sqrt(.Machine$double.eps))
{
## Generalized Inverse of a Matrix
  dnx <- dimnames(X)
  if(is.null(dnx)) dnx <- vector("list", 2)
  s <- svd(X)
  nz <- s$d > tol * s$d[1]
  structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
    dimnames = dnx[2:1])
}

#' Hierarchical cluster analysis 
#'
#' This is a function for performing a hierarchical cluster analysis using scores
#' 
#' This function performs a hierarchical cluster analysis using scores.
#'
#' @name hcmsma
#' @aliases hcmsma
#' @rdname hcmsma
#' @docType methods
#' @export
#'
#' @param object an object of class "\code{msma}", usually, a result of a call to \code{\link{msma}}
#' @param nclust a numeric scalar, number of clusters.
#' @param graph a numeric vector, numbers of columns for Y. The length of vector corresponds to the number of blocks.
#' @param hmethod, the agglomeration method to be used in the function "\code{hclust}".
#' @param axes a numeric (or vector), specifying the component(s) to analyze. 
#' @param block a character, indicating which the "block" or "super" is used. 
#' @param XY a character, indicating "X" or "Y". 
#'
#' @return \item{hcout}{An object of class hclust}
#' @return \item{clusters}{a vector with group memberships}
#' @return \item{object}{an object of class "\code{msma}", usually, a result of a call to \code{\link{msma}}}
#'
hcmsma = function(object, nclust=4, graph=FALSE, hmethod="ward.D2", axes = c(1, 2), block="block", XY="X"){
v1 = "s"
if(block == "block"){
tmpvar = do.call(rbind, object[[paste0(v1, "b", XY)]])[, axes,drop=FALSE]
}else{
tmpvar = object[[paste0(v1, "s", XY)]][, axes,drop=FALSE]
}
hcout = hclust(dist(tmpvar), method = hmethod)

if(graph){
plot(hcout)
rect.hclust(hcout, k=nclust, border="red")
}

clusters=cutree(hcout, nclust)

list(hcout=hcout, clusters=clusters, object=object)
}

#' @rdname msma-internal
rmnorm <- function(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)), method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE, checkSymmetry = TRUE)
{
if (checkSymmetry && !isSymmetric(sigma, tol = sqrt(.Machine$double.eps), check.attributes = FALSE)) {
stop("sigma must be a symmetric matrix")
}
if (length(mean) != nrow(sigma))
stop("mean and sigma have non-conforming size")

method <- match.arg(method)

R <- if(method == "eigen") {
ev <- eigen(sigma, symmetric = TRUE)
if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))){
    warning("sigma is numerically not positive semidefinite")
}
## ev$vectors %*% diag(sqrt(ev$values), length(ev$values)) %*% t(ev$vectors)
## faster for large  nrow(sigma):
t(ev$vectors %*% (t(ev$vectors) * sqrt(pmax(ev$values, 0))))
}
else if(method == "svd"){
s. <- svd(sigma)
if (!all(s.$d >= -sqrt(.Machine$double.eps) * abs(s.$d[1]))){
    warning("sigma is numerically not positive semidefinite")
}
t(s.$v %*% (t(s.$u) * sqrt(pmax(s.$d, 0))))
}
else if(method == "chol"){
R <- chol(sigma, pivot = TRUE)
R[, order(attr(R, "pivot"))]
}

retval <- matrix(rnorm(n * ncol(sigma)), nrow = n, byrow = !pre0.9_9994) %*%  R
retval <- sweep(retval, 2, mean, "+")
colnames(retval) <- names(mean)
retval
}

#' @rdname msma-internal
sprNMF <- function(X, K, lambda=NULL, ite = 500, seed=1){
  set.seed(seed)

  N = nrow(X); M = ncol(X)
  W<-matrix(runif(N*K), nrow = N)
  H<-matrix(runif(K*M), nrow = K)
  
  loss<-NULL
#-------Update------------#  
  for(i in 1:ite){
    
    H_num<-t(W)%*%X
    H_deno<-t(W)%*%W%*%H + 1e-10
    H<-H*(H_num/H_deno)
    
    W_num<-X%*%t(H)
    W_deno<-W%*%H%*%t(H) + 1e-10
    W<-W*(W_num/W_deno)
if(!is.null(lambda)) W = t(apply(W, 1, function(w) normvec(sparse(normvec(w), lambda))))

    
    loss[i]<-sum((X-W%*%H)^2)
    
  }
  
  return(list(Basis = W, Encoding = H, loss = loss))
}

####################################################################################################
# S4PCA reference-compatible candidate engine for msma 4.0 development.
# This internal function intentionally preserves the manuscript-generating
# algorithm before later validation, diagnostics, and public API integration.
####################################################################################################

.s4pca_norm_reference <- function(a) {
  if (all(a == 0)) a else c(a) / sqrt(drop(crossprod(c(a))))
}

.s4pca_sparse_reference <- function(x, lam, eta = 1, type = "lasso", inidx = NULL) {
  if (all(x == 0)) {
    lam <- 0
  } else if (isTRUE(length(lam) == 1) && lam >= max(abs(x))) {
    tmpdiff <- diff(sort(unique(abs(x))))
    tmpdiff <- tmpdiff[tmpdiff > 1e-8]
    lam <- max(abs(x)) - ifelse(length(tmpdiff) > 0, utils::tail(tmpdiff, 1) / 2, 1e-6)
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
      ifelse(abs(x[j]) > 2 * lam[j],
        ifelse(abs(x[j]) <= eta * lam[j],
          ((eta - 1) * x[j] - sign(x[j]) * eta * lam[j]) / (eta - 2), x[j]),
        sign(x[j]) * max(c(abs(x[j]) - lam[j], 0)))
    } else if (type == "mcp") {
      if (eta == 1) stop("eta=1 not available for mcp")
      ifelse(abs(x[j]) <= eta * lam[j],
        sign(x[j]) * max(c(abs(x[j]) - lam[j], 0)) / (1 - 1 / eta), x[j])
    } else {
      stop("unknown sparse penalty")
    }
  })
}

.s4pca_project_reference <- function(x, y) {
  if (is.null(ncol(y))) {
    c(x %*% cbind(y) / drop(crossprod(y)))
  } else {
    crossy <- t(y) %*% y
    i <- 0
    judge <- TRUE
    while (judge) {
      invcrossy <- try(solve(crossy + 1e-18 * 10^i * diag(ncol(crossy))), silent = TRUE)
      judge <- inherits(invcrossy, "try-error")
      i <- i + 1
    }
    invcrossy %*% t(y) %*% x
  }
}

.s4pca_fit_reference <- function(X, Z = NULL, comp = 6, lambda = 0.02,
                                 gamma = 0, history_method = "sumabs", mu = 1.0,
                                 hard_exclusion = FALSE, niter = 30, seed = NULL,
                                 verbose = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  history_method <- match.arg(history_method, c("sumabs", "any", "count"))
  Xs <- X
  Zs <- Z
  n <- nrow(Xs)
  p <- ncol(Xs)
  s <- .s4pca_norm_reference(rnorm(n))
  Xd <- Xs
  S <- W <- NULL
  nzi <- NULL
  c1 <- 1
  while (c1 <= comp) {
    c1 <- c1 + 1
    for (i in seq_len(niter)) {
      w <- c(t(Xd) %*% c((1 - mu) * s + mu * if (is.null(Zs)) 0 else Zs))
      if (hard_exclusion && length(nzi)) w[nzi] <- 0
      if (gamma > 0 && !is.null(W)) {
        if (history_method == "sumabs") h <- rowSums(abs(W))
        else if (history_method == "any") h <- as.numeric(rowSums(abs(W) > 0) > 0)
        else h <- rowSums(abs(W) > 0)
        lam_vec <- lambda + gamma * h
      } else {
        lam_vec <- lambda
      }
      w <- .s4pca_norm_reference(w)
      w <- .s4pca_norm_reference(.s4pca_sparse_reference(w, lam = lam_vec, type = "lasso"))
      s <- c(Xd %*% w)
    }
    S <- cbind(S, s)
    W <- cbind(W, w)
    nz_now <- which(w != 0)
    if (length(nz_now)) nzi <- unique(c(nzi, nz_now))
    if (length(nzi) == p) break
    pred <- S %*% .s4pca_project_reference(Xs, S)
    Xd <- Xs - pred
    if (verbose) {
      overlap_rate <- mean(rowSums(abs(W) > 1e-8) >= 2)
      cat(sprintf("comp=%d  overlap=%.3f  used=%d\n", ncol(W), overlap_rate, length(nzi)))
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
  selected_count <- rowSums(abs(W) > 1e-8)
  overlap_all <- mean(selected_count >= 2)
  overlap_selected <- if (any(selected_count >= 1)) {
    sum(selected_count >= 2) / sum(selected_count >= 1)
  } else 0
  list(W = W, S = S, rss = rss, AIC = aic, BIC = bic, EBIC = ebic,
       overlap = overlap_all, overlap_all = overlap_all,
       overlap_selected = overlap_selected, used_idx = nzi,
       settings = list(lambda = lambda, gamma = gamma,
         history_method = history_method, mu = mu,
         hard_exclusion = hard_exclusion, niter = niter, seed = seed))
}

####################################################################################################
# S4PCA Milestone 3: validated internal interface and diagnostics.
####################################################################################################

.s4pca_validate_inputs <- function(X, Z = NULL, comp = 6, lambda = 0.02,
                                   gamma = 0, history_method = "sumabs",
                                   mu = 1, hard_exclusion = FALSE,
                                   niter = 30, seed = NULL) {
  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.numeric(X) || length(dim(X)) != 2L) {
    stop("X must be a numeric matrix", call. = FALSE)
  }
  if (nrow(X) < 2L || ncol(X) < 1L) {
    stop("X must contain at least two rows and one column", call. = FALSE)
  }
  if (any(!is.finite(X))) {
    stop("X must not contain NA, NaN, or Inf", call. = FALSE)
  }
  if (!is.null(Z)) {
    if (is.matrix(Z) && ncol(Z) == 1L) Z <- drop(Z)
    if (!is.numeric(Z) || !is.null(dim(Z))) {
      stop("Z must be a numeric vector or a one-column numeric matrix", call. = FALSE)
    }
    if (length(Z) != nrow(X)) {
      stop("length(Z) must equal nrow(X)", call. = FALSE)
    }
    if (any(!is.finite(Z))) {
      stop("Z must not contain NA, NaN, or Inf", call. = FALSE)
    }
  }
  scalar_integer <- function(x, name, lower) {
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < lower || x != as.integer(x)) {
      stop(name, " must be a single integer >= ", lower, call. = FALSE)
    }
    as.integer(x)
  }
  scalar_nonnegative <- function(x, name) {
    if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x < 0) {
      stop(name, " must be a single non-negative finite number", call. = FALSE)
    }
    as.numeric(x)
  }
  comp <- scalar_integer(comp, "comp", 1L)
  niter <- scalar_integer(niter, "niter", 1L)
  lambda <- scalar_nonnegative(lambda, "lambda")
  gamma <- scalar_nonnegative(gamma, "gamma")
  if (length(mu) != 1L || !is.numeric(mu) || !is.finite(mu) || mu < 0 || mu > 1) {
    stop("mu must be a single finite number in [0, 1]", call. = FALSE)
  }
  history_method <- match.arg(history_method, c("sumabs", "any", "count"))
  if (length(hard_exclusion) != 1L || !is.logical(hard_exclusion) || is.na(hard_exclusion)) {
    stop("hard_exclusion must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.null(seed)) seed <- scalar_integer(seed, "seed", 0L)
  list(X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
       history_method = history_method, mu = as.numeric(mu),
       hard_exclusion = hard_exclusion, niter = niter, seed = seed)
}

.s4pca_fit <- function(X, Z = NULL, comp = 6, lambda = 0.02, gamma = 0,
                       history_method = "sumabs", mu = 1,
                       hard_exclusion = FALSE, niter = 30, seed = NULL,
                       verbose = FALSE) {
  input <- .s4pca_validate_inputs(
    X = X, Z = Z, comp = comp, lambda = lambda, gamma = gamma,
    history_method = history_method, mu = mu,
    hard_exclusion = hard_exclusion, niter = niter, seed = seed
  )
  fit <- .s4pca_fit_reference(
    X = input$X, Z = input$Z, comp = input$comp,
    lambda = input$lambda, gamma = input$gamma,
    history_method = input$history_method, mu = input$mu,
    hard_exclusion = input$hard_exclusion, niter = input$niter,
    seed = input$seed, verbose = verbose
  )
  fit$diagnostics <- list(
    engine = "reference-compatible",
    iterations_per_component = rep(input$niter, ncol(fit$W)),
    extracted_components = ncol(fit$W),
    requested_components = input$comp,
    terminated_early = ncol(fit$W) < input$comp,
    selected_per_component = colSums(abs(fit$W) > 1e-8),
    selected_variables = sum(rowSums(abs(fit$W) > 1e-8) >= 1L),
    repeated_variables = sum(rowSums(abs(fit$W) > 1e-8) >= 2L),
    convergence_rule = "fixed_iterations",
    reference_compatible = TRUE
  )
  fit$settings$interface <- "validated"
  fit
}

####################################################################################################
# S4PCA Milestone 5: reconstruction and repeated split conformal model selection.
####################################################################################################

#' Reconstruct data from an S4PCA fit
#'
#' Reconstructs the training data or a new matrix using the score subspace and
#' reconstruction coefficients learned by a public S4PCA call to \code{msma}.
#'
#' @param object an object of class \code{s4pca}.
#' @param newdata optional numeric matrix with the same variables as the fitted X.
#' @param scaled logical; if TRUE, return reconstruction on the standardized scale.
#' @return a reconstructed numeric matrix.
#' @export
s4pca_reconstruct <- function(object, newdata = NULL, scaled = FALSE) {
  if (!inherits(object, "s4pca")) stop("object must inherit from 's4pca'", call. = FALSE)
  if (length(scaled) != 1L || !is.logical(scaled) || is.na(scaled)) {
    stop("scaled must be TRUE or FALSE", call. = FALSE)
  }
  B <- object$reconstruction_loadings
  if (is.null(B)) stop("object does not contain reconstruction coefficients", call. = FALSE)
  if (is.null(newdata)) {
    Xhat <- object$S %*% B
  } else {
    X <- as.matrix(newdata)
    if (!is.numeric(X) || ncol(X) != nrow(object$W) || any(!is.finite(X))) {
      stop("newdata must be a finite numeric matrix with the fitted variables", call. = FALSE)
    }
    if (isTRUE(object$scaling)) {
      X <- sweep(sweep(X, 2, object$Xcenter, "-"), 2, object$Xscale, "/")
    }
    Xhat <- (X %*% object$W) %*% B
  }
  colnames(Xhat) <- rownames(object$W)
  if (!scaled && isTRUE(object$scaling)) {
    Xhat <- sweep(sweep(Xhat, 2, object$Xscale, "*"), 2, object$Xcenter, "+")
  }
  Xhat
}

.s4pca_grid <- function(lambdaX, gammaX, comp) {
  if (!is.numeric(lambdaX) || !length(lambdaX) || any(!is.finite(lambdaX)) || any(lambdaX < 0)) {
    stop("lambdaX must contain non-negative finite values", call. = FALSE)
  }
  if (!is.numeric(gammaX) || !length(gammaX) || any(!is.finite(gammaX)) || any(gammaX < 0)) {
    stop("gammaX must contain non-negative finite values", call. = FALSE)
  }
  if (!is.numeric(comp) || !length(comp) || any(!is.finite(comp)) || any(comp < 1) || any(comp != as.integer(comp))) {
    stop("comp must contain positive integers", call. = FALSE)
  }
  unique(expand.grid(lambdaX = as.numeric(lambdaX), gammaX = as.numeric(gammaX),
                     comp = as.integer(comp), KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE))
}

.s4pca_one_split <- function(X, Z, grid, calibration_fraction, alpha,
                             max_overlap, muX, history.method, niterS4,
                             scaling, seed) {
  set.seed(seed)
  n <- nrow(X)
  n_cal <- max(2L, min(n - 2L, as.integer(ceiling(n * calibration_fraction))))
  cal_idx <- sort(sample.int(n, n_cal, replace = FALSE))
  train_idx <- setdiff(seq_len(n), cal_idx)
  out <- vector("list", nrow(grid))
  for (g in seq_len(nrow(grid))) {
    fit <- msma(
      X = X[train_idx, , drop = FALSE],
      Z = if (is.null(Z)) NULL else Z[train_idx],
      comp = grid$comp[g], lambdaX = grid$lambdaX[g], muX = muX,
      structure.method = "soft", gammaX = grid$gammaX[g],
      history.method = history.method, niterS4 = niterS4,
      scaling = scaling, intseed = seed + g
    )
    pred <- s4pca_reconstruct(fit, X[cal_idx, , drop = FALSE], scaled = FALSE)
    conformity <- rowMeans((X[cal_idx, , drop = FALSE] - pred)^2)
    q <- as.numeric(stats::quantile(conformity, probs = 1 - alpha, type = 8, names = FALSE))
    out[[g]] <- data.frame(
      lambdaX = grid$lambdaX[g], gammaX = grid$gammaX[g], comp = grid$comp[g],
      score = q, overlap_all = fit$overlap_all,
      overlap_selected = fit$overlap_selected,
      admissible = fit$overlap_selected <= max_overlap
    )
  }
  do.call(rbind, out)
}

#' Repeated split conformal selection for S4PCA
#'
#' Evaluates combinations of sparsity, overlap penalty, and component count by
#' reconstruction error on repeated calibration splits. The selected candidate
#' minimizes the median conformal reconstruction quantile subject to an optional
#' overlap constraint.
#'
#' @param X numeric data matrix.
#' @param Z optional numeric supervision vector or one-column matrix.
#' @param lambdaX candidate sparsity parameters.
#' @param gammaX candidate soft-structure parameters.
#' @param comp candidate component counts.
#' @param alpha miscoverage level in (0,1).
#' @param repeats positive number of repeated splits.
#' @param calibration_fraction fraction assigned to calibration in each split.
#' @param max_overlap maximum admissible \code{overlap_selected}.
#' @param muX supervision weight in [0,1].
#' @param history.method one of \code{"sumabs"}, \code{"any"}, or \code{"count"}.
#' @param niterS4 fixed S4PCA updates per component.
#' @param scaling logical data scaling indicator.
#' @param intseed random seed.
#' @param refit logical; refit the selected model on all observations.
#' @return an object of class \code{s4pca_selection}.
#' @export
s4pca_conformal_select <- function(
    X, Z = NULL, lambdaX = c(0.05, 0.1, 0.2), gammaX = c(0, 0.05, 0.1),
    comp = 2:4, alpha = 0.1, repeats = 5, calibration_fraction = 0.25,
    max_overlap = 1, muX = 1, history.method = "sumabs", niterS4 = 30,
    scaling = TRUE, intseed = 1, refit = TRUE) {
  input <- .s4pca_validate_inputs(X = X, Z = Z, comp = 1, lambda = 0,
    gamma = 0, history_method = history.method, mu = muX,
    hard_exclusion = FALSE, niter = niterS4, seed = intseed)
  X <- input$X; Z <- input$Z
  if (nrow(X) < 4L) stop("X must contain at least four rows for split selection", call. = FALSE)
  if (length(alpha) != 1L || !is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be in (0, 1)", call. = FALSE)
  }
  if (length(calibration_fraction) != 1L || !is.numeric(calibration_fraction) ||
      !is.finite(calibration_fraction) || calibration_fraction <= 0 || calibration_fraction >= 1) {
    stop("calibration_fraction must be in (0, 1)", call. = FALSE)
  }
  if (length(max_overlap) != 1L || !is.numeric(max_overlap) ||
      !is.finite(max_overlap) || max_overlap < 0 || max_overlap > 1) {
    stop("max_overlap must be in [0, 1]", call. = FALSE)
  }
  if (length(repeats) != 1L || !is.numeric(repeats) || !is.finite(repeats) ||
      repeats < 1 || repeats != as.integer(repeats)) {
    stop("repeats must be a positive integer", call. = FALSE)
  }
  if (length(refit) != 1L || !is.logical(refit) || is.na(refit)) {
    stop("refit must be TRUE or FALSE", call. = FALSE)
  }
  grid <- .s4pca_grid(lambdaX, gammaX, comp)
  split_results <- lapply(seq_len(as.integer(repeats)), function(b) {
    z <- .s4pca_one_split(X, Z, grid, calibration_fraction, alpha, max_overlap,
      muX, input$history_method, input$niter, scaling, intseed + 1000L * b)
    z[["repeat"]] <- b
    z
  })
  details <- do.call(rbind, split_results)
  key <- interaction(details$lambdaX, details$gammaX, details$comp, drop = TRUE)
  groups <- split(details, key)
  summary <- do.call(rbind, lapply(groups, function(d) data.frame(
    lambdaX = d$lambdaX[1], gammaX = d$gammaX[1], comp = d$comp[1],
    score = stats::median(d$score),
    overlap_all = stats::median(d$overlap_all),
    overlap_selected = stats::median(d$overlap_selected),
    admissible = stats::median(d$overlap_selected) <= max_overlap
  )))
  rownames(summary) <- NULL
  eligible <- which(summary$admissible)
  if (!length(eligible)) {
    warning("No candidate satisfied max_overlap; selecting from the full grid", call. = FALSE)
    eligible <- seq_len(nrow(summary))
  }
  ord <- order(summary$score[eligible], summary$overlap_selected[eligible],
               summary$comp[eligible], summary$lambdaX[eligible], summary$gammaX[eligible])
  selected <- summary[eligible[ord[1]], , drop = FALSE]
  final_fit <- NULL
  if (refit) {
    final_fit <- msma(X = X, Z = Z, comp = selected$comp,
      lambdaX = selected$lambdaX, muX = muX, structure.method = "soft",
      gammaX = selected$gammaX, history.method = input$history_method,
      niterS4 = input$niter, scaling = scaling, intseed = intseed)
  }
  result <- list(selected = selected, summary = summary, details = details,
    fit = final_fit, settings = list(alpha = alpha, repeats = as.integer(repeats),
      calibration_fraction = calibration_fraction, max_overlap = max_overlap,
      muX = muX, history.method = input$history_method,
      niterS4 = input$niter, scaling = scaling, intseed = intseed))
  class(result) <- "s4pca_selection"
  result
}


