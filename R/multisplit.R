#' Multi Split conformal prediction intervals with Multivariate Response
#'
#' Compute prediction intervals using Multi Split conformal inference with
#' multivariate response.
#'
#' @param x The feature variables, a matrix nxp.
#' @param y The matrix of multivariate responses (dimension nxq)
#' @param x0 The new points to evaluate, a matrix of dimension n0xp.
#' @param train.fun A function to perform model training, i.e., to produce an
#'   estimator of E(Y|X), the conditional expectation of the response variable
#'   Y given features X. Its input arguments should be x: matrix of features,
#'   and y: matrix of responses.
#' @param predict.fun A function to perform prediction for the (mean of the)
#'   responses at new feature values. Its input arguments should be out: output
#'   produced by train.fun, and newx: feature values at which we want to make
#'   predictions.
#' @param alpha Miscoverage level for the prediction intervals, i.e., intervals
#'   with coverage 1-alpha are formed. Default for alpha is 0.1.
#' @param split Indices that define the data-split to be used (i.e., the indices
#'   define the first half of the data-split, on which the model is trained).
#'   Default is NULL, in which case the split is chosen randomly.
#' @param seed Integer to be passed to set.seed before defining the random
#'   data-split to be used. Default is FALSE, which effectively sets no seed.
#'   If both split and seed are passed, the former takes priority and the latter
#'   is ignored.
#' @param randomized Should the randomized approach be used? Default is FALSE.
#' @param verbose Should intermediate progress be printed out? Default is FALSE.
#' @param training_size Split proportion between training and calibration set.
#' Default is 0.5.
#' @param s_type The type of modulation function.
#'  Currently we have 3 options: "identity","st-dev","alpha-max". Default is "std-dev"
#' @param B Number of repetitions. Default is 100.
#' @param lambda Smoothing parameter. Default is 0.
#' @param tau It is a smoothing parameter:
#' tau=1-1/B  Bonferroni intersection method
#' tau=0 unadjusted intersection
#' Default is 1-(B+1)/(2*B).
#' @param seed_beta The seed for the randomized version. Default is FALSE.
#' @param score The chosen score for the split conformal function.
#'
#' @return A list with the following components x0, lo, up. In particular lo and up
#' are matrices of dimension n0 x q.
#'
#' @details The work is an extension of the univariate approach to Multi Split
#' conformal inference to a multivariate context.
#' @details This function is based on the package \code{\link{future.apply}} to
#'  perform parallelization.
#'
#' @references "Multi Split Conformal Prediction" by Solari, Djordjilovic (2021) is
#' the baseline for the univariate case.
#'
#' @example inst/examples/ex.msplit.R
#' @export conformal.multidim.msplit



conformal.multidim.msplit = function(x,y, x0, train.fun, predict.fun, alpha=0.1,
                       split=NULL, seed=FALSE, randomized=FALSE,seed_beta=FALSE,
                       verbose=FALSE, training_size=NULL,score = "max",
                       s_type = "st-dev",B=100,lambda=0,
                       tau = 1-(B+1)/(2*B)) {



  if(is.null(training_size) || length(training_size)!=B)
    training_size=rep(0.5,B)

  if (!is.null(seed)) set.seed(seed)

  n0=nrow(x0)
  p=ncol(x0)
  q=ncol(y)
  n=nrow(x)
  full=q*n0
  loB<-upB<-matrix(0,nrow=B,ncol=full)



  future::plan(future::multisession)
  options(future.rng.onMisuse="ignore")

  lo_up <- t(future.apply::future_sapply(1:B, function(bbb) {


    out<-conformal.multidim.split(x,y, x0, train.fun, predict.fun,
                               alpha*(1-tau) + (alpha*lambda)/B,
                             split, seed+bbb, randomized,seed_beta,
                             verbose, training_size[bbb] ,score, s_type)

    return(cbind(t(out$lo),t(out$up)))

      }))


  Y = rbind(lo_up[,1:full],lo_up[,-(1:full)])
  tr <- tau*B + .001
  finalInt = t(future.apply::future_sapply(1:full, function(kk) interval.build(Y[,kk],B,tr)))

  lo<-matrix(finalInt[,1], nrow = n0, ncol = q, byrow = TRUE)
  up<-matrix(finalInt[,2], nrow = n0, ncol = q, byrow = TRUE)


  ## To avoid CRAN check errors
  ## R CMD check: make sure any open connections are closed afterward
  future::plan(future::sequential)



  return(list(lo=lo,up=up,x0=x0))
}


