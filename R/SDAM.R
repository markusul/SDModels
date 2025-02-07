#' Spectrally Deconfounded Additive Models
#'
#' Estimate high-dimensional additive models using spectral deconfounding \insertCite{scheidegger2023spectral}{SDModels}.
#' The covariates are expanded into B-spline basis functions. A spectral
#' transformation is used to remove bias arising from hidden confounding and
#' a group lasso objective is minimized to enforce component-wise sparsity.
#' Optimal number of basis functions per component and sparsity penalty are
#' chosen by cross validation.
#'@references
#'  \insertAllCited{}
#' @author Cyrill Scheidegger
#' @param X A numeric matrix of size \eqn{n\times p}, where \eqn{n} is the number of observations and \eqn{p} is the number of covariates.
#' @param Y A numeric vector of length \eqn{n}, representing the response variable.
#' @param Q_type Type of deconfounding, one of 'trim', 'pca', 'no_deconfounding'. 
#' 'trim' corresponds to the Trim transform \insertCite{Cevid2020SpectralModels}{SDModels} 
#' as implemented in the Doubly debiased lasso \insertCite{Guo2022DoublyConfounding}{SDModels}, 
#' 'pca' to the PCA transformation\insertCite{Paul2008PreconditioningProblems}{SDModels}. 
#' See \code{\link{get_Q}}.
#' @param trim_quantile  Quantile for Trim transform, 
#' only needed for trim, see \code{\link{get_Q}}.
#' @param q_hat  Assumed confounding dimension, only needed for pca, 
#' see \code{\link{get_Q}}.
#' @param cv_k The number of folds for cross-validation. Default is 5.
#' @param cv_method The method for selecting the regularization parameter during cross-validation.
#' One of "min" (minimum cv-loss) and "1se" (one-standard-error rule) Default is "1se".
#' @param n_K The number of candidate values for the number of basis functions for B-splines. Default is 4.
#' @param n_lambda1 The number of candidate values for the regularization parameter in the initial cross-validation step. Default is 10.
#' @param n_lambda2 The number of candidate values for the regularization parameter in the second stage of cross-validation
#' (once the optimal number of basis function K is decided, a second stage of cross-validation for the regularization parameter
#' is performed on a finer grid). Default is 20.
#' @param Q_scale  Should data be scaled to estimate the spectral transformation? 
#' Default is \code{TRUE} to not reduce the signal of high variance covariates.
#' @param ind_lin A vector of indices specifying which covariates to model linearly (i.e. not expanded into basis function).
#'  Default is `NULL`.
#' @param mc.cores  Number of cores to use for parallel processing, if \code{mc.cores > 1}
#' the cross validation is parallelized. Default is `1`. (only supported for unix)
#'
#' @return An object of class `SDAM` containing the following elements:
#' \item{X}{The original design matrix.}
#' \item{p}{The number of covariates in `X`.}
#' \item{intercept}{The intercept term of the fitted model.}
#' \item{K}{A vector of the number of basis functions for each covariate,
#' where 1 corresponds to a linear term. The entries of the vector will mostly by
#' the same, but some entries might be lower if the corresponding component of
#' X contains only few unique values.}
#' \item{breaks}{A list of breakpoints used for the B-splines. Used to reconstruct the B-spline basis functions.}
#' \item{coefs}{A list of coefficients for the B-spline basis functions for each component.}
#' \item{active}{A vector of active covariates that contribute to the model.}
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20 * 15), ncol = 15)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(20)
#' model <- SDAM(X, Y, Q_type = "trim", trim_quantile = 0.5, cv_k = 5)
#' print(model)
#'
#' @export
SDAM <- function(X, Y, Q_type = "trim", trim_quantile = 0.5, q_hat = 0, cv_k = 5, 
                 cv_method = "1se", n_K = 4, n_lambda1 = 10, n_lambda2 = 20, 
                 Q_scale = TRUE, ind_lin = NULL, mc.cores = 1){
  n <- NROW(X)
  p <- NCOL(X)
  
  gprLassoControl <- grplasso::grpl.control(save.x = FALSE, save.y = FALSE, trace = 0)
  
  # create vector of candidate values for K
  # intuition: candidate values for K should be between K0 = 4 and 10*n^0.2
  K.up <- round(10*n^0.2)
  vK <- unique(round(seq(4, K.up, length.out = n_K)))
  
  # spectral transformation
  Qf <- get_Qf(X, type = Q_type, trim_quantile = trim_quantile, q_hat = q_hat, 
               gpu = FALSE, scaling = Q_scale)
  QY <- Qf(Y)
  
  # get number of unique elements in each column of X
  n_unique_X <- apply(X, 2, function(x){length(unique(x))})
  
  # Generate the design and model parameters for every K in vK
  lmodK <- list()
  for (i in 1:length(vK)){
    K <- vK[i]
    # effective number of basis functions for each Xj, j = 1,..., p
    # K_eff[j] can be at most equal to the number of unique values of Xj
    # K_eff[j] is set to 1 for all j in ind_lin
    # K_eff[j] is set to 1 if K_eff[j]<=3, since B-spline needs 4 basis functions
    K_eff <- rep(K, p)
    K_eff[ind_lin] <- 1
    K_eff <- pmin(K_eff, n_unique_X)
    K_eff[K_eff < 4] <- 1
    
    # first column is intercept
    B <- cbind(rep(1, n), matrix(nrow = n, ncol = sum(K_eff)))
    Rlist <- list()
    lbreaks <- list()
    
    # variable grouping, intercept not penalized gets NA
    index <-c(NA, rep(1:p, times = K_eff))
    for (j in 1:p){
      # number of breaks is number of basis functions minus order (4 by default) + 2
      if(K_eff[j] >= 4){
        breaks <- quantile(X[,j], probs=seq(0, 1, length.out = K_eff[j]-2))
        lbreaks[[j]] <- breaks
        Bj <- Bbasis(X[,j], breaks = breaks)
      }
      else{
        lbreaks[[j]] <- NULL
        Bj <- X[, j]
      }
      Rj.inv <- solve(chol(1/n*t(Bj) %*% Bj))
      B[, index == j & !is.na(index)] <- Bj %*% Rj.inv
      Rlist[[j]] <- Rj.inv
    }
    QB <- Qf(B)
    
    # calculate maximal lambda
    lambdamax <- grplasso::lambdamax(QB, QY, index = index, model = grplasso::LinReg(), 
                                     center = FALSE, standardize = FALSE)
    # lambdas for cross validation
    lambda <- exp(seq(log(lambdamax), log(lambdamax/1000), length.out = n_lambda1))
    lmodK[[i]] <- list(Rlist = Rlist, lbreaks = lbreaks, index = index, B = B, 
                       QB = QB, lambda = lambda, K = K, K_eff = K_eff)
  }
  
  # generate folds for CV
  ind <-  sample(rep(1:cv_k, length.out = n), replace = FALSE)
  
  # calculates mse on fold l and for a listK which has the form of a lmodK[[i]]
  mse_fold_K <- function(l, listK){
    test <- ind == l
    train <- !test
    
    #test <- which(ind == l)
    #QYtrain <- QY[-test]
    #QYtest <- QY[test]
    #QBtrain <- listK$QB[-test, ]
    #QBtest <- listK$QB[test, ]
    
    # use capture.output to supress the output form grplasso
    # use suppressWarnings to igrnore the warnings "Penalization not adjusted to non-penalized predictors"
    # which we are aware of.
    suppressWarnings(
    mod <- grplasso::grplasso(listK$QB[test, ], QY[test], index = listK$index, 
                              lambda = listK$lambda, model = grplasso::LinReg(), 
                              center = FALSE, standardize = FALSE, 
                              control = gprLassoControl)
    )
    
    QYpred <- predict(mod, newdata = listK$QB[test, ])
    mse <- apply(QYpred, 2, function(y){mean((y - QY[test])^2)})
    return(mse)
  }
  
  mse_fold <- function(l){
    MSEl <- lapply(lmodK, function(listK){mse_fold_K(l, listK)})
    return(unname(do.call(rbind, MSEl)))
  }
  
  if(mc.cores == 1){
    MSES <- pbapply::pblapply(1:cv_k, mse_fold) 
  } else {
    MSES <- parallel::mclapply(1:cv_k, mse_fold, mc.cores = mc.cores)
  }
  
  # aggregate MSEs over folds
  MSES.agg <- Reduce("+", MSES) / cv_k
  ind.min <- which(MSES.agg == min(MSES.agg), arr.ind = TRUE)
  K.min <- vK[ind.min[1]]
  lambda.min <- lmodK[[ind.min[1]]]$lambda[ind.min[2]]
  
  # refit model for K.min and find best value for lambda in the neighborhood of lambda.min
  modK.min <- lmodK[[ind.min[1]]]
  modK.min$lambda <- exp(seq(log(lambda.min * 10), log(lambda.min/10), 
                             length.out = n_lambda2))
  
  if(mc.cores == 1){
    MSES1 <- lapply(1:cv_k, mse_fold_K, listK = modK.min)
  } else {
    MSES1 <- parallel::mclapply(1:cv_k, mse_fold_K, listK = modK.min, 
                                mc.cores = mc.cores)
  }
  
  MSES1 <- do.call(rbind, MSES1)
  MSE1.agg <- apply(MSES1, 2, mean)
  se.agg <- 1/sqrt(cv_k) * apply(MSES1, 2, sd)
  ind.min1 <- which(MSE1.agg == min(MSE1.agg))
  
  if(cv_method == "min"){
    lambdastar <- modK.min$lambda[ind.min1]
  } else {
    if(cv_method != "1se"){
      warning("CV method not implemented. Taking '1se'.")
    }
    lambdastar <- max(modK.min$lambda[which(MSE1.agg <= MSE1.agg[ind.min1]+se.agg[ind.min1])])
  }
  
  ## fit model on full data with K.min and lambdastar
  suppressWarnings(
  mod <- grplasso::grplasso(modK.min$QB, QY, index = modK.min$index, 
                            lambda = lambdastar, model = grplasso::LinReg(), 
                            center = FALSE, standardize = FALSE, 
                            control = gprLassoControl)
  )
  
  # transform back to original scale
  lcoef <- list()
  active <- numeric()
  running_ind <- 1
  index <- modK.min$index
  Rlist <- modK.min$Rlist
  for(j in 1:p){
    cj <- mod$coefficients[index == j & !is.na(index)]
    if(sum(cj^2) != 0){
      active <- c(active, j)
      # transform back
      lcoef[[j]] <- Rlist[[j]] %*% cj
    }
  }
  
  intercept <- mod$coefficients[1]
  lreturn <- list()
  
  # original covariates
  lreturn$X <- X
  lreturn$p <- NCOL(X)
  
  # intercept
  lreturn$intercept <-intercept
  
  # number of basis functions for each component
  lreturn$K <- modK.min$K_eff
  
  # list of breaks of B-spline basis
  lreturn$breaks <- modK.min$lbreaks
  
  # list of coefficients
  lreturn$coefs <- lcoef
  
  # estimated active set
  lreturn$active <- active
  class(lreturn) <- "SDAM"
  return(lreturn)
}









