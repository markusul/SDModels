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
#' @param formula Object of class \code{formula} or describing the model to fit 
#' of the form \code{y ~ x1 + x2 + ...} where \code{y} is a numeric response and 
#' \code{x1, x2, ...} are vectors of covariates. Interactions are not supported.
#' @param data Training data of class \code{data.frame} containing the variables in the model.
#' @param x Matrix of covariates, alternative to \code{formula} and \code{data}.
#' @param y Vector of responses, alternative to \code{formula} and \code{data}.
#' @param Q_type Type of deconfounding, one of 'trim', 'pca', 'no_deconfounding'. 
#' 'trim' corresponds to the Trim transform \insertCite{Cevid2020SpectralModels}{SDModels} 
#' as implemented in the Doubly debiased lasso \insertCite{Guo2022DoublyConfounding}{SDModels}, 
#' 'pca' to the PCA transformation\insertCite{Paul2008PreconditioningProblems}{SDModels}. 
#' See \code{\link{get_Q}}.
#' @param trim_quantile  Quantile for Trim transform, 
#' only needed for trim, see \code{\link{get_Q}}.
#' @param q_hat  Assumed confounding dimension, only needed for pca, 
#' see \code{\link{get_Q}}.
#' @param nfolds The number of folds for cross-validation. Default is 5.
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
#' @param mc.cores Number of cores to use for parallel computation `vignette("Runtime")`. 
#' The `future` package is used for parallel processing. 
#' To use custom processing plans mc.cores has to be <= 1, see [`future` package](https://future.futureverse.org/).
#' @param verbose If \code{TRUE} progress updates are shown using the `progressr` package. 
#' To customize the progress bar, see [`progressr` package](https://progressr.futureverse.org/articles/progressr-intro.html)
#' @param notRegularized A vector of indices specifying which covariates not to regularize.
#'  Default is `NULL`.
#' @return An object of class `SDAM` containing the following elements:
#' \item{X}{The original design matrix.}
#' \item{p}{The number of covariates in `X`.}
#' \item{var_names}{Names of the covariates in the training data.}
#' \item{intercept}{The intercept term of the fitted model.}
#' \item{K}{A vector of the number of basis functions for each covariate,
#' where 1 corresponds to a linear term. The entries of the vector will mostly by
#' the same, but some entries might be lower if the corresponding component of
#' X contains only few unique values.}
#' \item{breaks}{A list of breakpoints used for the B-splines. Used to reconstruct the B-spline basis functions.}
#' \item{coefs}{A list of coefficients for the B-spline basis functions for each component.}
#' \item{active}{A vector of active covariates that contribute to the model.}
#' @seealso \code{\link{get_Q}}, \code{\link{predict.SDAM}}, \code{\link{varImp.SDAM}}, 
#' \code{\link{predict_individual_fj}}, \code{\link{partDependence}}
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(10 * 5), ncol = 5)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(10)
#' model <- SDAM(x = X, y = Y, Q_type = "trim", trim_quantile = 0.5, nfold = 2, n_K = 1)
#' 
#' # if we know that the first covariate one is relevant, we can also choose to not regularize it
#' model <- SDAM(x = X, y = Y, Q_type = "trim", trim_quantile = 0.5, nfold = 2, 
#'               n_K = 1, notRegularized = c(1))
#' 
#' \donttest{
#' set.seed(22)
#' library(HDclassif)
#' data(wine)
#' names(wine) <- c("class", "alcohol", "malicAcid", "ash", "alcalinityAsh", "magnesium", 
#'                  "totPhenols", "flavanoids", "nonFlavPhenols", "proanthocyanins", 
#'                  "colIntens", "hue", "OD", "proline")
#' wine <- log(wine)
#'
#' # estimate model
#' # do not use class in the model and restrict proline to be linear 
#' model <- SDAM(alcohol ~ . - class, wine, ind_lin = "proline")
#' 
#' # extract variable importance
#' varImp(model)
#' 
#' # most important variable
#' mostImp <- names(which.max(varImp(model)))
#' mostImp
#' 
#' # predict for individual Xj
#' x <- seq(min(wine[, mostImp]), max(wine[, mostImp]), length.out = 100)
#' predJ <- predict_individual_fj(object = model, j = mostImp, x = x)
#' 
#' plot(x, predJ, 
#'      xlab = paste0("log ", mostImp), ylab = "log alcohol")
#' 
#' # partial dependece
#' plot(partDependence(model, mostImp))
#' 
#' # predict 
#' predict(model, newdata = wine[42, ])
#' 
#' ## alternative function call with customized progress bar
#' progressr::handlers(progressr::handler_txtprogressbar(char = cli::col_red(cli::symbol$heart)))
#' mod_none <- SDAM(x = as.matrix(wine[1:10, -c(1, 2)]), y = wine$alcohol[1:10], 
#'                  Q_type = "no_deconfounding", nfolds = 2, n_K = 4, 
#'                  n_lambda1 = 4, n_lambda2 = 8)
#' }
#'
#' @export
SDAM <- function(formula = NULL, data = NULL, x = NULL, y = NULL, 
                 Q_type = "trim", trim_quantile = 0.5, q_hat = 0, nfolds = 5, 
                 cv_method = "1se", n_K = 4, n_lambda1 = 10, n_lambda2 = 20, 
                 Q_scale = TRUE, ind_lin = NULL, mc.cores = 1, verbose = TRUE, 
                 notRegularized = NULL){
  input_data <- data.handler(formula = formula, data = data, x = x, y = y)
  X <- input_data$X
  Y <- input_data$Y
  
  n <- NROW(X)
  p <- NCOL(X)
  
  if(n != length(Y)) stop('X and Y must have the same number of observations')
  if(!is.numeric(nfolds) || nfolds < 2 || nfolds > n) stop('nfolds must be an integer between 2 and n')
  if(!is.numeric(n_K) || n_K < 1) stop('n_K must be a positive integer')
  if(!is.numeric(n_lambda1) || n_lambda1 < 1) stop('n_lambda1 must be a positive integer')
  if(!is.numeric(n_lambda2) || n_lambda2 < 1) stop('n_lambda2 must be a positive integer')
  if(!is.numeric(mc.cores) || mc.cores < 1) stop('mc.cores must be a positive integer')
  
  if(!is.null(ind_lin)){
    if(!is.numeric(ind_lin)){
      if(!is.character(ind_lin)) stop("ind_lin must either contain integers or variable names")
      ind_lin <- which(colnames(data.frame(X)) %in% ind_lin)
    }
    if((min(ind_lin) < 1) || max(ind_lin) > p) stop("ind_lin must contain covariates in the data in [1, p]")
  }
  
  if(!is.null(notRegularized)){
    if(!is.numeric(notRegularized)){
      if(!is.character(notRegularized)) stop("notRegularized must either contain integers or variable names")
      notRegularized <- which(colnames(data.frame(X)) %in% notRegularized)
    }
    if((min(notRegularized) < 1) || max(notRegularized) > p) stop("notRegularized must contain covariates in the data in [1, p]")
  }
  
  gprLassoControl <- grplasso::grpl.control(save.x = FALSE, save.y = FALSE, trace = 0)
  
  # create vector of candidate values for K
  # intuition: candidate values for K should be between K0 = 4 and 10*n^0.2
  K.up <- round(10*n^0.2)
  vK <- unique(round(seq(4, K.up, length.out = n_K)))
  
  # spectral transformation
  Qf <- get_Qf(X, type = Q_type, trim_quantile = trim_quantile, q_hat = q_hat, 
               scaling = Q_scale)
  QY <- Qf(Y)
  
  # get number of unique elements in each column of X
  n_unique_X <- apply(X, 2, function(x){length(unique(x))})
  
  # Generate the design and model parameters for every K in vK
  progressr::with_progress({
    pr <- progressr::progressor(along = 1:(n_K), enable = verbose)
    pr(sprintf("Design generation"), amount = 0, class = "sticky")
    if(mc.cores > 1){
      plan <- if (parallelly::supportsMulticore()) "multicore" else "multisession"
      with(future::plan(plan, workers = min(mc.cores, n_K)), local = TRUE)
    }
    
  lmodK <- future.apply::future_lapply(future.seed = TRUE, 1:length(vK), function(i){
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
    #B <- cbind(rep(1, n), matrix(nrow = n, ncol = sum(K_eff)))
    B <- matrix(1, nrow = n, ncol = 1)
    Rlist <- list()
    lbreaks <- list()
    
    # variable grouping, intercept not penalized gets NA
    #index <- c(NA, rep(1:p, times = K_eff))
    index <- NA
    for (j in 1:p){
      # number of breaks is number of basis functions minus order (4 by default) + 2
      if(K_eff[j] >= 4){
        breaks <- quantile(X[,j], probs=seq(0, 1, length.out = K_eff[j]-2))
        breaks <- unique(breaks)
        K_eff[j] <- length(breaks) + 2
        
        lbreaks[[j]] <- breaks
        Bj <- Bbasis(X[,j], breaks = breaks)
      }
      else{
        lbreaks[[j]] <- NULL
        Bj <- X[, j]
      }
      Rj.inv <- solve(chol(1/n*t(Bj) %*% Bj))
      
      #setting index of not regularized variables to NA
      index <- c(index, rep(ifelse(j %in% notRegularized, NA,j), K_eff[j]))
      B <- cbind(B, Bj %*% Rj.inv)
      
      #B[, index == j & !is.na(index)] <- Bj %*% Rj.inv
      Rlist[[j]] <- Rj.inv
    }
    QB <- Qf(B)
    
    
    # calculate maximal lambda
    lambdamax <- grplasso::lambdamax(QB, QY, index = index, model = grplasso::LinReg(), 
                                     center = FALSE, standardize = FALSE)
    # lambdas for cross validation
    if(is.finite(lambdamax)){
      lambda <- exp(seq(log(lambdamax), log(lambdamax/1000), length.out = n_lambda1))
    }else{
      lambda <- rep(0, n_lambda1)
      index <- rep(1, length(index))
    }
    pr()
    list(Rlist = Rlist, lbreaks = lbreaks, index = index, 
         QB = QB, lambda = lambda, K = K, K_eff = K_eff)
  })
  })
  
  # generate folds for CV
  ind <- sample(rep(1:nfolds, length.out = n), replace = FALSE)
  
  # calculates mse on fold l and for a listK which has the form of a lmodK[[i]]
  mse_fold_K <- function(l, listK){
    test <- ind == l

    # use capture.output to supress the output form grplasso
    # use suppressWarnings to igrnore the warnings "Penalization not adjusted to non-penalized predictors"
    # which we are aware of.
    suppressWarnings(
    mod <- grplasso::grplasso(listK$QB[!test, ], QY[!test], index = listK$index, 
                              lambda = listK$lambda, model = grplasso::LinReg(), 
                              center = FALSE, standardize = FALSE, 
                              control = gprLassoControl)
    )

    QYpred <- predict(mod, newdata = listK$QB[test, ])
    mse <- apply(QYpred, 2, function(y){mean((y - QY[test])^2)})
    pr()
    return(mse)
  }
  
  mse_fold <- function(l){
    MSEl <- future.apply::future_lapply(future.seed = TRUE, lmodK, 
                                        mse_fold_K, 
                                        l = l)
    return(unname(do.call(rbind, MSEl)))
  }
  
  #use random generator that works with multiprocessing
  ok <- RNGkind("L'Ecuyer-CMRG")
  progressr::with_progress({
    pr <- progressr::progressor(along = 1:(nfolds * n_K), enable = verbose)
    pr(sprintf("Initial cross-validation"), amount = 0, class = "sticky")
    if(mc.cores > 1){
      plan <- if (parallelly::supportsMulticore()) "multicore" else "multisession"
      with(future::plan(plan, workers = min(mc.cores, nfolds)), local = TRUE)
    }
    MSES <- lapply(X = 1:nfolds, mse_fold)
  })
  
  #if(mc.cores == 1){
  #  MSES <- pbapply::pblapply(1:nfolds, mse_fold) 
  #} else {
  #  MSES <- parallel::mclapply(1:nfolds, mse_fold, mc.cores = mc.cores)
  #}
  
  # aggregate MSEs over folds
  MSES.agg <- Reduce("+", MSES) / nfolds
  ind.min <- which(MSES.agg == min(MSES.agg), arr.ind = TRUE)
  K.min <- vK[ind.min[1]]
  lambda.min <- lmodK[[ind.min[1]]]$lambda[ind.min[2]]
  
  # refit model for K.min and find best value for lambda in the neighborhood of lambda.min
  modK.min <- lmodK[[ind.min[1]]]
  
  if(lambda.min == 0){
    modK.min$lambda <- rep(0, n_lambda2)
  }else{
    modK.min$lambda <- exp(seq(log(lambda.min * 10), log(lambda.min/10), 
                               length.out = n_lambda2))
  }
  
  progressr::with_progress({
    pr <- progressr::progressor(along = 1:nfolds, enable = verbose)
    pr(sprintf("Second stage cross-validation"), amount = 0, class = "sticky")
    if(mc.cores > 1){
      plan <- if (parallelly::supportsMulticore()) "multicore" else "multisession"
      with(future::plan(plan, workers = min(mc.cores, nfolds)), local = TRUE)
    }
    MSES1 <- future.apply::future_lapply(future.seed = TRUE, 
                                         X = 1:nfolds, 
                                         mse_fold_K, 
                                         listK = modK.min)
  })
  #if(verbose) print("Second stage cross-validation")
  #if(mc.cores == 1){
  #  MSES1 <- pbapply::pblapply(1:nfolds, mse_fold_K, listK = modK.min)
  #} else {
  #  MSES1 <- parallel::mclapply(1:nfolds, mse_fold_K, listK = modK.min, 
  #                              mc.cores = mc.cores)
  #}
  
  MSES1 <- do.call(rbind, MSES1)
  MSE1.agg <- apply(MSES1, 2, mean)
  se.agg <- 1/sqrt(nfolds) * apply(MSES1, 2, sd)
  ind.min1 <- which.min(MSE1.agg)
  
  if(cv_method == "min"){
    lambdastar <- modK.min$lambda[ind.min1]
  } else {
    if(cv_method != "1se"){
      warning("CV method not implemented. Taking '1se'.")
    }
    lambdastar <- max(modK.min$lambda[MSE1.agg <= MSE1.agg[ind.min1]+se.agg[ind.min1]])
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
  #index <- modK.min$index
  index <- c(NA, unlist(lapply(1:p, function(j) rep(j, modK.min$K_eff[j]))))

  Rlist <- modK.min$Rlist
  for(j in 1:p){
    cj <- mod$coefficients[index == j & !is.na(index)]
    if(sum(cj^2) != 0){
      active <- c(active, j)
      # transform back
      lcoef[[j]] <- Rlist[[j]] %*% cj
    } else {
      lcoef[[j]] <- 0
    }
  }
  intercept <- mod$coefficients[1]
  lreturn <- list()
  
  # original covariates
  lreturn$X <- X
  lreturn$p <- NCOL(X)
  lreturn$var_names = colnames(data.frame(X))
  
  # intercept
  lreturn$intercept <-intercept
  
  # number of basis functions for each component
  lreturn$K <- modK.min$K_eff
  
  # list of breaks of B-spline basis
  lreturn$breaks <- modK.min$lbreaks
  
  # list of coefficients
  names(lcoef) <- lreturn$var_names
  lreturn$coefs <- lcoef
  
  # estimated active set
  lreturn$active <- active
  class(lreturn) <- "SDAM"
  RNGkind(ok[1])
  return(lreturn)
}









