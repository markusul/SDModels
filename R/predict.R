#' Predictions for the SDTree
#' 
#' Predicts the response for new data using a fitted SDTree.
#' @author Markus Ulmer
#' @param object Fitted object of class \code{SDTree}.
#' @param newdata New test data of class \code{data.frame} containing 
#' the covariates for which to predict the response.
#' @param ... Further arguments passed to or from other methods.
#' @return A vector of predictions for the new data.
#' @examples
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDTree(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5)
#' predict(model, newdata = data.frame(X))
#' @seealso \code{\link{SDTree}}
#' @export
predict.SDTree <- function(object, newdata, ...){
  if(!is.data.frame(newdata)) stop('newdata must be a data.frame')
  
  X <- data.handler(~., newdata)$X
  if(!all(object$var_names %in% colnames(X))) stop('newdata must contain all covariates used for training')

  X <- as.matrix(X[, object$var_names])
  if(any(is.na(X))) stop('X must not contain missing values')
  
  traverse_tree(object$tree, X)
}

#' Predictions for the SDForest
#' 
#' Predicts the response for new data using a fitted SDForest.
#' @author Markus Ulmer
#' @param object Fitted object of class \code{SDForest}.
#' @param newdata New test data of class \code{data.frame} containing
#' the covariates for which to predict the response.
#' @param mc.cores Number of cores to use for parallel processing,
#' if \code{mc.cores > 1} the trees predict in parallel.
#' @param ... Further arguments passed to or from other methods.
#' @return A vector of predictions for the new data.
#' @examples
#' set.seed(1)
#' n <- 50
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', nTree = 5, cp = 0.5)
#' predict(model, newdata = data.frame(X))
#' @seealso \code{\link{SDForest}}
#' @export
predict.SDForest <- function(object, newdata, mc.cores = 1, ...){
  # predict function for the spectral deconfounded random forest
  # using the mean over all trees as the prediction
  # check data type
  if(!is.data.frame(newdata)) stop('newdata must be a data.frame')
  
  X <- data.handler(~., newdata)$X
  if(!all(object$var_names %in% colnames(X))) stop('newdata must contain all covariates used for training')

  X <- matrix(X[, object$var_names], nrow = nrow(X))
  colnames(X) <- object$var_names
  
  if(any(is.na(X))) stop('X must not contain missing values')
  
  worker_fun <- function(tree){
    preds_i <- tryCatch({
      preds <- traverse_tree(tree[["tree"]], X)
      list(ok = TRUE, preds = preds)
    }, error = function(e) {
      list(ok = FALSE, error = conditionMessage(e))
    }, warning = function(w) {
      # convert warnings to tagged results if needed
      list(ok = TRUE, tree = NULL, warning = conditionMessage(w))
    })
    preds_i
  }
  if(mc.cores > 1){
    if(Sys.info()[["sysname"]] == "Linux"){
      preds_list <- parallel::mclapply(object$forest, 
                                       worker_fun, 
                                       mc.cores = mc.cores)
    }else{
      future::plan('multisession', workers = mc.cores)
      preds_list <- future.apply::future_lapply(future.seed = TRUE, 
                                                X = object$forest, 
                                                worker_fun)
    }
  }else{
    preds_list <- pbapply::pblapply(object$forest, worker_fun)
  }
  
  #check worker statuses
  failed_workers <- which(vapply(preds_list, function(z) !isTRUE(z$ok), logical(1)))
  if (length(failed_workers) > 0) {
    stop(sprintf("SDForest: %d worker(s) failed, first error: %s",
                 length(failed_workers), preds_list[[failed_workers[1]]]$error))
  }
  preds <- lapply(preds_list, function(preds_i) preds_i$preds)
  pred <- do.call(cbind, preds)
  rowMeans(pred)
}

#' Out-of-bag predictions for the SDForest
#' 
#' Predicts the response for the training data 
#' using only the trees in the SDForest 
#' that were not trained on the observation.
#' @author Markus Ulmer
#' @param object Fitted object of class \code{SDForest}.
#' @param X Covariates of the training data.
#' If \code{NULL}, the data saved in the object is used.
#' @return A vector of out-of-bag predictions for the training data.
#' #' set.seed(1)
#' n <- 50
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', nTree = 5, cp = 0.5)
#' predictOOB(model)
#' @seealso \code{\link{SDForest}} \code{\link{prune.SDForest}} \code{\link{plotOOB}}
#' @export
predictOOB <- function(object, X = NULL){
  if(is.null(X)){
    X <- object$X
  }
  if(!is.matrix(X)) stop('X must be a matrix and either provided or in the object')

  n <- nrow(X)

  if(ncol(X) != length(object$var_names)){
    stop("The number of covariates has to correspond to the data used for training the forest.")
  }

  oob_ind <- object$oob_ind

  sapply(1:n, function(i){
    if(length(oob_ind[[i]]) == 0){
      return(NA)
    }
    xi <- matrix(X[i, ], nrow = 1)
    
    model_idx <- oob_ind[[i]]
    model_idx <- model_idx[model_idx <= length(object$forest)]
    predictions <- sapply(model_idx, function(model){
      traverse_tree(object$forest[[model]]$tree, xi)
    })
    mean(predictions)
  })
}



#' Predictions for SDAM
#'
#' Predicts the response for new data using a fitted SDAM.
#' @author Cyrill Scheidegger
#' @param object Fitted object of class \code{SDAM}.
#' @param newdata New test data of class \code{data.frame} containing
#' the covariates for which to predict the response.
#' @param ... Further arguments passed to or from other methods.
#' @return A vector of predictions for the new data.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(10 * 5), ncol = 5)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(10)
#' model <- SDAM(x = X, y = Y, Q_type = "trim", trim_quantile = 0.5, nfold = 2, n_K = 1)
#' predict(model, newdata = data.frame(X))
#' @seealso \code{\link{SDAM}}
#' @export
predict.SDAM <- function(object, newdata, ...){
  if(!is.data.frame(newdata)) stop('newdata must be a data.frame')
  
  X <- data.handler(~., newdata)$X
  if(!all(object$var_names %in% colnames(X))) stop('newdata must contain all covariates used for training')
  
  X <- X[, object$var_names]
  if(any(is.na(X))) stop('X must not contain missing values')

  if(is.null(nrow(X))) X <- matrix(X, ncol = object$p)
  
  intercept <- object$intercept
  breaks_list <- object$breaks
  coefs_list <- object$coefs
  # Initialize prediction vector
  y_pred <- rep(intercept, nrow(X))
  
  # Calculate contributions from active variables
  for (j in object$active) {
    if (length(breaks_list) != 0 && !is.null(breaks_list[[j]])) {
      Bj <- Bbasis(X[, j], breaks = breaks_list[[j]])
      y_pred <- y_pred + Bj %*% coefs_list[[j]]
    } else {
      y_pred <- y_pred + X[, j] * c(coefs_list[[j]])
    }
  }
  return(c(y_pred))
}


#' Predictions of individual component functions for SDAM
#'
#' Predicts the contribution of an individual component j using a fitted SDAM.
#' @author Cyrill Scheidegger
#' @param object Fitted object of class \code{SDAM}.
#' @param j Which component to evaluate.
#' @param x New numeric data to predict for.
#' @return A vector of predictions for fj evaluated at Xjnew.
#' @seealso \code{\link{SDAM}}
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(10 * 5), ncol = 5)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(10)
#' model <- SDAM(x = X, y = Y, Q_type = "trim", trim_quantile = 0.5, nfold = 2, n_K = 1)
#' predict_individual_fj(model, j = 1, seq(-2, 2, length.out = 100))
#' @export
predict_individual_fj <- function(object, j, x = NULL){
  if(is.character(j)) j <- which(object$var_names %in% j)
  if(length(j) == 0) 
    stop("j has not be an integer in [1, p] or a covariate name used for training")
  
  if(!is.numeric(j) | length(j) != 1 | j > object$p | j < 1) 
    stop("j has to be an integer in [1, p] or a covariate name used for training")

  if(!is.null(x)){
    if(!is.numeric(x)) stop('x must be a numeric vector')
    if(any(is.na(x))) stop('x must not contain missing values')
  }else{
    x <- object$X[, j]
  }
    
  if (!(j %in% object$active)) {
    return(rep(0, length(x)))
  }
  
  if(length(object$breaks) == 0){
    breaks_j <- NULL
  }else{
    breaks_j <- object$breaks[[j]]
  }
  coefs_j <- object$coefs[[j]]
  
  if (!is.null(breaks_j)) {
    Bj <- Bbasis(x, breaks = breaks_j)
    return(c(Bj %*% coefs_j))
  } else {
    return(x * c(coefs_j))
  }
}