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
#' n <- 50
#' X <- matrix(rnorm(n * 20), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDTree(x = X, y = y, Q_type = 'no_deconfounding')
#' predict(model, newdata = data.frame(X))
#' @seealso \code{\link{SDTree}}
#' @export
predict.SDTree <- function(object, newdata, ...){
  if(!is.data.frame(newdata)) stop('newdata must be a data.frame')
  
  X <- data.handler(~., newdata)$X
  if(!all(object$var_names %in% colnames(X))) stop('newdata must contain all covariates used for training')

  X <- X[, object$var_names]
  if(any(is.na(X))) stop('X must not contain missing values')
  
  predict_outsample(object$tree, X)
}

#' Predictions for the SDForest
#' 
#' Predicts the response for new data using a fitted SDForest.
#' @author Markus Ulmer
#' @param object Fitted object of class \code{SDForest}.
#' @param newdata New test data of class \code{data.frame} containing
#' the covariates for which to predict the response.
#' @param ... Further arguments passed to or from other methods.
#' @return A vector of predictions for the new data.
#' @examples
#' set.seed(1)
#' n <- 50
#' X <- matrix(rnorm(n * 20), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', nTree = 10)
#' predict(model, newdata = data.frame(X))
#' @seealso \code{\link{SDForest}}
#' @export
predict.SDForest <- function(object, newdata, ...){
  # predict function for the spectral deconfounded random forest
  # using the mean over all trees as the prediction
  # check data type
  if(!is.data.frame(newdata)) stop('newdata must be a data.frame')
  
  X <- data.handler(~., newdata)$X
  if(!all(object$var_names %in% colnames(X))) stop('newdata must contain all covariates used for training')

  X <- X[, object$var_names]
  if(any(is.na(X))) stop('X must not contain missing values')

  pred <- do.call(cbind, lapply(object$forest, function(x){predict_outsample(x$tree, X)}))
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
    xi <- X[i, ]
    predictions <- sapply(oob_ind[[i]], function(model){
      predict_outsample(object$forest[[model]]$tree, xi)
    })
    mean(predictions)
  })
}



#' Predictions for SDAM
#'
#' Predicts the response for new data using a fitted SDAM.
#' @author Cyrill Scheidegger
#' @param object Fitted object of class \code{SDAM}.
#' @param Xnew Matrix of new test data at which to evaluate the fitted function.
#' @param ... Further arguments passed to or from other methods.
#' @return A vector of predictions for the new data.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20 * 15), ncol = 15)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(20)
#' model <- SDAM(X, Y, Q_type = "trim", trim_quantile = 0.5, cv_k = 5)
#' predict(object = model, Xnew = X)
#' @export
predict.SDAM <- function(object, Xnew, ...){
  n <- nrow(Xnew)
  p <- ncol(Xnew)
  if(p != object$p){
    stop("Xnew must have same number of columns as training data.")
  }
  intercept <- object$intercept
  breaks_list <- object$breaks
  coefs_list <- object$coefs
  # Initialize prediction vector
  y_pred <- rep(intercept, n)
  
  # Calculate contributions from active variables
  for (j in object$active) {
    if (!is.null(breaks_list[[j]])) {
      Bj <- Bbasis(Xnew[, j], breaks = breaks_list[[j]])
      y_pred <- y_pred + Bj %*% coefs_list[[j]]
    } else {
      y_pred <- y_pred + Xnew[, j] * c(coefs_list[[j]])
    }
  }
  return(y_pred)
}


#' Predictions of individual component functions for SDAM
#'
#' Predicts the contribution of an individual component j using fitted SDAM.
#' @author Cyrill Scheidegger
#' @param object Fitted object of class \code{SDAM}.
#' @param Xjnew Vector of new test data at which to evaluate fj, i.e. the contribution
#' of the j-th component of X.
#' @param j Which component to evaluate.
#' @return A vector of predictions for fj evaluated at Xjnew.
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20 * 15), ncol = 15)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(20)
#' model <- SDAM(X, Y, Q_type = "trim", trim_quantile = 0.5, cv_k = 5)
#' predict_individual_fj(object = model, Xjnew = X[, 1], j = 1)
#' @export
predict_individual_fj <- function(object, Xjnew, j){
  if (!(j %in% object$active)) {
    return(rep(0, length(Xjnew)))
  }
  breaks_j <- object$breaks[[j]]
  coefs_j <- object$coefs[[j]]
  
  if (!is.null(breaks_j)) {
    Bj <- Bbasis(Xjnew, breaks = breaks_j)
    return(Bj %*% coefs_j)
  } else {
    return(Xjnew * c(coefs_j))
  }
}