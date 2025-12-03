#' Print SDForest
#' 
#' Print contents of the SDForest.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{SDForest}.
#' @param ... Further arguments passed to or from other methods.
#' @return No return value, called for side effects
#' @seealso \code{\link{SDForest}}
#' @method print SDForest
#' @examples
#' 
#' set.seed(1)
#' n <- 50
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', nTree = 5, cp = 0.5)
#' print(model)
#' @export
print.SDForest <- function(x, ...){
  cat("SDForest result\n\n")
  cat("Number of trees: ", length(x$forest), "\n")
  cat("Number of covariates: ", length(x$var_names), "\n")
  if(!is.null(x$oob_loss)){
    cat("OOB loss: ", round(x$oob_loss, 10), "\n")
    cat("OOB spectral loss: ", round(x$oob_SDloss, 10), "\n")
  }
}


#' Print SDAM
#'
#' Print number of covariates and number of active covariates for SDAM object.
#' @author Cyrill Scheidegger
#' @param x Fitted object of class \code{SDAM}.
#' @param ... Further arguments passed to or from other methods.
#' @return No return value, called for side effects
#' @seealso \code{\link{SDAM}}
#' @method print SDAM
#' @examples
#' 
#' set.seed(1)
#' X <- matrix(rnorm(10 * 5), ncol = 5)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(10)
#' model <- SDAM(x = X, y = Y, Q_type = "trim", trim_quantile = 0.5, nfold = 2, n_K = 1)
#' print(model)
#' @export
print.SDAM <- function(x, ...){
  cat("SDAM result\n\n")
  cat("Number of covariates: ", x$p, "\n")
  cat("Number of active covariates: ", length(x$active), "\n")
}
