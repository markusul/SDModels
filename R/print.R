#' Print a SDTree
#' 
#' Print contents of the SDTree.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{SDTree}.
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{SDTree}}
#' @method print SDTree
#' @export
print.SDTree <- function(x, ...){
  # print function for the spectral deconfounded tree
  print(x$tree, 'value', 's', 'j', 'label', 'decision', 'n_samples')
}

#' Print SDForest
#' 
#' Print contents of the SDForest.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{SDForest}.
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{SDForest}}
#' @method print SDForest
#' @export
print.SDForest <- function(x, ...){
  cat("SDForest result\n\n")
  cat("Number of trees: ", length(x$forest), "\n")
  cat("Number of covariates: ", length(x$var_names), "\n")
  if(!is.null(x$oob_loss)){
    cat("OOB loss: ", round(x$oob_loss, 2), "\n")
    cat("OOB spectral loss: ", round(x$oob_SDloss, 2), "\n")
  }
}


#' Print SDAM
#'
#' Print number of covariates and number of active covariates for SDAM object.
#' @author Cyrill Scheidegger
#' @param x Fitted object of class \code{SDAM}.
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{SDAM}}
#' @method print SDAM
#' @export
print.SDAM <- function(x, ...){
  cat("SDAM result\n\n")
  cat("Number of covariates: ", x$p, "\n")
  cat("Number of active covariates: ", length(x$active), "\n")
}
