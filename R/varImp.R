#' @export 
varImp <- function(object) {UseMethod("varImp")}

#' Extract variable importance of an SDTree
#' 
#' This function extracts the variable importance of an SDTree. 
#' The variable importance is calculated as the sum of the decrease in the loss 
#' function resulting from all splits that use this covariate.
#' @author Markus Ulmer
#' @param object an SDTree object
#' @return A named vector of variable importance
#' @seealso \code{\link{varImp.SDForest}} \code{\link{SDTree}}
#' @examples
#' data(iris)
#' tree <- SDTree(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris, cp = 0.5)
#' varImp(tree)
#' @export
varImp.SDTree <- function(object){
  j_dec <- matrix(object$tree[object$tree[, "leaf"] == 2, c("j", "res_dloss")], 
                  ncol = 2, dimnames = list(NULL, c("j", "res_dloss")))
  var_importance <- rep(0, length(object$var_names))
  
  imp <- tapply(j_dec[, "res_dloss"], j_dec[, "j"], sum)

  var_importance[as.numeric(names(imp))] <- imp
  names(var_importance) <- object$var_names
  var_importance
}

#' Extract variable importance of an SDForest
#' 
#' This function extracts the variable importance of an SDForest.
#' The variable importance is calculated as the sum of the decrease in the loss function 
#' resulting from all splits that use a covariate for each tree. 
#' The mean of the variable importance of all trees results in the variable importance for the forest.
#' @author Markus Ulmer
#' @param object an SDForest object
#' @return A named vector of variable importance
#' @seealso \code{\link{varImp.SDTree}} \code{\link{SDForest}}
#' @aliases varImp
#' @examples
#' data(iris)
#' fit <- SDForest(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, 
#'                  iris, nTree = 10, cp = 0.5)
#' varImp(fit)
#' @export
varImp.SDForest <- function(object){
  rowMeans(do.call(cbind, lapply(object$forest, function(tree) varImp(tree))))
}

#' Extract Variable importance for SDAM
#'
#' This function extracts the variable importance of an SDAM.
#' The variable importance is calculated as the empirical squared L2 norm of fj. 
#' The measure is not standardized.
#' @author Cyrill Scheidegger
#' @param object an \code{SDAM} object
#' @return A vector of variable importance
#' @seealso \code{\link{SDAM}}
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(10 * 5), ncol = 5)
#' Y <- sin(X[, 1]) -  X[, 2] + rnorm(10)
#' model <- SDAM(x = X, y = Y, Q_type = "trim", trim_quantile = 0.5, nfold = 2)
#' varImp(model)
#' @export
varImp.SDAM <- function(object){
  vIj <- function(j){
    return(mean(predict_individual_fj(object, j)^2))
  }
  vI <- sapply(1:object$p, vIj)
  names(vI) <- object$var_names
  return(vI)
}
