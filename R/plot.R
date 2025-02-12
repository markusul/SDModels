#' Plot SDTree
#' 
#' Plot the SDTree.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{SDTree}.
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{SDTree}}
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDTree(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5)
#' plot(model)
#' @export
plot.SDTree <- function(x, ...){
  data.tree::SetEdgeStyle(x$tree, label = function(e) {e$decision})
  data.tree::SetNodeStyle(x$tree, label = function(n) {n$label})
  plot(x$tree)
}
