#' @export 
toList <- function(object, ...) UseMethod('toList')

#' @export 
fromList <- function(object, ...) UseMethod('fromList')

#' SDTree toList method
#' 
#' Converts the tree in an SDTree object from 
#' class \code{Node} \insertCite{Glur2023Data.tree:Structure}{SDModels} to class \code{list}.
#' This makes it substantially easier to save the tree to disk.
#' @author Markus Ulmer
#' @references
#'  \insertAllCited{}
#' @param object an SDTree object with the tree in Node format
#' @param ... Further arguments passed to or from other methods.
#' @return an SDTree object with the tree in list format
#' @seealso \code{\link{fromList}}
#' @examples
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDTree(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5)
#' toList(model)
#' @export
toList.SDTree <- function(object, ...){
  object$tree <- as.list(object$tree)
  object
}

#' SDTree fromList method
#' 
#' Converts the tree in an SDTree object from
#' class \code{list} to class \code{Node} \insertCite{Glur2023Data.tree:Structure}{SDModels}.
#' @author Markus Ulmer
#' @references
#'  \insertAllCited{}
#' @param object an SDTree object with the tree in list format
#' @param ... Further arguments passed to or from other methods.
#' @return an SDTree object with the tree in Node format
#' @seealso \code{\link{toList}}
#' @examples
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDTree(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5)
#' fromList(toList(model))
#' @export
fromList.SDTree <- function(object, ...){
  object$tree <- data.tree::as.Node(object$tree)
  object
}

#' SDForest toList method
#' 
#' Converts the trees in an SDForest object from
#' class \code{Node} \insertCite{Glur2023Data.tree:Structure}{SDModels} to class \code{list}.
#' This makes it substantially easier to save the forest to disk.
#' @author Markus Ulmer
#' @references
#'  \insertAllCited{}
#' @param object an SDForest object with the trees in Node format
#' @param ... Further arguments passed to or from other methods.
#' @return an SDForest object with the trees in list format
#' @seealso \code{\link{fromList}} \code{\link{toList.SDTree}}
#' @aliases toList
#' @examples
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5, nTree = 2)
#' toList(model)
#' @export
toList.SDForest <- function(object, ...){
  object$forest <- lapply(object$forest, toList)
  object
}

#' SDForest fromList method
#' 
#' Converts the trees in an SDForest object from
#' class \code{list} to class \code{Node} \insertCite{Glur2023Data.tree:Structure}{SDModels}.
#' @author Markus Ulmer
#' @references
#'  \insertAllCited{}
#' @param object an SDForest object with the trees in list format
#' @param ... Further arguments passed to or from other methods.
#' @return an SDForest object with the trees in Node format
#' @seealso \code{\link{fromList}} \code{\link{fromList.SDTree}}
#' @aliases fromList
#' @examples
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5, nTree = 2)
#' fromList(toList(model))
#' @export
fromList.SDForest <- function(object, ...){
  object$forest <- lapply(object$forest, fromList)
  object
}