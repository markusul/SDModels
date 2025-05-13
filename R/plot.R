#' Plot SDTree
#' 
#' Plot the SDTree.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{SDTree}.
#' @param ... Further arguments for DiagrammeR::render_graph()
#' @return graph object from DiagrammeR::render_graph()
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
  plot(x$tree, ...)
}

#' Plot performance of SDForest against number of trees
#' 
#' This plot helps to analyze whether enough trees were used. If the loss does
#' not stabilize one can fit another SDForest and merge the two.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{SDForest}.
#' @param ... Further arguments passed to or from other methods.
#' @return A ggplot object
#' @seealso \code{\link{SDForest}}
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5, nTree = 500)
#' plot(model)
#' @export
plot.SDForest <- function(x, ...){
  Y_ <- x$Q(x$Y)
  
  # iterate over observations
  preds <- pbapply::pblapply(1:length(x$Y), function(i){
    if(length(x$oob_ind[[i]]) == 0){
      return(NA)
    }
    xi <- x$X[i, ]
    
    # predict for each tree
    pred <- rep(NA, length(x$forest))
    model_idx <- x$oob_ind[[i]]
    model_idx <- model_idx[model_idx <= length(x$forest)]
    predictions <- sapply(model_idx, function(model){
      predict_outsample(x$forest[[model]]$tree, xi)
    })
    pred[model_idx] <- predictions
    pred
  })
  
  preds <- do.call(rbind, preds)
  
  # aggregate tree predictions
  preds <- apply(preds, 1, function(pred){
    predsums <- pred
    predsums[is.na(predsums)] <- 0
    predsums <- cumsum(predsums)
    predsums / cumsum(!is.na(pred))
  })
  
  preds <- t(preds)
  
  # calculate loss
  oob_loss <- apply(preds, 2, function(pred) mean((pred - x$Y)**2))
  oob_SDloss <- apply(preds, 2, function(pred) mean((x$Q(pred) - Y_)**2))
  
  loss_data <- data.frame(oob.SDE = oob_SDloss, oob.MSE = oob_loss, nTree = 1:length(oob_loss))
  loss_data <- loss_data[!is.na(oob_loss), ]
  
  gg_sde <- ggplot2::ggplot(loss_data, ggplot2::aes(x = .data$nTree, y = .data$oob.SDE)) +
    ggplot2::geom_line() + 
    ggplot2::theme_bw()
  
  gg_mse <- ggplot2::ggplot(loss_data, ggplot2::aes(x = .data$nTree, y = .data$oob.MSE)) +
    ggplot2::geom_line() + 
    ggplot2::theme_bw()
  
  gridExtra::grid.arrange(gg_sde, gg_mse, ncol = 2)
}