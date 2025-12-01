#' Plot SDTree
#' 
#' Plot the SDTree.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{SDTree}.
#' @param main title for the tree
#' @param digits integer indicating the number of decimal places to round() the 
#' leaf values to
#' @param digits_decisions integer indicating the number of decimal places to 
#' round() the splitting rule to.
#' @param weighted if true, connections from parent to children is scaled with res_dloss,
#' more important splits result in thicker lines.
#' @param ... Further arguments passed to or from other methods.
#' @return A ggplot object
#' @seealso \code{\link{SDTree}}
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDTree(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.5)
#' plot(model)
#' @export
plot.SDTree <- function(x, main = "", digits = 2, digits_decisions = 2, 
                        weighted = TRUE, ...){
  # prepare labels
  node_labels <- apply(x$tree, 1, split_names, var_names = x$var_names, digits = digits_decisions)
  leaves <- round(x$tree[, "value"], digits)
  samples <- x$tree[, "n_samples"]
  
  node_labels <- paste0(node_labels, "\n", samples, "\n", leaves)
  nodes <- data.frame(name = 1:nrow(x$tree), labels = node_labels)
  nodes <- nodes[x$tree[, "leaf"] != 0, ]
  node_labels <- nodes$labels
  
  #collect edges
  edges_left <- data.frame(from = 1:nrow(x$tree), to = x$tree[, "left"], 
                           res_dloss = x$tree[, "res_dloss"])
  edges_left <- edges_left[x$tree[, "leaf"] == 2, ]
  edges_right <- data.frame(from = 1:nrow(x$tree), to = x$tree[, "right"], 
                            res_dloss = x$tree[, "res_dloss"])
  edges_right <- edges_right[x$tree[, "leaf"] == 2, ]
  edges <- rbind(edges_left, edges_right)
  
  if(weighted){
    #res_dloss <- edges$res_dloss
    #re scale edge weights
    edges$res_dloss <- (edges$res_dloss - min(edges$res_dloss)) / (max(edges$res_dloss) - min(edges$res_dloss)) * 2 + 0.5
  }else{
    edges$res_dloss <- 0.5
  }
  #edges <- edges[, 1:2]
  
  # find location for annotations
  treeGraph <- igraph::graph_from_data_frame(edges, vertices = nodes)
  root <- igraph::V(treeGraph)[igraph::degree(treeGraph, mode="in") == 0]   # node with no parent
  depth <- max(max(igraph::distances(treeGraph, v = root, mode = "out")), 1)
  nLeaves <- max((sum(x$tree[, "leaf"] == 1) - 1), 1) * 0.9
  
  ggraph::ggraph(treeGraph, layout = 'dendrogram', circular = FALSE) + 
    ggraph::geom_edge_diagonal(color = "darkgrey", ggplot2::aes(edge_width = .data$res_dloss))+
    ggraph::geom_node_text(ggplot2::aes(label = .data$labels)) + 
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = 'white', colour = 'white')) +
    ggplot2::annotate("segment", x = nLeaves*0.98, y = depth*0.98, xend = nLeaves*0.9, yend = depth*0.98, 
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches")), color = "black") +
    ggplot2::annotate("text", x = nLeaves * 0.95, y = depth, label = "yes", size = 4) +
    ggplot2::annotate("segment", x = nLeaves*1.02, y = depth*0.98, xend = nLeaves*1.1, yend = depth*0.98, 
                      arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches")), color = "black") +
    ggplot2::annotate("text", x = nLeaves * 1.05, y = depth, label = "no", size = 4) + 
    ggplot2::ggtitle(main)
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
    xi <- matrix(x$X[i, ], nrow = 1)
    
    # predict for each tree
    pred <- rep(NA, length(x$forest))
    model_idx <- x$oob_ind[[i]]
    model_idx <- model_idx[model_idx <= length(x$forest)]
    predictions <- sapply(model_idx, function(model){
      traverse_tree(x$forest[[model]]$tree, xi)
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