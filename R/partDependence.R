#' Partial dependence
#' 
#' This function calculates the partial dependence of a model on a single variable.
#' For that predictions are made for all observations in the dataset while varying 
#' the value of the variable of interest. The overall partial effect is the average
#' of all predictions. \insertCite{Friedman2001GreedyMachine}{SDModels}
#' @importFrom Rdpack reprompt
#' @references
#'   \insertAllCited{}
#' @author Markus Ulmer
#' @param object A model object that has a predict method that takes newdata as argument 
#' and returns predictions.
#' @param j The variable for which the partial dependence should be calculated.
#' Either the column index of the variable in the dataset or the name of the variable.
#' @param X The dataset on which the partial dependence should be calculated.
#' Should contain the same variables as the dataset used to train the model.
#' If NULL, tries to extract the dataset from the model object.
#' @param subSample Number of samples to draw from the original data for the empirical 
#' partial dependence. If NULL, all the observations are used.
#' @param verbose If \code{TRUE} progress updates are shown using the `progressr` package. 
#' To customize the progress bar, see [`progressr` package](https://progressr.futureverse.org/)
#' @param mc.cores Number of cores to use for parallel computation `vignette("Runtime")`. 
#' The `future` package is used for parallel processing. 
#' To use custom processing plans mc.cores has to be <= 1, see [`future` package](https://future.futureverse.org/).
#' @return An object of class \code{partDependence} containing
#' \item{preds_mean}{The average prediction for each value of the variable of interest.}
#' \item{x_seq}{The sequence of values for the variable of interest.}
#' \item{preds}{The predictions for each value of the variable of interest for each observation.}
#' \item{j}{The name of the variable of interest.}
#' \item{xj}{The values of the variable of interest in the dataset.}
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' y <- sign(x) * 3 + rnorm(100)
#' model <- SDTree(x = x, y = y, Q_type = 'no_deconfounding')
#' pd <- partDependence(model, 1, X = x, subSample = 10)
#' plot(pd)
#' @seealso \code{\link{SDForest}}, \code{\link{SDTree}}
#' @export
partDependence <- function(object, j, X = NULL, subSample = NULL, 
                           verbose = TRUE, mc.cores = 1){
  j_name <- j

  if(is.null(X)){
    X <- object$X
    if(is.null(X)) stop('X must be provided if it is not part of the object')
    
  }
  X <- data.frame(X)

  if(is.character(j)){
    j <- which(names(X) == j)
  }
  
  if(!is.null(subSample)) X <- X[sample(1:nrow(X), subSample), ]
  X <- data.frame(X)
  
  if(!is.numeric(j)) stop('j must be a numeric or character')
  if(j > ncol(X)) stop('j must be smaller than p')
  if(j < 1) stop('j must be larger than 0')
  if(any(is.na(X))) stop('X must not contain missing values')
  
  x_seq <- seq(min(X[, j]), max(X[, j]), length.out = 100)
  
  progressr::with_progress({
    p <- progressr::progressor(along = x_seq, enable = verbose)
    if(mc.cores > 1){
      plan <- if (parallelly::supportsMulticore()) "multicore" else "multisession"
      with(future::plan(plan, workers = mc.cores), local = TRUE)
    }
    preds <- future.apply::future_lapply(future.seed = TRUE, 
                                         X = x_seq, 
            function(x){
              X_new <- X
              X_new[, j] <- x
              pred <- predict(object, newdata = X_new)
              p(sprintf("x=%g", x))
              return(pred)
            })
  })
  preds <- do.call(rbind, preds)
  preds_mean <- rowMeans(preds)
  
  res <- list(preds_mean = preds_mean, x_seq = x_seq, preds = preds, j = j_name, xj = X[, j])
  class(res) <- 'partDependence'
  
  res
}

#' Print partDependence
#' 
#' Print contents of the partDependence.
#' @author Markus Ulmer
#' @param x Fitted object of class \code{partDependence}.
#' @param ... Further arguments passed to or from other methods.
#' @return No return value, called for side effects
#' @seealso \code{\link{partDependence}}, \code{\link{plot.partDependence}}
#' @method print partDependence
#' @examples
#' set.seed(1)
#' x <- rnorm(10)
#' y <- sign(x) * 3 + rnorm(10)
#' model <- SDTree(x = x, y = y, Q_type = 'no_deconfounding', cp = 0.5)
#' pd <- partDependence(model, 1, X = x)
#' print(pd)
#' @export
print.partDependence <- function(x, ...){
  cat("Partial dependence of covariate: ", x$j, "\n")
  cat("Plot to analyze!")
}

#' Plot partial dependence
#' 
#' This function plots the partial dependence of a model on a single variable.
#' @author Markus Ulmer
#' @param x An object of class \code{partDependence} returned by \code{\link{partDependence}}.
#' @param n_examples Number of examples to plot in addition to the average prediction.
#' @param ... Further arguments passed to or from other methods.
#' @return A ggplot object.
#' @seealso \code{\link{partDependence}}
#' set.seed(1)
#' x <- rnorm(10)
#' y <- sign(x) * 3 + rnorm(10)
#' model <- SDTree(x = x, y = y, Q_type = 'no_deconfounding', cp = 0.5)
#' pd <- partDependence(model, 1, X = x)
#' plot(pd)
#' @export
plot.partDependence <- function(x, n_examples = 19, ...){
  ggdep <- ggplot2::ggplot() + ggplot2::theme_bw()
  preds <- x$preds
  x_seq <- x$x_seq
  
  
  sample_examples <- sample(1:ncol(preds), min(n_examples, ncol(preds)))

  for(i in sample_examples){
    pred_data <- data.frame(x = x_seq, y = preds[, i])
    ggdep <- ggdep + ggplot2::geom_line(data = pred_data, 
                                        ggplot2::aes(x = .data$x, y = .data$y), col = 'grey')
  }
  
  ggdep <- ggdep + ggplot2::geom_line(data = data.frame(x = x_seq, y = x$preds_mean), 
                                      ggplot2::aes(x = .data$x, y = .data$y), col = '#08cbba', 
                                      linewidth = 1.5)
  ggdep <- ggdep + ggplot2::geom_rug(data = data.frame(x = x$xj, 
                                                       y = min(preds[, sample_examples])), 
                                     ggplot2::aes(x = .data$x, y = .data$y), 
                                     sides = 'b', col = '#949494')
  ggdep <- ggdep + ggplot2::ylab('f(x)') + ggplot2::ggtitle('Partial dependence')
  if(is.character(x$j)){
    ggdep <- ggdep + ggplot2::xlab(x$j)
  }else{
    ggdep <- ggdep + ggplot2::xlab(paste('x', x$j, sep = ''))
  }
  
  ggdep
}
