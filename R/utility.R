#' @importFrom Rdpack reprompt
#' @import DiagrammeR
#' @import future.apply
#' @import future
#' @importFrom stats lm.fit
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats sd
#' @importFrom utils lsf.str
#' @importFrom utils capture.output
#' @importFrom stats rbeta
#' @importFrom rlang .data

data.handler <- function(formula = NULL, data = NULL, x = NULL, y = NULL){
  if(is.null(formula)){
    if(is.null(x) | is.null(y)){
      stop("Error: Either data or x and y is required.")
    }else {
      if(is.vector(x)) x <- as.matrix(x)
      if(!is.matrix(x)){
        stop("Error: x must be a matrix!")
      }
      if(!is.numeric(x)){
        stop("Error: x must contain numerical values! Use formula for categorical predictors.")
      }
      if (!is.numeric(y)) stop("Error: y must be numeric. Only regression is supported at the moment.")
      if(any(is.na(x)) | any(is.na(y))){
        stop("Error: Missing values are not allowed.")
      }
      if(any(is.infinite(x)) | any(is.infinite(y))){
        stop("Error: Infinite values are not allowed.")
      }
      return(list(X = as.matrix(x), Y = as.numeric(y)))
    }
  }else {
    if(is.null(data)){
      stop("Error: data is required.")
    }else {
      Call <- match.call()
      indx <- match(c("formula", "data"), names(Call), nomatch = 0L)
      
      if (indx[1] == 0L) stop("a 'formula' argument is required")
      
      temp <- Call[c(1L, indx)]      # only keep the arguments we wanted
      temp[[1L]] <- quote(stats::model.frame) # change the function called
      m <- eval.parent(temp)
      
      # ordinal covariates to numeric
      ord <- names(m)[sapply(m, is.ordered)]
      m[ord] <- lapply(m[ord], as.integer)
      
      Terms <- attr(m, "terms")
      if(any(attr(Terms, "order") > 1L)) stop("Trees/SDAM cannot handle interaction terms")
      
      Y <- model.response(m)
      X <- model.matrix(attr(m, "terms"), m)[, -1L, drop = FALSE]
      
      if(any(is.infinite(X)) | any(is.infinite(Y))){
        stop("Error: Infinite values are not allowed.")
      }
      if(!is.null(Y) & !is.numeric(Y)){
        stop("Error: Only regression is suported at the moment. Y must be numeric.")
      }
      list(X = X, Y = Y)
    }
  }
}

#helper functions to label nodes for plotting
split_names <- function(node, var_names = NULL, digits = 2){
  if(node["leaf"] == 1) return("")
    
  if(is.null(var_names)){
    paste('X', node["j"], ' <= ', round(node["s"], digits), sep = '')
  }else{
    paste(var_names[node["j"]], ' <= ', round(node["s"], digits), sep = '')
  }
}


# finds all the reasonable spliting points in a data matrix
find_s <- function(X, max_candidates = 100){
  p <- ncol(X)
  if(p == 1){
    X <- matrix(X, ncol = 1)
  }
  n <- nrow(X)
  
  X_sort <- apply(X, 2, sort, method = 'quick')
  
  if(is.null(dim(X_sort))){
    X_sort <- matrix(X_sort, ncol = p)
  }
  
  # find middle points between observed x values
  s <- X_sort[-nrow(X_sort), ] + diff(X_sort)/2
  
  # for runtime reasons limit split candidates
  if(nrow(s) > max_candidates){
    s <- s[unique(floor(seq(1, dim(s)[1], length.out = max_candidates))), ]
  }
  
  if(is.null(dim(s))){
    matrix(s, ncol = p)
  }else{
    s
  }
}

traverse_tree <- function(tree, X, m = 1){
  if (is.null(tree) || nrow(tree) == 0) {
    stop("Tree is empty or not constructed properly.")
  }
  if(m < 1 | m > nrow(tree)){
    stop("m has to be a valid index of the tree")
  }
  
  if(tree[m, "leaf"] == 1) return(rep(tree[m, "value"], nrow(X)))
  
  # choose child
  rightSamples <- X[, tree[m, "j"]] >= tree[m, "s"]
  
  preds <- rep(NA, nrow(X))
  if(sum(rightSamples) > 0)
    preds[rightSamples] <- traverse_tree(tree, matrix(X[rightSamples, ], 
                                                      ncol = ncol(X)), 
                                         m = tree[m, "right"])
  if(sum(!rightSamples) > 0)
    preds[!rightSamples] <- traverse_tree(tree, matrix(X[!rightSamples, ], 
                                                       ncol = ncol(X)), 
                                          m = tree[m, "left"])
  preds
}

getCp_max <-function(tree, m = 1){
  if(tree[m, "leaf"] == 1) return(list(tree[m, "cp"], m))
  
  left_cp <- getCp_max(tree, tree[m, "left"])
  right_cp <- getCp_max(tree, tree[m, "right"])
  
  cp_vec <- c(max(tree[m, "cp"], left_cp[[1]], right_cp[[1]]), left_cp[[1]], right_cp[[1]])
  m_vec <- c(m, left_cp[[2]], right_cp[[2]])
  list(cp_vec, m_vec)
}

loss <- function(Y, f_X){
  as.numeric(sum((Y - f_X)^2) / length(Y))
}

pruned_loss <- function(tree, X_val, Y_val, Q_val, cp){
  # function to prune tree using the minimum loss decrease t
  # and return spectral loss on the validation set
  
  # prune tree
  tree <- prune(tree, cp)
  
  # predict on test set
  f_X_hat_val <- traverse_tree(tree$tree, X_val)
  
  # return spectral loss
  sum((Q_val(Y_val) - Q_val(f_X_hat_val)) ** 2) / length(Y_val)
}

# more efficient transformations
get_Qf <- function(X, type, trim_quantile = 0.5, q_hat = 0, scaling = TRUE){
  if(type == 'no_deconfounding') {
    return(function(v) v)
  }
  X <- scale(X, center = TRUE, scale = scaling)
  
  svd_error <- function(X, q, f = 1, count = 1){
    tryCatch({
      svd(X * f, nv = 0, nu = q)
    }, error = function(e) {
      warning(paste(e, ':X multipied by number close to 1'))
      if(count > 5) stop('svd did not converge')
      return(svd_error(X, q, 1 + 0.0000000000000001 * 10 ^ count, count + 1))})
  }
  
  if(ncol(X) == 1){
    warning('only one covariate, no deconfounding possible')
    return(function(v) v)
  }
  
  modes <- c('trim' = 1, 'pca' = 2, 'no_deconfounding' = 3)
  if(!(type %in% names(modes))) stop(paste("type must be one of:", 
                                           paste(names(modes), collapse = ', ')))
  
  # number of observations
  n <- dim(X)[1]
  
  # needed number of singular values
  q <- q_hat
  if(type == 'trim'){
    q <- floor(quantile(1:min(dim(X)), 1-trim_quantile))
  }
  
  # calculate deconfounding matrix
  sv <- svd_error(X, q)
  Uq <- sv$u[, 1:q]

  switch(modes[type], 
         {#trim
           D_tilde <- sv$d[1:q]
           D_tilde <- D_tilde[q] / D_tilde
           
           #tau <- quantile(sv$d, trim_quantile)
           #D_tilde <- unlist(lapply(sv$d, FUN = function(x)min(x, tau))) / sv$d
           #D_tilde[is.na(D_tilde)] <- 1
           #q <- sum(D_tilde != 1)
           #D_tilde <- D_tilde[1:q]
           },
         {# pca
           if(q_hat <= 0) 
             stop("the assumed confounding dimension must be larger than zero, increase q_hat")
           D_tilde <- rep(0, q_hat)
           #q <- q_hat
           }
         )
  
  
  Qf <- function(v){
    UqV <- crossprod(Uq, v)
    v + Uq %*% (UqV * (D_tilde - 1))
  }
  return(Qf)
}

get_Wf <- function(A, gamma, intercept = FALSE){
  if(intercept) A <- cbind(1, A)
  if(ncol(A) > nrow(A)) stop('A must have full rank!')
  if(gamma < 0) stop('gamma must be non-negative')
  
  Q_prime <- qr.Q(qr(A))
  Wf <- function(v){
    v - (1 - sqrt(gamma)) * Q_prime %*% crossprod(Q_prime, v)
  }
  return(Wf)
}

Qf_temp <- function(v, Ue, Qf){
  Qfv <- Qf(v)
  Qfv - Ue %*% crossprod(Ue, Qfv)
}



# Helper function to construct the B spline basis function.
# Essentially a wrapper for fda::bsplineS(), but extended to enable
# linear extrapolation outside range(breaks).
Bbasis <- function(x, breaks){
  l <- length(breaks)
  Bx <- matrix(0, nrow = length(x), ncol = l + 2)
  slope_left <- -3/(breaks[2] - breaks[1])
  slope_right <- 3/(breaks[l]- breaks[l-1])
  ind_left <- (x < breaks[1])
  ind_right <- (x > breaks[l])
  ind_range <- !(ind_left | ind_right)
  Bx[ind_range, ] <- fda::bsplineS(x[ind_range], breaks = breaks)
  Bx[ind_left, 1] <- 1 + (x[ind_left]-breaks[1]) * slope_left
  Bx[ind_right, l + 2] <- 1 + (x[ind_right]-breaks[l]) * slope_right
  Bx[ind_left, 2] <- - (x[ind_left]-breaks[1]) * slope_left
  Bx[ind_right, l + 1] <- - (x[ind_right]-breaks[l]) * slope_right
  return(Bx)
}


