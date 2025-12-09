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


# finds all the reasonable splitting points in a data matrix
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

estimate_tree <- function(boot_index, Y, X, A, max_leaves, cp, min_sample, mtry, fast,
                          Q_type, trim_quantile, q_hat, Qf, gamma, max_candidates, 
                          Q_scale, predictors){
  if(is.null(boot_index)){
    boot_index <- 1:nrow(X)
    tree_in_forest <- FALSE
  }else{
    tree_in_forest <- TRUE
  }
  n <- length(boot_index)
  
  # estimate spectral transformation
  if(!is.null(A)){
    if(is.null(gamma)) stop('gamma must be provided if A is provided')
    if(is.vector(A)) A <- matrix(A)
    if(!is.matrix(A)) stop('A must be a matrix')
    if(nrow(A) != nrow(X)) stop('A must have n rows')
    Wf <- get_Wf(matrix(A[boot_index, ], ncol = ncol(A)), gamma)
  }else {
    Wf <- function(v) v
  }

  if(is.null(Qf)){
    if(!is.null(A)){
      Qf <- function(v) get_Qf(Wf(X[boot_index, ]), Q_type, trim_quantile, q_hat, Q_scale)(Wf(v))
    }else{
      Qf <- get_Qf(X[boot_index, ], Q_type, trim_quantile, q_hat, Q_scale)
    }
  }else{
    if(!is.function(Qf)) stop('Q must be a function')
    if(length(Qf(rnorm(n))) == n) stop('Q must map from n to n')
  }
  
  #selection of predictors
  if(!is.null(predictors)){
    if(is.character(predictors)){
      if(!all(predictors %in% colnames(X)))
        stop("predictors must either be numeric columne index or in colnames of X")
      predictors <- which(colnames(X) %in% predictors)
    }
    if(is.numeric(predictors)){
      if(!all(predictors > 0 & predictors <= ncol(X)))
        stop("predictors must either be numeric columne index or in colnames of X")
    }
    pred_names <- colnames(X)
    X <- matrix(X[, predictors], ncol = length(predictors))
    if(!is.null(pred_names)){
      colnames(X) <- pred_names[predictors]
    }
  }
  
  # number of covariates
  p <- ncol(X)
  if(!is.null(mtry) && mtry > p) stop('mtry must be at most p')
  
  # calculate first estimate
  E <- matrix(1, n, 1)
  E_tilde <- Qf(E)
  Ue <- E_tilde / sqrt(sum(E_tilde ** 2))
  Y_tilde <- Qf(Y[boot_index])
  
  # solve linear model
  c_hat <- qr.coef(qr(E_tilde), Y_tilde)
  c_hat <- as.numeric(c_hat)
  
  loss_start <- as.numeric(sum((Y_tilde - c_hat) ** 2) / n)
  loss_temp <- loss_start
  
  # initialize tree
  treeInfo <- c("name", "left", "right", "j", "s", "value", "dloss", 
                "res_dloss", "cp", "n_samples", "leaf")
  d <- length(treeInfo)
  
  tree <- matrix(0, ncol = d, nrow = 1, dimnames = list(NULL, treeInfo))
  tree[1, c("name", "value", "dloss", "cp", "n_samples", "leaf")] <- 
    c(1, c_hat, loss_start, 10, n, 1)
  treeSize <- 1
  
  # memory for optimal splits
  memory <- list()
  potential_splits <- 1
  
  # variable importance
  var_imp <- rep(0, p)
  names(var_imp) <- colnames(X)
  
  after_mtry <- 0
  
  for(i in 1:max_leaves){
    # iterate over all possible splits every time
    # for slow but slightly better solution
    if(!fast){
      potential_splits <- 1:i
      to_small <- sapply(potential_splits, 
                         function(x){sum(E[, x]) < min_sample*2})
      potential_splits <- potential_splits[!to_small]
    }
    
    #iterate over new to estimate splits
    for(branch in potential_splits){
      # get samples in branch to evaluate
      E_branch <- E[, branch]
      index <- which(E_branch == 1)
      X_branch <- matrix(X[boot_index[index], ], nrow = length(index))
      
      # get potential splitting candidates
      s <- find_s(X_branch, max_candidates = max_candidates)
      n_splits <- nrow(s)
      
      # remove splits resulting in to small leaves
      if(min_sample > 1) {
        s <- s[-c(0:(min_sample - 1), (n_splits - min_sample + 2):(n_splits+1)), ]
      }
      s <- matrix(s, ncol = p)
      
      optSplits <- lapply(1:p, function(j){
        s_j <- unique(s[, j])
        E_next <- lapply(s_j, function(si) {
          E_next <- matrix(0, nrow = n, ncol = 1)
          E_next[index[X_branch[, j] > si], ] <- 1
          if(sum(E_next) == 0)return(NULL)
          E_next
        })
        E_next <- do.call(cbind, E_next)
        if(is.null(E_next)) return(c(-10, j, 0, branch))
        U_next_prime <- Qf_temp(E_next, Ue, Qf)
        U_next_size <- colSums(U_next_prime ** 2)
        dloss <- as.numeric(crossprod(U_next_prime, Y_tilde))**2 / U_next_size
        
        opt <- which.max(unlist(dloss))
        c(dloss[[opt]], j, s_j[opt], branch)
      })
      memory[[branch]] <- do.call(rbind, optSplits)
    }
    
    if(i > after_mtry && !is.null(mtry)){
      Losses_dec <- lapply(memory, function(branch){
        branch[sample(1:p, mtry), ]})
      Losses_dec <- do.call(rbind, Losses_dec)
    }else {
      Losses_dec <- do.call(rbind, memory)
    }
    
    loc <- which.max(Losses_dec[, 1])
    best_branch <- Losses_dec[loc, 4]
    j <- Losses_dec[loc, 2]
    s <- Losses_dec[loc, 3]
    
    if(Losses_dec[loc, 1] <= 0){
      break
    }
    
    # divide observations in leaf
    index <- which(E[, best_branch] == 1)
    index_n_branches <- index[X[boot_index[index], j] > s]
    
    # new indicator matrix
    E <- cbind(E, matrix(0, n, 1))
    E[index_n_branches, best_branch] <- 0
    E[index_n_branches, i+1] <- 1
    
    E_tilde_branch <- E_tilde[, best_branch]
    suppressWarnings({
      E_tilde[, best_branch] <- Qf(E[, best_branch])
    })
    E_tilde <- cbind(E_tilde, matrix(E_tilde_branch - E_tilde[, best_branch]))
    
    c_hat <- qr.coef(qr(E_tilde), Y_tilde)
    
    u_next_prime <- Qf_temp(E[, i + 1], Ue, Qf)
    Ue <- cbind(Ue, u_next_prime / sqrt(sum(u_next_prime ** 2)))
    
    # check if loss decrease is larger than minimum loss decrease
    # and if linear model could be estimated
    if(sum(is.na(as.numeric(c_hat))) > 0){
      warning('singulaer matrix QE, tree might be to large, consider increasing cp')
      break
    }
    
    loss_dec <- as.numeric(loss_temp - loss(Y_tilde, E_tilde %*% c_hat))
    loss_temp <- loss_temp - loss_dec
    
    if(loss_dec <= cp * loss_start){
      break
    }
    # add loss decrease to variable importance
    var_imp[j] <- var_imp[j] + loss_dec
    
    # add space for the two new leaves
    tree <- rbind(tree, matrix(0, nrow = 2, ncol = d))
    
    # select leaf to split
    leaves <- tree[, "leaf"] == 1
    toSplit <- leaves & (tree[, "name"] == best_branch)
    if(sum(toSplit) != 1) stop("Tries to split more than one leaf")
    
    # save split rule
    tree[toSplit, c("left", "right", "j", "s", "res_dloss", "leaf")] <- 
      c(treeSize + 1, treeSize + 2, j, s, loss_dec, 2)
    
    # add new leaves
    tree[treeSize + 1, c("name", "dloss", "cp", "n_samples", "leaf")] <- 
      c(tree[toSplit, "name"], loss_dec, loss_dec / loss_start, sum(E[, best_branch] == 1), 1)
    tree[treeSize + 2, c("name", "dloss", "cp", "n_samples", "leaf")] <- 
      c(i + 1, loss_dec, loss_dec / loss_start, sum(E[, i + 1] == 1), 1)
    treeSize <- treeSize + 2
    
    # add estimates to tree leaves
    c_hat <- as.numeric(c_hat)
    # access leaf estimates by leaf names (i.e. columns of E)
    tree[tree[, "leaf"] == 1, "value"] <- c_hat[tree[tree[, "leaf"] == 1, "name"]]
    
    # the two new partitions need to be checked for optimal splits in next iteration
    potential_splits <- c(best_branch, i + 1)
    
    # a partition with less than min_sample observations or unique samples 
    # are not available for further splits
    to_small <- sapply(potential_splits, function(x){
      new_samples <- nrow(unique(matrix(X[boot_index[as.logical(E[, x])],], nrow = sum(E[, x]))))
      if(is.null(new_samples)) new_samples <- 0
      (new_samples < min_sample * 2)
    })
    if(sum(to_small) > 0){
      for(el in potential_splits[to_small]){
        # to small partitions cannot decrease the loss
        memory[[el]] <- matrix(0, p, 4)
      }
      potential_splits <- potential_splits[!to_small]
    }
  }
  
  if(i == max_leaves){
    warning('maximum number of iterations was reached, consider increasing m!')
  }
  
  # predict the test set
  if(tree_in_forest){
    f_X_hat <- NULL
  }else{
    f_X_hat <- traverse_tree(tree, X)
  }
  
  
  var_names <- colnames(data.frame(X))
  names(var_imp) <- var_names
  
  # cp max of all splits after
  new_cp <- getCp_max(tree)
  tree[new_cp[[2]], "cp"] <- new_cp[[1]]
  
  # use max cp over siblings to ensure binary tree
  for(i in 1:nrow(tree)){
    if(tree[i, c("j")] != 0){
      tree[tree[i, c("left", "right")], "cp"] <- 
        max(tree[tree[i, c("left", "right")], "cp"])
    }
  }
  
  res <- list(predictions = f_X_hat, tree = tree, 
              var_names = var_names, var_importance = var_imp)
  class(res) <- 'SDTree'
  res
}
