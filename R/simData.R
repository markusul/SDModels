#' Simulate data with linear confounding and non-linear causal effect
#' 
#' Simulation of data from a confounded non-linear model. 
#' The data generating process is given by:
#' \deqn{Y = f(X) + \delta^T H  + \nu}
#' \deqn{X = \Gamma^T H + E}
#' where \eqn{f(X)} is a random function on the fourier basis
#' with a subset of size m covariates \eqn{X_j} having a causal effect on \eqn{Y}.
#' \deqn{f(x_i) = \sum_{j = 1}^p 1_{j \in js} \sum_{k = 1}^K (\beta_{j, k}^{(1)} \cos(0.2 k x_j) + 
#' \beta_{j, k}^{(2)} \sin(0.2 k x_j))}
#' \eqn{E}, \eqn{\nu} are random error terms and 
#' \eqn{H \in \mathbb{R}^{n \times q}} is a matrix of random confounding covariates.
#' \eqn{\Gamma \in \mathbb{R}^{q \times p}} and \eqn{\delta \in \mathbb{R}^{q}} are random coefficient vectors.
#' For the simulation, all the above parameters are drawn from a standard normal distribution, except for 
#' \eqn{\nu} which is drawn from a normal distribution with standard deviation 0.1.
#' The parameters \eqn{\beta} are drawn from a uniform distribution between -1 and 1.
#' @author Markus Ulmer
#' @param q number of confounding covariates in H
#' @param p number of covariates in X
#' @param n number of observations
#' @param m number of covariates with a causal effect on Y
#' @param K number of fourier basis functions K \eqn{K \in \mathbb{N}}, e.g. complexity of causal function
#' @param eff the number of affected covariates in X by the confounding, if NULL all covariates are affected
#' @param fixEff if eff is smaller than p: If fixEff = TRUE, the causal parents 
#' are always affected by confounding if fixEff = FALSE, affected covariates are chosen completely at random.
#' @return a list containing the simulated data:
#' \item{X}{a matrix of covariates}
#' \item{Y}{a vector of responses}
#' \item{f_X}{a vector of the true function f(X)}
#' \item{j}{the indices of the causal covariates in X}
#' \item{beta}{the parameter vector for the function f(X), see \code{\link{f_four}}}
#' \item{H}{the matrix of confounding covariates}
#' @seealso \code{\link{f_four}}
#' @examples
#' set.seed(42)
#' # simulation of confounded data
#' sim_data <- simulate_data_nonlinear(q = 2, p = 150, n = 100, m = 2)
#' X <- sim_data$X
#' Y <- sim_data$Y
#' 
#' @export 
simulate_data_nonlinear <- function(q, p, n, m, K = 2, eff = NULL, fixEff = FALSE){
  # complexity of f_X (number of fourier basis functions) K
  complexity <- K
  # random parameter for fourier basis
  beta <- runif(m * complexity * 2, -1, 1)
  
  # random confounding covariates H
  H <- matrix(rnorm(n * q, 0, 1), nrow = n)

  # random correlation matrix cov(X, H)
  Gamma <- matrix(rnorm(q * p, 0, 1), nrow = q)

  # random sparse subset of covariates in X
  js <- sample(1:p, m)
  
  if(!is.null(eff)){
    if(eff > p | eff < 0) stop('eff must be smaller than p or NULL')
    
    non_effected <- p - eff
    
    if(fixEff){
      if(eff < m) stop('Cannot fix confounding on causal parents if eff < m!')
      Gamma[, sample(c(1:p)[-js], non_effected)] <- 0
    }else{
      Gamma[, sample(1:p, non_effected)] <- 0
    }
  }

  # random coefficient vector delta
  delta <- rnorm(q, 0, 1)

  # random error term
  E <- matrix(rnorm(n * p, 0, 1), nrow = n)

  if(q == 0){
    X <- E
  }else{
    X <- H %*% Gamma + E
  }

  # generate f_X
  f_X <- apply(X, 1, function(x) f_four(x, beta, js))
    
  # generate Y
  Y <- f_X + H %*% delta + rnorm(n, 0, 0.1)
  
  #return data
  list(X = X, Y = Y, f_X = f_X, j = js, beta = beta, H = H)
}

#' Function of x on a fourier basis
#' 
#' Function of x on a fourier basis with a subset of covariates 
#' having a causal effect on Y using the parameters beta.
#' The function is given by:
#' \deqn{f(x_i) = \sum_{j = 1}^p 1_{j \in js} \sum_{k = 1}^K (\beta_{j, k}^{(1)} \cos(0.2 k x_j) +
#' \beta_{j, k}^{(2)} \sin(0.2 k x_j))}
#' @author Markus Ulmer
#' @param x a vector of covariates
#' @param beta the parameter vector for the function f(X)
#' @param js the indices of the causal covariates in X
#' @return the value of the function f(x)
#' @seealso \code{\link{simulate_data_nonlinear}}
#' @examples
#' set.seed(42)
#' # simulation of confounded data
#' sim_data <- simulate_data_nonlinear(q = 2, p = 150, n = 100, m = 2)
#' X <- sim_data$X
#' j <- sim_data$j[1]
#' apply(X, 1, function(x) f_four(x, sim_data$beta, j))
#' 
#' @export
f_four <- function(x, beta, js){
  # function to generate f_X
  # x: covariates
  # beta: parameter vector
  # js: relevant covariates

  # number of relevant covariates
  m <- length(js)

  # complexity of f_X
  complexity <- length(beta) / (2 * m)

  # calculate f_X
  do.call(sum, lapply(1:m, function(i) {
    j <- js[i]
    
    # select beta for covariate j
    beta_ind <- 1:(2*complexity) + (i-1) * 2 * complexity
    
    # calculate f_X_j
    do.call(sum, lapply(1:complexity, function(k) 
      beta[beta_ind[1 + (k-1) *2]] * sin(k * 0.2 * x[j]) + 
        beta[beta_ind[2 + (k-1) *2]] * cos(k * 0.2 * x[j])))
  }))
}


#' Simulate data with linear confounding and causal effect following a step-function
#' 
#' Simulation of data from a confounded non-linear model. Where the non-linear function is a random regression tree.
#' The data generating process is given by:
#' \deqn{Y = f(X) + \delta^T H + \nu}
#' \deqn{X = \Gamma^T H + E}
#' where \eqn{f(X)} is a random regression tree with \eqn{m} random splits of the data. 
#' Resulting in a random step-function with \eqn{m+1} levels, i.e. leaf-levels.
#' \deqn{f(x_i) = \sum_{k = 1}^K 1_{\{x_i \in R_k\}} c_k}
#' \eqn{E}, \eqn{\nu} are random error terms and 
#' \eqn{H \in \mathbb{R}^{n \times q}} is a matrix of random confounding covariates.
#' \eqn{\Gamma \in \mathbb{R}^{q \times p}} and \eqn{\delta \in \mathbb{R}^{q}} are random coefficient vectors.
#' For the simulation, all the above parameters are drawn from a standard normal distribution, except for 
#' \eqn{\delta} which is drawn from a normal distribution with standard deviation 10.
#' For a split a covariate is sampled uniformly and split at a random point using a 
#' beta distribution (with both shape parameters equal 2) on the support of the chosen covariate.
#' The leaf levels \eqn{c_k} are drawn from a uniform distribution between \eqn{cl} and \eqn{cu}.
#' @references
#'  \insertAllCited{}
#' @author Markus Ulmer
#' @param q number of confounding covariates in H
#' @param p number of covariates in X
#' @param n number of observations
#' @param m number of splits done using a random covariate
#' @param make_tree Whether the random regression tree should be returned.
#' @param cl lower limit of the uniform distribution of the step levels
#' @param cu upper limit of the uniform distribution of the step levels
#' @return a list containing the simulated data:
#' \item{X}{a \code{matrix} of covariates}
#' \item{Y}{a \code{vector} of responses}
#' \item{f_X}{a \code{vector} of the true function f(X)}
#' \item{j}{the indices of the causal covariates in X}
#' \item{tree}{If \code{make_tree}, the random regression tree of class 
#' \code{SDTree}}
#' @seealso \code{\link{simulate_data_nonlinear}}
#' @examples
#' set.seed(42)
#' # simulation of confounded data
#' sim_data <- simulate_data_step(q = 2, p = 15, n = 100, m = 2, make_tree = TRUE)
#' X <- sim_data$X
#' Y <- sim_data$Y
#' 
#' all(predict(sim_data$tree, data.frame(X)) == sim_data$f_X)
#' plot(regPath(sim_data$tree))
#' @export 
simulate_data_step <- function(q, p, n, m, make_tree = FALSE, cl = -50, cu = 50){
  # minimum number of observations for split
  min_sample <- 2
  
  # random confounding covariates H
  H <- matrix(rnorm(n * q), nrow = n)
  
  # random correlation matrix cov(X, H)
  Gamma <- matrix(rnorm(q * p), nrow = q)
  
  # random coefficient vector delta
  delta <- rnorm(q, 0, 10)
  
  # relevant covariates
  js <- c()
  
  # generate X
  if(q == 0){
    # no confounding covariates
    X <- matrix(rnorm(n * p), nrow = n)
  }else{
    # confounding covariates
    X <- H %*% Gamma + matrix(rnorm(n * p), nrow = n)
  }
  
  # generate tree
  if(make_tree){
    # initialize tree
    treeInfo <- c("name", "left", "right", "j", "s", "value", "dloss", 
                  "res_dloss", "cp", "n_samples", "leaf")
    d <- length(treeInfo)
    
    tree <- matrix(0, ncol = d, nrow = 1, dimnames = list(NULL, treeInfo))
    tree[1, c("name", "value", "dloss", "cp", "n_samples", "leaf")] <- 
      c(1, runif(1, cl, cu), 10, 10, n, 1)
    treeSize <- 1
    
    var_names <- paste0("X", 1:p)
    var_imp <- rep(0, p)
    names(var_imp) <- var_names
  }
  
  # partitions of observations
  index <- list(1:n)
  
  if(m > 0){
  for (i in 1:m){
    # get number of observations in each partition
    samples_per_part <- unlist(lapply(index, function(x)length(x)))
    
    # get potential splits (partitions with enough observations)
    potential_splitts <- which(samples_per_part >= min_sample)
    
    # sample potential partition to split
    # probability of each partition is proportional to number of observations
    if(length(potential_splitts) == 1){
      branch <- potential_splitts
    }else {
      branch <- sample(potential_splitts, 1,
                       prob = samples_per_part[potential_splitts] / 
                         sum(samples_per_part[potential_splitts]))
    }
    
    # sample covariate to split on
    j <- sample(1:p, 1)
    js <- c(js, j)
    
    # sample split point
    potential_s <- X[index[[branch]], j]
    s <- rbeta(1, 2, 2) * (max(potential_s) - min(potential_s)) + min(potential_s)
    
    # split partition
    index <- append(index, list(index[[branch]][X[index[[branch]], j] > s]))
    index[[branch]] <- index[[branch]][X[index[[branch]], j] <= s]
    
    # add split to tree
    if(make_tree){
      # add space for the two new leaves
      tree <- rbind(tree, matrix(0, nrow = 2, ncol = d))
      
      # select leaf to split
      leaves <- tree[, "leaf"] == 1
      toSplit <- leaves & (tree[, "name"] == branch)
      if(sum(toSplit) != 1) stop("Tries to split more than one leaf")
      
      # save split rule
      tree[toSplit, c("left", "right", "j", "s", "res_dloss", "leaf")] <- 
        c(treeSize + 1, treeSize + 2, j, s, 1/i, 2)
      var_imp[j] <- var_imp[j] + 1/i
      
      # add new leaves
      tree[treeSize + 1, c("name", "value", "dloss", "cp", "n_samples", "leaf")] <- 
        c(tree[toSplit, "name"], runif(1, cl, cu), 
          1/i, 1/i, length(index[[branch]]), 1)
      tree[treeSize + 2, c("name", "value", "dloss", "cp", "n_samples", "leaf")] <- 
        c(i + 1, runif(1, cl, cu), 1/i, 1/i, length(index[[length(index)]]), 1)
      treeSize <- treeSize + 2
    }
  }
  }
  
  # sample means per partition
  f_X_means <- runif(length(index), cl, cu)
  
  # generate f_X
  f_X <- rep(0, n)
  for (i in 1:length(index)){
    f_X[index[[i]]] <- f_X_means[i]
  }
  
  # generate Y
  if(q == 0){
    # no confounding covariates
    Y <- f_X + rnorm(n)
  }else{
    # confounding covariates
    Y <- f_X + H %*% delta + rnorm(n)
  }
  
  # return data
  if(make_tree){
    # add leave values to tree
    tree[tree[, "leaf"] == 1, "value"] <- f_X_means[tree[tree[, "leaf"] == 1, "name"]]
    res <- list(predictions = f_X, tree = tree, 
                var_names = var_names, var_importance = var_imp)
    class(res) <- 'SDTree'
    return(list(X = X, Y = Y, f_X = f_X, j = js, tree = res))
  }
  
  # return data
  return(list(X = X, Y = Y, f_X = f_X, j = js))
}