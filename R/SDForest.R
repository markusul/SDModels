#' Spectrally Deconfounded Random Forests
#' 
#' Estimate regression Random Forest using spectral deconfounding.
#' The spectrally deconfounded Random Forest (SDForest) combines SDTrees in the same way, 
#' as in the original Random Forest \insertCite{Breiman2001RandomForests}{SDModels}.
#' The idea is to combine multiple regression trees into an ensemble in order to 
#' decrease variance and get a smooth function. Ensembles work best if the different 
#' models are independent of each other. To decorrelate the regression trees as much 
#' as possible from each other, we have two mechanisms. The first one is bagging 
#' \insertCite{Breiman1996BaggingPredictors}{SDModels}, where we train each regression 
#' tree on an independent bootstrap sample of the observations, e.g., we draw a 
#' random sample of size \eqn{n} with replacement from the observations. 
#' The second mechanic to decrease the correlation is that only a random subset 
#' of the covariates is available for each split. Before each split, 
#' we sample \eqn{\text{mtry} \leq p} from all the covariates and choose the one 
#' that reduces the loss the most only from those.
#' \deqn{\widehat{f(X)} = \frac{1}{N_{tree}} \sum_{t = 1}^{N_{tree}} SDTree_t(X)}
#' @references
#'  \insertAllCited{}
#' @author Markus Ulmer
#' @param formula Object of class \code{formula} or describing the model to fit 
#' of the form \code{y ~ x1 + x2 + ...} where \code{y} is a numeric response and 
#' \code{x1, x2, ...} are vectors of covariates. Interactions are not supported.
#' @param data Training data of class \code{data.frame} containing the variables in the model.
#' @param x Matrix of covariates, alternative to \code{formula} and \code{data}.
#' @param y Vector of responses, alternative to \code{formula} and \code{data}.
#' @param nTree Number of trees to grow.
#' @param cp Complexity parameter, minimum loss decrease to split a node. 
#' A split is only performed if the loss decrease is larger than \code{cp * initial_loss}, 
#' where \code{initial_loss} is the loss of the initial estimate using only a stump.
#' @param min_sample Minimum number of observations per leaf. 
#' A split is only performed if both resulting leaves have at least 
#' \code{min_sample} observations.
#' @param mtry Number of randomly selected covariates to consider for a split, 
#' if \code{NULL} half of the covariates are available for each split. 
#' \eqn{\text{mtry} = \lfloor \frac{p}{2} \rfloor}
#' @param mc.cores Number of cores to use for parallel processing,
#' if \code{mc.cores > 1} the trees are estimated in parallel.
#' @param Q_type Type of deconfounding, one of 'trim', 'pca', 'no_deconfounding'. 
#' 'trim' corresponds to the Trim transform \insertCite{Cevid2020SpectralModels}{SDModels} 
#' as implemented in the Doubly debiased lasso \insertCite{Guo2022DoublyConfounding}{SDModels}, 
#' 'pca' to the PCA transformation\insertCite{Paul2008PreconditioningProblems}{SDModels}. 
#' See \code{\link{get_Q}}.
#' @param trim_quantile Quantile for Trim transform, 
#' only needed for trim, see \code{\link{get_Q}}.
#' @param q_hat Assumed confounding dimension, only needed for pca, 
#' see \code{\link{get_Q}}.
#' @param Qf Spectral transformation, if \code{NULL} 
#' it is internally estimated using \code{\link{get_Q}}.
#' @param A Numerical Anchor of class \code{matrix}. See \code{\link{get_W}}.
#' @param gamma Strength of distributional robustness, \eqn{\gamma \in [0, \infty]}. 
#' See \code{\link{get_W}}.
#' @param max_size Maximum number of observations used for a bootstrap sample.
#' If \code{NULL} n samples with replacement are drawn.
#' @param gpu If \code{TRUE}, the calculations are performed on the GPU. 
#' If it is properly set up.
#' @param return_data If \code{TRUE}, the training data is returned in the output.
#' This is needed for \code{\link{prune.SDForest}}, \code{\link{regPath.SDForest}}, 
#' and for \code{\link{mergeForest}}.
#' @param mem_size Amount of split candidates that can be evaluated at once.
#' This is a trade-off between memory and speed can be decreased if either
#' the memory is not sufficient or the gpu is to small.
#' @param leave_out_ind Indices of observations that should not be used for training.
#' @param envs Vector of environments of class \code{factor} 
#' which can be used for stratified tree fitting.
#' @param nTree_leave_out Number of trees that should be estimated while leaving
#' one of the environments out. Results in number of environments times number of trees.
#' @param nTree_env Number of trees that should be estimated for each environment.
#' Results in number of environments times number of trees.
#' @param max_candidates Maximum number of split points that are 
#' proposed at each node for each covariate.
#' @param Q_scale Should data be scaled to estimate the spectral transformation? 
#' Default is \code{TRUE} to not reduce the signal of high variance covariates, 
#' and we do not know of a scenario where this hurts.
#' @param verbose If \code{TRUE} fitting information is shown.
#' @param predictors Subset of colnames(X) or numerical indices of the covariates 
#' for which an effect on y should be estimated. All the other covariates are only
#' used for deconfounding.
#' @return Object of class \code{SDForest} containing:
#' \item{predictions}{Vector of predictions for each observation.}
#' \item{forest}{List of SDTree objects.}
#' \item{var_names}{Names of the covariates.}
#' \item{oob_loss}{Out-of-bag loss. MSE}
#' \item{oob_SDloss}{Out-of-bag loss using the spectral transformation.}
#' \item{var_importance}{Variable importance.
#' The variable importance is calculated as the sum of the decrease in the loss function 
#' resulting from all splits that use a covariate for each tree. 
#' The mean of the variable importance of all trees results in the variable importance for the forest.}
#' \item{oob_ind}{List of indices of trees that did not contain the observation in the training set.}
#' \item{oob_predictions}{Out-of-bag predictions.}
#' If \code{return_data} is \code{TRUE} the following are also returned:
#' \item{X}{Matrix of covariates.}
#' \item{Y}{Vector of responses.}
#' \item{Q}{Spectral transformation.}
#' If \code{envs} is provided the following are also returned:
#' \item{envs}{Vector of environments.}
#' \item{nTree_env}{Number of trees for each environment.}
#' \item{ooEnv_ind}{List of indices of trees that did not contain the observation or the same environment in the training set
#' for each observation.}
#' \item{ooEnv_loss}{Out-of-bag loss using only trees that did not contain the observation or the same environment.}
#' \item{ooEnv_SDloss}{Out-of-bag loss using the spectral transformation and only trees that did not contain the observation
#' or the same environment.}
#' \item{ooEnv_predictions}{Out-of-bag predictions using only trees that did not contain the observation or the same environment.}
#' \item{nTree_leave_out}{If environments are left out, the environment for each tree, that was left out.}
#' \item{nTree_env}{If environments are provided, the environment each tree is trained with.}
#' @seealso \code{\link{get_Q}}, \code{\link{get_W}}, \code{\link{SDTree}}, 
#' \code{\link{simulate_data_nonlinear}}, \code{\link{regPath}}, 
#' \code{\link{stabilitySelection}}, \code{\link{prune}}, \code{\link{partDependence}}
#' @examples
#' set.seed(1)
#' n <- 50
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', nTree = 5, cp = 0.5)
#' predict(model, newdata = data.frame(X))
#' 
#' ###### subset of predictors
#' # if we know, that only the first covariate has an effect on y,
#' # we can estimate only its effect and use the others just for deconfounding
#' model <- SDForest(x = X, y = y, cp = 0.5, nTree = 5, predictors = c(1))
#' 
#' \donttest{
#' set.seed(42)
#' # simulation of confounded data
#' sim_data <- simulate_data_nonlinear(q = 2, p = 150, n = 100, m = 2)
#' X <- sim_data$X
#' Y <- sim_data$Y
#' train_data <- data.frame(X, Y)
#' # causal parents of y
#' sim_data$j
#' 
#' # comparison to classical random forest
#' fit_ranger <- ranger::ranger(Y ~ ., train_data, importance = 'impurity')
#' 
#' fit <- SDForest(x = X, y = Y, nTree = 100, Q_type = 'pca', q_hat = 2)
#' fit <- SDForest(Y ~ ., nTree = 100, train_data)
#' fit
#' 
#' # we can plot the fit to see whether the number of trees is high enough
#' # if the performance stabilizes, we have enough trees otherwise one can fit
#' # more and add them
#' plot(fit)
#' 
#' # a few more might be helpfull
#' fit2 <- SDForest(Y ~ ., nTree = 50, train_data) 
#' fit <- mergeForest(fit, fit2)
#' 
#' # comparison of variable importance
#' imp_ranger <- fit_ranger$variable.importance
#' imp_sdf <- fit$var_importance
#' imp_col <- rep('black', length(imp_ranger))
#' imp_col[sim_data$j] <- 'red'
#' 
#' plot(imp_ranger, imp_sdf, col = imp_col, pch = 20,
#'      xlab = 'ranger', ylab = 'SDForest', 
#'      main = 'Variable Importance')
#' 
#' # check regularization path of variable importance
#' path <- regPath(fit)
#' # out of bag error for different regularization
#' plotOOB(path)
#' plot(path)
#' 
#' # detection of causal parent using stability selection
#' stablePath <- stabilitySelection(fit)
#' plot(stablePath)
#' 
#' # pruning of forest according to optimal out-of-bag performance
#' fit <- prune(fit, cp = path$cp_min)
#' 
#' # partial functional dependence of y on the most important covariate
#' most_imp <- which.max(fit$var_importance)
#' dep <- partDependence(fit, most_imp)
#' plot(dep, n_examples = 100)
#' }
#' @export
SDForest <- function(formula = NULL, data = NULL, x = NULL, y = NULL, nTree = 100, 
                     cp = 0, min_sample = 5, mtry = NULL, mc.cores = 1, 
                     Q_type = 'trim', trim_quantile = 0.5, q_hat = 0, Qf = NULL, 
                     A = NULL, gamma = 7, max_size = NULL, gpu = FALSE, 
                     return_data = TRUE, mem_size = 1e+7, leave_out_ind = NULL, 
                     envs = NULL, nTree_leave_out = NULL, nTree_env = NULL, 
                     max_candidates = 100, Q_scale = TRUE, verbose = TRUE, 
                     predictors = NULL){
  if(gpu) ifelse(GPUmatrix::installTorch(), 
                 gpu_type <- 'torch', 
                 gpu_type <- 'tensorflow')
  input_data <- data.handler(formula = formula, data = data, x = x, y = y)
  X <- input_data$X
  Y <- input_data$Y

  # number of observations
  n <- nrow(X)
  # number of covariates
  p <- ifelse(is.null(predictors), ncol(X), length(predictors))

  if(is.null(max_size)) max_size <- n

  if(n != length(Y)) stop('X and Y must have the same number of observations')
  if(!is.null(mtry) && mtry < 1) stop('mtry must be larger than 0')
  if(!is.null(mtry) && mtry > p) stop('mtry must be at most p')
  if(gpu && (mc.cores > 1)) 
    warning('gpu and multicore cannot be used together, 
            no gpu is not used for tree estimations')

  if(!is.null(leave_out_ind) && any(leave_out_ind >= n, leave_out_ind < 1)) 
    stop('leave_out_ind must be smaller than n')
  if(!is.null(envs) && any(length(envs) != n, !is.factor(envs)))
    stop('envs must be a factor of length n')
  if(!is.null(envs) && !is.null(nTree_leave_out) && !is.null(nTree_env))
    stop('nTree_leave_out and nTree_env cannot be used together')
  if(!is.null(envs) && !(all(!is.null(nTree_leave_out), sum(nTree_leave_out) > 0) | 
                         all(!is.null(nTree_env), sum(nTree_env) > 0)))
    stop('either nTree_leave_out or nTree_env must be provided larger than 0')

  if(!is.null(A)){
    if(is.null(gamma)) stop('gamma must be provided if A is provided')
    if(is.vector(A)) A <- matrix(A)
    if(!is.matrix(A)) stop('A must be a matrix')
    if(nrow(A) != n) stop('A must have n rows')
    Wf <- get_Wf(A, gamma, gpu)
  }else {
    Wf <- function(v) v
  }

  if(is.null(Qf)){
    if(!is.null(A)){
      Qf <- function(v) get_Qf(Wf(X), Q_type, trim_quantile, q_hat, gpu, Q_scale)(Wf(v))
    }else{
      Qf <- get_Qf(X, Q_type, trim_quantile, q_hat, gpu, Q_scale)
    }
  }else{
    if(!is.function(Qf)) stop('Q must be a function')
    if(length(Qf(rnorm(n))) == n) stop('Q must map from n to n')
  }

  # mtry
  if(is.null(mtry)){
    mtry <- floor(0.5 * p)
    if(mtry < 1) mtry <- 1
  }

  # bootstrap samples
  all_ind <- 1:n
  if(!is.null(leave_out_ind)){
    all_ind <- all_ind[-leave_out_ind]
    if(!is.null(envs)){
      envs <- envs[-leave_out_ind]
    }
  }
  
  if(is.null(envs)){
    ind <- lapply(1:nTree, function(x)
      sample(all_ind, min(length(all_ind), max_size), replace = TRUE))
  }else{# stratified bootstrapping
    nEnv <- length(levels(envs))
    if(nEnv == 1) stop('only one environment is provided')
    nTree_env <- if(!is.null(nTree_leave_out)) 
      nTree_leave_out else nTree_env
    if(length(nTree_env) > 1){
      if(any(length(nTree_env) != nEnv, 
             !setequal(names(nTree_env), levels(envs)))){
        stop('if nTree_env is a vector, it must have the same length as 
             levels(envs) and the names must be the levels of envs')
      }
      nTree_env <- nTree_env[nTree_env != 0]
    }else {
      nTree_env <- rep(nTree_env, nEnv)
      names(nTree_env) <- levels(envs)
    }
    tree_env <- rep(names(nTree_env), nTree_env)

    nTree <- sum(nTree_env)
    if(verbose) print(paste0("Fitting stratified trees resulting in ", nTree, " trees."))

    ind <- lapply(names(nTree_env), function(env_l) {
      lapply(1:nTree_env[env_l], function(x) {
      if(!is.null(nTree_leave_out)){
        # not use leave out environment for training
        all_ind_env <- all_ind[envs != env_l]
      }else{
        # use only specific environment for training
        all_ind_env <- all_ind[envs == env_l]
      }
      sample(all_ind_env, min(length(all_ind_env), max_size), replace = TRUE)
      })})
      ind <- do.call(c, ind)
  }
  
  if(mc.cores > 1){
    if(locatexec::is_unix()){
      if(verbose) print('mclapply')
      res <- parallel::mclapply(ind, function(i) {
        Xi <- matrix(X[i, ], ncol = ncol(X))
        colnames(Xi) <- colnames(X)
        SDTree(x = Xi, y = Y[i], cp = cp, min_sample = min_sample, 
               Q_type = Q_type, trim_quantile = trim_quantile, q_hat = q_hat, 
               mtry = mtry, A = A[i, ], gamma = gamma, mem_size = mem_size, 
               max_candidates = max_candidates, Q_scale = Q_scale, 
               predictors = predictors)
        }, 
        mc.cores = mc.cores)
    }else{
      if(verbose) print('makeCluster')
      cl <- parallel::makeCluster(mc.cores)
      doParallel::registerDoParallel(cl)
      parallel::clusterExport(cl = cl, 
                              unclass(lsf.str(envir = asNamespace("SDModels"), 
                                              all = TRUE)),
                              envir = as.environment(asNamespace("SDModels")))
      res <- parallel::clusterApplyLB(cl = cl, ind, fun = function(i){
        Xi <- matrix(X[i, ], ncol = ncol(X))
        colnames(Xi) <- colnames(X)
        SDTree(x = Xi, y = Y[i], cp = cp, min_sample = min_sample, 
               Q_type = Q_type, trim_quantile = trim_quantile, q_hat = q_hat, 
               mtry = mtry, A = A[i, ], gamma = gamma, mem_size = mem_size, 
               max_candidates = max_candidates, Q_scale = Q_scale, 
               predictors = predictors)
        })
      parallel::stopCluster(cl = cl)
    }
  }else{
    res <- pbapply::pblapply(ind, function(i){
      Xi <- matrix(X[i, ], ncol = ncol(X))
      colnames(Xi) <- colnames(X)
      SDTree(x = Xi, y = Y[i], cp = cp, min_sample = min_sample, 
             Q_type = Q_type, trim_quantile = trim_quantile, q_hat = q_hat, 
             mtry = mtry, A = A[i, ], gamma = gamma, gpu = gpu, 
             mem_size = mem_size, max_candidates = max_candidates, 
             Q_scale = Q_scale, predictors = predictors)
    })
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

  # ensemble predictions for each observation
  # but only with the trees that did not contain the observation in the training set
  oob_ind <- lapply(1:n, function(i) which(unlist(lapply(lapply(ind, 
                         function(train)c(1:n)[-train]), 
                         function(x) any(x == i)))))

  oob_predictions <- sapply(1:n, function(i){
    if(length(oob_ind[[i]]) == 0){
      return(NA)
    }
    xi <- X[i, ]
    predictions <- sapply(oob_ind[[i]], function(model){
      predict_outsample(res[[model]]$tree, xi)
    })
    return(mean(predictions))
  })

  Y_tilde <- Qf(Y)
  oob_SDloss <- loss(Y_tilde, Qf(oob_predictions))
  oob_loss <- loss(Y, oob_predictions)


  if(!is.null(envs)){
    # oob predictions only with model not trained on the same environment
    if(!is.null(nTree_leave_out)){
      ooEnv_ind <- lapply(1:n, function(i){
        idx <- oob_ind[[i]]
        idx[tree_env[idx] == envs[i]]
      })
    }else{
      ooEnv_ind <- lapply(1:n, function(i){
        idx <- oob_ind[[i]]
        idx[tree_env[idx] != envs[i]]
      })
    }

    ooEnv_predictions <- sapply(1:n, function(i){
      if(length(ooEnv_ind[[i]]) == 0){
        return(NA)
      }
      xi <- X[i, ]
      predictions <- sapply(ooEnv_ind[[i]], function(model){
        predict_outsample(res[[model]]$tree, xi)
      })
      return(mean(predictions))
    })

    ooEnv_SDloss <- loss(Y_tilde, Qf(ooEnv_predictions))
    ooEnv_loss <- loss(Y, ooEnv_predictions)
  }

  # predict with all trees
  pred <- do.call(cbind, lapply(res, function(x){predict_outsample(x$tree, X)}))
  
  # use mean over trees as final prediction
  f_X_hat <- rowMeans(pred)
  
  # variable importance
  var_imp <- do.call(cbind, lapply(res, function(x){x$var_importance}))
  var_imp <- rowMeans(var_imp)

  output <- list(predictions = f_X_hat, 
                 forest = res, 
                 var_names = colnames(data.frame(X)), 
                 oob_loss = oob_loss, 
                 oob_SDloss = oob_SDloss, 
                 var_importance = var_imp, 
                 oob_ind = oob_ind, 
                 oob_predictions = oob_predictions)
  
  if(return_data){
    output$X <- as.matrix(X)
    output$Y <- as.matrix(Y)
    output$Q <- Qf
  }

  if(!is.null(envs)){
    if(!is.null(nTree_leave_out)){
      output$nTree_leave_out <- tree_env
    }else{
      output$nTree_env <- tree_env
    }
    output$tree_env <- nTree_env
    output$envs <- envs
    output$ooEnv_ind <- ooEnv_ind
    output$ooEnv_loss <- ooEnv_loss
    output$ooEnv_SDloss <- ooEnv_SDloss
    output$ooEnv_predictions <- ooEnv_predictions
  }

  class(output) <- 'SDForest'
  output
}