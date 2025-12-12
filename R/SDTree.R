#' Spectrally Deconfounded Tree
#' 
#' Estimates a regression tree using spectral deconfounding. 
#' A regression tree is part of the function class of step functions
#' \eqn{f(X) = \sum_{m = 1}^M 1_{\{X \in R_m\}} c_m}, where (\eqn{R_m}) with 
#' \eqn{m = 1, \ldots, M} are regions dividing the space of \eqn{\mathbb{R}^p} 
#' into \eqn{M} rectangular parts. Each region has response level \eqn{c_m \in \mathbb{R}}.
#' For the training data, we can write the step function as \eqn{f(\mathbf{X}) = \mathcal{P} c} 
#' where \eqn{\mathcal{P} \in \{0, 1\}^{n \times M}} is an indicator matrix encoding 
#' to which region an observation belongs and \eqn{c \in \mathbb{R}^M} is a vector 
#' containing the levels corresponding to the different regions. This function then minimizes
#' \deqn{(\hat{\mathcal{P}}, \hat{c}) = \text{argmin}_{\mathcal{P}' \in \{0, 1\}^{n \times M}, c' \in \mathbb{R}^ {M}} \frac{||Q(\mathbf{Y} - \mathcal{P'} c')||_2^2}{n}}
#' We find \eqn{\hat{\mathcal{P}}} by using the tree structure and repeated splitting of the leaves, 
#' similar to the original cart algorithm \insertCite{Breiman2017ClassificationTrees}{SDModels}.
#' Since comparing all possibilities for \eqn{\mathcal{P}} is impossible, we let a tree grow greedily. 
#' Given the current tree, we iterate over all leaves and all possible splits. 
#' We choose the one that reduces the spectral loss the most and estimate after each split 
#' all the leaf estimates
#' \eqn{\hat{c} = \text{argmin}_{c' \in \mathbb{R}^M} \frac{||Q\mathbf{Y} - Q\mathcal{P} c'||_2^2}{n}} 
#' which is just a linear regression problem. This is repeated until the loss decreases 
#' less than a minimum loss decrease after a split. 
#' The minimum loss decrease equals a cost-complexity parameter \eqn{cp} times 
#' the initial loss when only an overall mean is estimated. 
#' The cost-complexity parameter \eqn{cp} controls the complexity of a regression tree 
#' and acts as a regularization parameter.
#' @references
#'  \insertAllCited{}
#' @author Markus Ulmer
#' @param formula Object of class \code{formula} or describing the model to fit 
#' of the form \code{y ~ x1 + x2 + ...} where \code{y} is a numeric response and 
#' \code{x1, x2, ...} are vectors of covariates. Interactions are not supported.
#' @param data Training data of class \code{data.frame} containing the variables in the model.
#' @param x Matrix of covariates, alternative to \code{formula} and \code{data}.
#' @param y Vector of responses, alternative to \code{formula} and \code{data}.
#' @param max_leaves Maximum number of leaves for the grown tree.
#' @param cp Complexity parameter, minimum loss decrease to split a node. 
#' A split is only performed if the loss decrease is larger than \code{cp * initial_loss}, 
#' where \code{initial_loss} is the loss of the initial estimate using only a stump.
#' @param min_sample Minimum number of observations per leaf. 
#' A split is only performed if both resulting leaves have at least 
#' \code{min_sample} observations.
#' @param mtry Number of randomly selected covariates to consider for a split, 
#' if \code{NULL} all covariates are available for each split.
#' @param fast If \code{TRUE}, only the optimal splits in the new leaves are 
#' evaluated and the previously optimal splits and their potential loss-decrease are reused. 
#' If \code{FALSE} all possible splits in all the leaves are reevaluated after every split.
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
#' @param max_candidates Maximum number of split points that are 
#' proposed at each node for each covariate.
#' @param Q_scale Should data be scaled to estimate the spectral transformation? 
#' Default is \code{TRUE} to not reduce the signal of high variance covariates, 
#' and we do not know of a scenario where this hurts.
#' @param predictors Subset of colnames(X) or numerical indices of the covariates 
#' for which an effect on y should be estimated. All the other covariates are only
#' used for deconfounding.
#' @return Object of class \code{SDTree} containing
#' \item{predictions}{Predictions for the training set.}
#' \item{tree}{The estimated tree of class \code{matrix}. 
#' The tree contains the information about all the splits and the resulting estimates.}
#' \item{var_names}{Names of the covariates in the training data.}
#' \item{var_importance}{Variable importance of the covariates. 
#' The variable importance is calculated as the sum of the decrease in the loss 
#' function resulting from all splits that use this covariate.}
#' @seealso \code{\link{simulate_data_nonlinear}}, \code{\link{regPath.SDTree}}, 
#' \code{\link{prune.SDTree}}, \code{\link{partDependence}}
#' @examples
#' set.seed(1)
#' n <- 10
#' X <- matrix(rnorm(n * 5), nrow = n)
#' y <- sign(X[, 1]) * 3 + rnorm(n)
#' model <- SDTree(x = X, y = y, cp = 0.5)
#' 
#' ###### subset of predictors
#' # if we know, that only the first covariate has an effect on y,
#' # we can estimate only its effect and use the others just for deconfounding
#' model <- SDTree(x = X, y = y, cp = 0.5, predictors = c(1))
#' 
#' \donttest{
#' set.seed(42)
#' # simulation of confounded data
#' sim_data <- simulate_data_step(q = 2, p = 15, n = 100, m = 2)
#' X <- sim_data$X
#' Y <- sim_data$Y
#' train_data <- data.frame(X, Y)
#' # causal parents of y
#' sim_data$j
#' 
#' tree_plain_cv <- cvSDTree(Y ~ ., train_data, Q_type = "no_deconfounding")
#' tree_plain <- SDTree(Y ~ ., train_data, Q_type = "no_deconfounding", cp = 0)
#' 
#' tree_causal_cv <- cvSDTree(Y ~ ., train_data)
#' tree_causal <- SDTree(y = Y, x = X, cp = 0)
#' 
#' # check regularization path of variable importance
#' path <- regPath(tree_causal)
#' plot(path)
#' 
#' tree_plain <- prune(tree_plain, cp = tree_plain_cv$cp_min)
#' tree_causal <- prune(tree_causal, cp = tree_causal_cv$cp_min)
#' plot(tree_causal)
#' plot(tree_plain)
#' }
#' @export
SDTree <- function(formula = NULL, data = NULL, x = NULL, y = NULL, max_leaves = NULL, 
                   cp = 0.01, min_sample = 5, mtry = NULL, fast = TRUE,
                   Q_type = 'trim', trim_quantile = 0.5, q_hat = 0, Qf = NULL, 
                   A = NULL, gamma = 0.5, max_candidates = 100, 
                   Q_scale = TRUE, predictors = NULL){
  input_data <- data.handler(formula = formula, data = data, x = x, y = y)
  X <- input_data$X
  Y <- input_data$Y

  # number of observations
  n <- nrow(X)

  if(is.null(max_leaves)) max_leaves <- n

  max_leaves <- max_leaves - 1

  # check validity of input
  if(n != length(Y)) stop('X and Y must have the same number of observations')
  if(max_leaves < 0) stop('max_leaves must be larger than 1')
  if(min_sample < 1) stop('min_sample must be larger than 0')
  if(cp < 0) stop('cp must be at least 0')
  if(!is.null(mtry) && mtry < 1) stop('mtry must be larger than 0')
  if(n < 2 * min_sample) stop('n must be at least 2 * min_sample')
  if(max_candidates < 1) stop('max_candidates must be at least 1')
  
  return(estimate_tree(boot_index = NULL, Y, X, A, max_leaves, cp, min_sample, mtry, fast,
                       Q_type, trim_quantile, q_hat, Qf, gamma, max_candidates, 
                       Q_scale, predictors))
}
