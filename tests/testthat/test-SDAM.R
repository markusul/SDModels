# test of automatic adaption of breaks depending on data
set.seed(1)
X <- matrix(rnorm(100 * 4), ncol = 4)
X[1:50, 3] <- 0
Y <- sin(X[, 1]) -  X[, 2] + rnorm(100)

# estimate model
expect_no_error(SDAM(x = X, y = Y, n_K = 1, verbose = FALSE, nfolds = 2))


