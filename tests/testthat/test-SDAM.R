# test of automatic adaption of breaks depending on data
set.seed(1)
X <- matrix(rnorm(100 * 15), ncol = 15)
X[1:50, 3] <- 0
Y <- sin(X[, 1]) -  X[, 2] + rnorm(100)

# estimate model
model <- SDAM(x = X, y = Y, n_K = 1)