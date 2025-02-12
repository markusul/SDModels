set.seed(1)

n <- 50
X <- matrix(rnorm(n * 20), nrow = n)
Y <- rnorm(n)
A <- rnorm(n)

expect_no_error(SDForest(x = X, y = Y, A = A, Q_type = 'no_deconfounding', nTree = 2))
expect_no_error(SDForest(x = X, y = Y, A = A, nTree = 2))
