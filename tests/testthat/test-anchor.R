set.seed(1)

n <- 50
X <- matrix(rnorm(n * 20), nrow = n)
Y <- rnorm(n)
A <- rnorm(n)

expect_no_error(SDForest(x = X, y = Y, A = A, Q_type = 'no_deconfounding', nTree = 2))
expect_no_error(SDForest(x = X, y = Y, A = A, nTree = 2))

# gamma = 1 should have no effect
tree1 <- SDTree(x = X, y = Y, Q_type = 'no_deconfounding')
tree2 <- SDTree(x = X, y = Y, Q_type = 'no_deconfounding', A = A, gamma = 1)

expect_equal(tree1$predictions, tree2$predictions)