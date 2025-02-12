set.seed(1)

n <- 100
X <- matrix(rnorm(n * 4), nrow = n)
Y <- X[, 1] * rnorm(n)
A <- sample(1:3, n, prob = c(0.3, 0.3, 0.3), replace = TRUE)
A <- as.factor(A)

fit1 <- SDForest(x = X, y = Y, envs = A, nTree_leave_out = 5, cp = 0.1, verbose = FALSE)
fit2 <- SDForest(x = X, y = Y, envs = A, nTree_env = 5, cp = 0.1, verbose = FALSE)
fit3 <- SDForest(x = X, y = Y, envs = A, nTree_leave_out = c('1' = 0, '2' = 3, '3' = 8), 
                 cp = 0.1, verbose = FALSE)

expect_equal(length(fit1$forest), 5 * length(unique(A)))
expect_equal(length(fit2$forest), 5 * length(unique(A)))
expect_equal(length(fit3$forest), sum(c('1' = 0, '2' = 3, '3' = 8)))


