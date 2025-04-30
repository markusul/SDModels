n <- 10000
set.seed(1)
X <- matrix(rnorm(n * 4), ncol = 4)
X[1:50, 3] <- 0
Y <- sin(X[, 1]) -  X[, 2] + rnorm(n)

# test SDAM using only linear and not regularized variables
expect_warning(
fit1 <- SDAM(x = X, y = Y, n_K = 1, verbose = FALSE, nfolds = 2, 
             ind_lin = 1:4, notRegularized = 1:4)
)

# does the solution correspond to the lm result?
fit2 <- lm(Y ~ X)
expect_equal(unlist(fit1$coefs), fit2$coefficients[fit1$var_names])
