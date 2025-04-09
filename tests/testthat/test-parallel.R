set.seed(1)
#RhpcBLASctl::omp_set_num_threads(1)
n <- 50
X <- matrix(rnorm(n * 20), nrow = n)
Y <- rnorm(n)

# fit in parallel
expect_no_error(
fit <- SDForest(x = X, y = Y, Q_type = 'no_deconfounding', 
        nTree = 2, mc.cores = 2, verbose = FALSE)
)

# predict in parallel
expect_no_error(
predict(fit, newdata = data.frame(X), mc.cores = 2)
)

