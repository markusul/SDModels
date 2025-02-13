set.seed(1)
#RhpcBLASctl::omp_set_num_threads(1)

X <- matrix(rnorm(50 * 20), nrow = 50)
Y <- rnorm(50)

expect_no_error(SDForest(x = X, y = Y, Q_type = 'no_deconfounding', 
        nTree = 2, mc.cores = 2, verbose = FALSE))

