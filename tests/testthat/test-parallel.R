set.seed(1)
n <- 50
X <- matrix(rnorm(n * 20), nrow = n)
Y <- rnorm(n)

# fit in parallel
set.seed(33)
expect_no_error(
fit <- SDForest(x = X, y = Y, Q_type = 'no_deconfounding', 
        nTree = 2, mc.cores = 2, verbose = FALSE)
)

# predict in parallel
expect_no_error(
pred <- predict(fit, newdata = data.frame(X), mc.cores = 2)
)

# reproducibility
set.seed(33)
fit2 <- SDForest(x = X, y = Y, Q_type = 'no_deconfounding', 
                 nTree = 2, mc.cores = 2, verbose = FALSE)
pred2 <- predict(fit2, newdata = data.frame(X), mc.cores = 2)

# compare everything except the data.trees
forest_ind <- which(names(fit) == "forest")
expect_equal(fit[-forest_ind], fit2[-forest_ind])

expect_equal(pred, pred2)
