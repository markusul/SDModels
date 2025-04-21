## Tests for edge case one covariate and one Tree in Forest

set.seed(1)
n <- 10
X <- matrix(rnorm(n * 1), nrow = n)
y <- sign(X[, 1]) * 3 + rnorm(n)
model <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.1, nTree = 1)

# does varImp work
expect_equal(model$var_importance, varImp(model))

# does it predict right
expect_equal(predict(model, newdata = data.frame(X)), model$predictions)

# test of pruning
model <- prune(model, cp = 0.6)
expect_equal(length(model$var_importance), 1)

# regularization path
expect_no_error(regPath(model))

# merging of forests and out of bag predictions
model2 <- SDForest(x = X, y = y, Q_type = 'no_deconfounding', cp = 0.1, nTree = 1)
model3 <- mergeForest(model, model2)
oob3 <- model3$oob_predictions
oob1 <- model$oob_predictions
oob2 <- model2$oob_predictions

oob1[is.na(oob1)] <- 0
oob2[is.na(oob2)] <- 0
oob3[is.na(oob3)] <- 0

oob32 <- oob1 + oob2
oob32[oob1 != 0 & oob2 != 0] <- oob32[oob1 != 0 & oob2 != 0] / 2

expect_equal(oob3, oob32)
