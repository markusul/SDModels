set.seed(1)

X <- matrix(rnorm(50 * 20), nrow = 50)
Y <- rnorm(50)

tree <- SDTree(x = X, y = Y, Q_type = 'no_deconfounding', 
               cp = 0, min_sample = 5)

# does min sample work
expect_true(min(table(tree$predictions)) >= 5)
rpart_tree <- rpart::rpart(y ~ ., data.frame(y = Y, X), 
                           control = rpart::rpart.control(minbucket = 5, 
                                                          cp = 0, 
                                                          minsplit = 10, 
                                                          xval = 0))
pruned_tree <- prune(tree, 0.1)
pruned_rpart_tree <- rpart::prune(rpart_tree, 0.1)

# predict = predictions
expect_equal(tree$predictions, as.vector(predict(tree, data.frame(X))))
# changes in model due to pruning and copy of tree before pruning
expect_false(all(tree$predictions == as.vector(predict(pruned_tree, data.frame(X)))))
# equality of tree and rpart tree (checked using predictions)
expect_equal(tree$predictions, as.vector(predict(rpart_tree)))
# equality of pruned tree and pruned rpart tree (checked using predictions)
expect_equal(as.vector(predict(pruned_tree, data.frame(X))), predict(pruned_rpart_tree))

mostImp <- which.max(varImp(tree))
partDependence(tree, mostImp, X, subSample = 10)

#test single variable and single prediction
tree <- SDTree(x = X[, 1], y = Y, Q_type = 'no_deconfounding', 
               cp = 0, min_sample = 5)
expect_equal(tree$predictions, as.vector(predict(tree, data.frame(X = X[, 1]))))
expect_equal(tree$predictions[1], predict(tree, data.frame(X = X[1, 1])))


#test tree with bootstrap sample
boot_index <- sample(1:50, 30, replace = T)
estimate_tree(Y = Y, X = X, A = NULL, max_leaves = 100, cp = 0, min_sample = 1, 
              mtry = NULL, fast = TRUE, Q_type = "trim", trim_quantile = 0.7, 
              q_hat = 3, Qf = NULL, gamma = NULL, max_candidates = 200, 
              Q_scale = TRUE, predictors = NULL, boot_index = boot_index)

             
#### does it work with only one covariate?
set.seed(1)
X <- matrix(rnorm(50 * 1), nrow = 50)
Y <- rnorm(50)

tree <- SDTree(x = X, y = Y, Q_type = 'no_deconfounding', 
               cp = 0, min_sample = 5)

# does min sample work
expect_true(min(table(tree$predictions)) >= 5)
rpart_tree <- rpart::rpart(y ~ ., data.frame(y = Y, X), 
                           control = rpart::rpart.control(minbucket = 5, 
                                                          cp = 0, 
                                                          minsplit = 10, 
                                                          xval = 0))
pruned_tree <- prune(tree, 0.1)
pruned_rpart_tree <- rpart::prune(rpart_tree, 0.1)

# predict = predictions
expect_equal(tree$predictions, as.vector(predict(tree, data.frame(X))))
# changes in model due to pruning and copy of tree before pruning
expect_false(all(tree$predictions == as.vector(predict(pruned_tree, data.frame(X)))))
# equality of tree and rpart tree (checked using predictions)
expect_equal(tree$predictions, as.vector(predict(rpart_tree)))
# equality of pruned tree and pruned rpart tree (checked using predictions)
expect_equal(as.vector(predict(pruned_tree, data.frame(X))), predict(pruned_rpart_tree))

partDependence(tree, 1, X, subSample = 10)

#test single variable and single prediction
tree <- SDTree(x = X[, 1], y = Y, Q_type = 'no_deconfounding', 
               cp = 0, min_sample = 5)
expect_equal(tree$predictions, as.vector(predict(tree, data.frame(X = X[, 1]))))
expect_equal(tree$predictions[1], predict(tree, data.frame(X = X[1, 1])))
