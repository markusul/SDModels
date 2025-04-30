set.seed(1)

X <- matrix(rnorm(50 * 20), nrow = 50)
Y <- sign(X[, 1]) + rnorm(50)

colnames(X) <- LETTERS[1:20]

selection_list <- list(c(1, 4), 20, "A", c("D", "E"))
selection <- "A"

for(selection in selection_list){
  tree1 <- SDTree(x = X, y = Y, cp = 0, predictors = selection, 
                  Q_type = "no_deconfounding")

  X_sel <- matrix(X[, selection], ncol = length(selection))
  
  if(is.numeric(selection)){
    colnames(X_sel) <- colnames(X)[selection]
  }else{
    colnames(X_sel) <- selection
  }
  
  tree2 <- SDTree(x = X_sel, y = Y, cp = 0, 
                  Q_type = "no_deconfounding")
  
  expect_equal(tree1[c(1, 3, 4)], tree2[c(1, 3, 4)])
  
  # Forest
  set.seed(1)
  forest1 <- SDForest(x = X, y = Y, predictors = selection, 
                      Q_type = "no_deconfounding", nTree = 2)
  set.seed(1)
  forest2 <- SDForest(x = X_sel, y = Y, 
                      Q_type = "no_deconfounding", nTree = 2)
  
  expect_equal(forest1[c(1, 3:11)], forest2[c(1, 3:11)])
  expect_equal(predict(forest1, data.frame(X_sel)), predict(forest2, data.frame(X_sel)))
  expect_equal(predict(forest1, data.frame(X)), predict(forest2, data.frame(X)))
}
