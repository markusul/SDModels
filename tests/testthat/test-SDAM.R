set.seed(1)

X <- matrix(rnorm(30 * 20), nrow = 30)
Y <- rnorm(30)

# fitting works
expect_no_message({
  mod_trim <- SDAM(X, Y, Q_type = "trim", trim_quantile = 0.5, cv_k = 3, n_K = 4, n_lambda1 = 4, n_lambda2 = 8)
  mod_pca <- SDAM(X, Y, Q_type = "pca", q_hat = 3, cv_k = 3, n_K = 4, n_lambda1 = 4, n_lambda2 = 8)
  mod_none <- SDAM(X, Y, Q_type = "no_deconfounding", cv_k = 3, n_K = 4, n_lambda1 = 4, n_lambda2 = 8)
})

# print works
expect_no_message({
  print(mod_trim)
})

# varImp works
expect_no_message({
  varImp(mod_trim)
})


# predict works
expect_no_message({
  Xnew <- matrix(rnorm(5 * 20), nrow = 5)
  predict(mod_trim, Xnew = Xnew)
  Xjnew <- seq(-1, 1, length.out = 10)
  predict_individual_fj(mod_trim, Xjnew = Xjnew, j = 4)
})

