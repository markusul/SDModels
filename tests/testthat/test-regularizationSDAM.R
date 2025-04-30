# test of automatic adaption of breaks depending on data
set.seed(1)
n <- 10
X <- matrix(rnorm(n * 4), ncol = 4)
X[1:(n/2), 3] <- 0
Y <- sin(X[, 1]) -  X[, 2] + rnorm(n)

colnames(X) <- LETTERS[1:4]

set.seed(3)
fit1 <- SDAM(x = X, y = Y, n_K = 1, verbose = FALSE, nfolds = 2)
set.seed(3)
fit2 <- SDAM(x = X, y = Y, notRegularized = 3:4, n_K = 1, verbose = FALSE, nfolds = 2)
set.seed(3)
fit3 <- SDAM(x = X, y = Y, notRegularized = LETTERS[3:4], n_K = 1, verbose = FALSE, nfolds = 2)

coef1 <- fit1$coefs[3:4]
coef2 <- fit2$coefs[3:4]

coef1 <- unlist(lapply(coef1, function(cu) {
  if(is.null(cu)){
    rep(0, 4)
  }else abs(cu)
}))

coef2 <- unlist(lapply(coef2, function(cu) {
  if(is.null(cu)){
    rep(0, 4)
  }else abs(cu)
}))

expect_equal(fit2, fit3)
expect_true(all(coef1 <= coef2))
