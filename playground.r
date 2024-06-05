source("R/SDForest.r")

library('ranger')

simulate_data_nonlinear <- function(q, p, n, m, eff = NULL, a = 0){
    #simulate data with confounding and non-linear f_X
    # q: number of confounding covariates in H
    # p: number of covariates in X
    # n: number of observations
    # m: number of covariates with a causal effect on Y

    m <- min(m, p)

    n_A_levels <- a

    if(a == 0){
        A <- rep(0, n)
    }else{
        #A_levels <- 1:n_A_levels
        #A <- sample(A_levels, n, replace = TRUE)
        A <- rnorm(n, 0, 1)
    }

    alpha_1 <- rnorm(p, 1, 0.1)
    #alpha_2 <- rnorm(q)
    #alpha_3 <- rnorm(1)
    #alpha_1 <- 1
    alpha_2 <- 0
    alpha_3 <- 0

    # complexity of f_X
    complexity <- 5
    # random parameter for fourier basis
    beta <- runif(m * complexity * 2, -1, 1)

    # random confounding covariates H
    if(q == 0){
        H <- matrix(0, nrow = n, ncol = 1)
        
        # random correlation matrix cov(X, H)
        Gamma <- matrix(0, nrow = 1, ncol = p)

        # random coefficient vector delta
        delta <- 0
    }else{
        H <- matrix(rnorm(n * q, 0, 1), nrow = n)
        H <- H + A %*% t(alpha_2)

        # random correlation matrix cov(X, H)
        Gamma <- matrix(rnorm(q * p, 0, 1), nrow = q)

        # random coefficient vector delta
        delta <- rnorm(q, 0, 2)
    }



    if(!is.null(eff)){
        non_effected <- p - eff
        if(non_effected <= 0) stop('eff must be smaller than p or NULL')
        
        Gamma[, sample(1:p, non_effected)] <- 0
    }



    # random error term
    E <- matrix(rnorm(n * p, 0, 1), nrow = n)

    X <- A %*% t(alpha_1) + H %*% Gamma + E
  
    # random sparse subset of covariates in X
    js <- sample(1:p, m)

    # generate f_X
    f_X <- apply(X, 1, function(x) f_four(x, beta, js))
    
    # generate Y
    Y <- f_X + H %*% delta + A %*% t(alpha_3) + rnorm(n, 0, 0.1)
  
    #return data
    dep <- list(alpha_1 = alpha_1, alpha_2 = alpha_2, alpha_3 = alpha_3, beta = beta, delta = delta, Gamma = Gamma)
    return(list(X = X, Y = Y, f_X = f_X, j = js, beta = beta, H = H, A = A, dep = dep))
}

set.seed(22)
data <- simulate_data_nonlinear(1, 50, 200, 1, a = 3)

X<- data$X

X
n <- nrow(X)
s <- find_s(X)
min_sample <- 2
s[-c(0:(min_sample - 1), (n - min_sample + 2):(n+1)), ]



apply(X, 2, quantile, seq(0.01, 0.1))

n <- 200
q <- 1
p <- 10

a_seq <- seq(-10, 10, 1)
a_seq <- 3
mse_list <- c()
mse_inv_list <- c()

plot(data$X[, data$j], data$Y, ylim = c(-10, 10), xlim = c(-12, 12))
points(data$X[, data$j], data$f_X, col = 'green', pch = 20, cex = 0.5)
for(i in a_seq){
    A <- rep(i, n)
    H <- matrix(rnorm(n * q, 0, 1), nrow = n)
    H <- H + A %*% t(data$dep$alpha_2)
    E <- matrix(rnorm(n * p, 0, 1), nrow = n)
    X <- A %*% t(data$dep$alpha_1) + H %*% data$dep$Gamma + E
    f_X <- apply(X, 1, function(x) f_four(x, data$beta, data$j))

    y_a <- f_X + H %*% data$dep$delta + A %*% t(data$dep$alpha_3) + rnorm(n_a, 0, 0.1)

    y_pred_inv <- predict(sdf10, data.frame(X))
    y_pred <- predict(sdf, data.frame(X))
    mse_inv <- quantile(abs(y_pred_inv - y_a), 0.99)
    mse <- quantile(abs(y_pred - y_a), 0.99)

    mse_list <- c(mse_list, mse)
    mse_inv_list <- c(mse_inv_list, mse_inv)

    points(X[, data$j], y_a, col = i + 4, pch = 20, cex = 0.5)
}

plot(a_seq, mse_list, col = 'black')
points(a_seq, mse_inv_list, col = 'red', pch = 20, cex = 0.5)
legend('topleft', legend = c('gam = 1', 'gam = 100'), col = c('black', 'red'), pch = c(1, 20), cex = 1)

points(data$X[, data$j], predict(sdf, data.frame(data$X)), col = '#204dca', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdf10, data.frame(data$X)), col = '#aa0caf', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdf0, data.frame(data$X)), col = '#af0c58', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdfd, data.frame(data$X)), col = '#0c9caf', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdfda, data.frame(data$X)), col = '#84e64c', pch = 20, cex = 0.5)


set.seed(2)
data <- simulate_data_nonlinear(1, 100, 200, 1)
X <- data$X
Y <- data$Y

#a <- 3
#X_train <- data$X[data$A != a, ]
#Y_train <- data$Y[data$A != a]
#A_train <- as.matrix(data$A[data$A != a])

X_train <- data$X
Y_train <- data$Y
A_train <- as.matrix(data$A)


sdf <- cv.SDTree(x = X_train, y = Y_train, Q_type = 'no_deconfounding')
library(ggplot2)
ggplot(sdf$cp_table, aes(x = cp, y = `SDLoss mean`)) + geom_point() + geom_line(  ) + 
  geom_ribbon(aes(ymin = `SDLoss mean` - `SDLoss sd`, ymax = `SDLoss mean` + `SDLoss sd`), alpha = 0.2)


tree <- SDTree(x = X_train, y = Y_train, Q_type = 'no_deconfounding', cp = sdf$cp_min)
plot(tree)


# fit SDForest
sdf <- SDForest(x = X_train, y = Y_train, Q_type = 'no_deconfounding', cp = 0, mtry = 1, min_sample = 5, max_candidates = 200)
sdf2 <- SDForest(x = X_train, y = Y_train, Q_type = 'no_deconfounding', cp = 0, mtry = 1, min_sample = 5, max_candidates = 20)

plot(sdf$predictions, sdf2$predictions)
sdf
sdf2


path <- regPath(sdf)
plot(path)

sdf0 <- SDForest(x = X_train, y = Y_train, A = A_train, gamma = 0, 
    Q_type = 'no_deconfounding')
sdfd <- SDForest(x = X_train, y = Y_train)
sdfda <- SDForest(x = X_train, y = Y_train, A = A_train, gamma = 0)
sdfda10 <- SDForest(x = X_train, y = Y_train, A = A_train, gamma = 100000, cp = 0.1)

predict_outsample(sdf10$forest[[1]]$tree, sdf10$X)
prune(sdf10, cp = 0.1)
path <- regPath(sdf10, multicore = F, oob = T)
plotOOB(path)
sdf
sdf10$X

X_pred <- data.frame(data$X)
names(X_pred) <- sdf$var_names

W <- get_W(A_train, gamma = 10000)
fit_linA <- lm.fit(x = W %*% X, y = W %*% Y)$coefficients
pred_linA <- data$X %*% fit_linA

fit_lin <- lm.fit(x = X, y = Y)$coefficients
pred_lin <- data$X %*% fit_lin


points(data$X[, data$j], pred_linA, col = 'red')
points(data$X[, data$j], pred_lin, col = 'blue')


plot(data$X[, data$j], data$Y, ylim = c(min(data$Y, data$f_X), max(data$Y, data$f_X)))
points(data$X[, data$j], predict(sdf, X_pred), col = '#204dca', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdf10, X_pred), col = '#aa0caf', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdf0, X_pred), col = '#af0c58', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdfd, X_pred), col = '#0c9caf', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdfda, X_pred), col = '#84e64c', pch = 20, cex = 0.5)
points(data$X[, data$j], predict(sdfda10, X_pred), col = 'blue', pch = 21, cex = 0.5)

points(data$X[, data$j], data$f_X, col = '#0c960c', pch = 20, cex = 0.5)


title('n = 200, p = 200, q = 1')
legend('bottomright', legend = c('gam = 1', 'gam = 100', 'gam = 0', 'trim', 'gam = 0 + trim', 'true causal'), 
    col = c('#204dca', '#aa0caf', '#af0c58', '#0c9caf', '#84e64c', '#0c960c'), pch = 20, cex = 1)

path <- regPath(sdfd, multicore = TRUE, oob = T)
dev.off()
plot(path)
plotOOB(path)

path$varImp_path

plot(path10)
plotOOB(path10)
dev.off()
library(glmnet)
source("R/SDForest.r")
n_train <- 503
n_test <- 500
r <- 5
q <- 2
p <- 1

n_caus <- 1

var_X <- 1
var_H <- 2
var_A <- 1
var_Y <- 0.01

beta_strength <- 1
delta_strength <- 1
gamma_strength <- 1
alpha_1_strength <- 1
alpha_2_strength <- 1
alpha_3_strength <- 0

A_shift <- 1000

beta <- matrix(0, nrow = p)
beta[1:n_caus, ] <- rnorm(n_caus, 0, beta_strength)

delta <- matrix(rnorm(q, 0, delta_strength), nrow = q)
gamma <- matrix(rnorm(q * p, 0, gamma_strength), nrow = q)
alpha_1 <- matrix(rnorm(r * p, 0, alpha_1_strength), nrow = r)
alpha_2 <- matrix(rnorm(r, 0, alpha_2_strength), nrow = r)
alpha_3 <- matrix(rnorm(r * q, 0, alpha_3_strength), nrow = r)

A <- matrix(rnorm(n_train * r, 0, var_A), nrow = n_train)
H <- A %*% alpha_3 + matrix(rnorm(n_train * q, 0, var_H), nrow = n_train)
X <- A %*% alpha_1 + H %*% gamma + matrix(rnorm(n_train * p, 0, var_X), nrow = n_train)
Y <- X %*% beta + H %*% delta + A %*% alpha_2 + rnorm(n_train, 0, var_Y)

mean(abs(X %*% beta))
mean(abs(H %*% delta))
mean(abs(A %*% alpha_2))
mean(abs(rnorm(n_train, 0, var_Y)))

A_test <- matrix(rnorm(n_test * r, 0, var_A), nrow = n_test) + A_shift
H_test <- A_test %*% alpha_3 + matrix(rnorm(n_test * q, 0, var_H), nrow = n_test)
X_test <- A_test %*% alpha_1 + H_test %*% gamma + matrix(rnorm(n_test * p, 0, var_X), nrow = n_test)
Y_test <- X_test %*% beta + H_test %*% delta + A_test %*% alpha_2 + rnorm(n_test, 0, var_Y)

W <- get_W(A, gamma = 100)

if(p == 1) {
  Q <- diag(n_train)
}else Q <- get_Q(X, type = 'trim')



fit_lin_cv <- cv.glmnet(x = X, y = Y, alpha = 1, nfolds = 10)
fit_lin <- glmnet(x = X, y = Y, alpha = 1, lambda = fit_lin_cv$lambda.1se)

fit_linA_cv <- cv.glmnet(x = W %*% X, y = W %*% Y, alpha = 1, nfolds = 10)
fit_linA <- glmnet(x = W %*% X, y = W %*% Y, alpha = 1, lambda = fit_linA_cv$lambda.1se)


fit_linQ_cv <- cv.glmnet(x = Q %*% X, y = Q %*% Y, alpha = 1, nfolds = 10)

fit_linQ <- glmnet(x = Q %*% X, y = Q %*% Y, alpha = 1, lambda = fit_linQ_cv$lambda.1se)

#fit_linA <- lm.fit(x = W %*% X, y = W %*% Y)$coefficients
#fit_lin <- lm.fit(x = X, y = Y)$coefficients
#fit_linQ <- lm.fit(x = Q %*% X, y = Q %*% Y)$coefficients

coefA <- coef(fit_linA)[-1, 1]
coef <- coef(fit_lin)[-1, 1]
coefQ <- coef(fit_linQ)[-1, 1]


pred_linA <- predict(fit_linA, newx = X_test)[, 1]
pred_lin <- predict(fit_lin, newx = X_test)[, 1]
pred_linQ <- predict(fit_linQ, newx = X_test)[, 1]


coef_col <- rep('black', p)
coef_col[1:n_caus] <- '#0ea10e'

ceof_df <- data.frame(beta = abs(beta), fit_linA = abs(coefA), 
  fit_lin = abs(coef), fit_linQ = abs(coefQ))
#plot(ceof_df, col = coef_col, pch = 20, cex = 1)


par(mfrow = c(1, 2))
plot(X_test[, 1], Y_test)
points(X_test[, 1], pred_lin, col = 'blue', pch = 20, cex = 1)
points(X_test[, 1], pred_linA, col = 'red', pch = 20, cex = 0.8)
points(X_test[, 1], pred_linQ, col = '#be24b7', pch = 20, cex = 0.5)
points(X_test[, 1], X_test %*% beta, col = 'green', pch = 20, cex = 0.3)

perf <- data.frame(lin = abs(Y_test - pred_lin), linA = abs(Y_test - pred_linA), 
  true = abs(Y_test - X_test %*% beta), linQ = abs(Y_test - pred_linQ))

boxplot(perf, col = c('blue', 'red', 'green', '#be24b7'), pch = 20, cex = 0.8)


n <- 300
H <- rnorm(n)
A <- (rbinom(n, 1, 0.5) - 1) * 2
X <- A + H + rnorm(n, 0, 1)
Y <- X + 2 * H + rnorm(n, 0, 1)

H_test <- rnorm(n)
A_test <- -4
X_test <- A_test + H_test + rnorm(n, 0, 1)
Y_test <- X_test + 2 * H_test + rnorm(n, 0, 1)


W <- get_W(matrix(A), gamma = 1000)
fit_linA <- lm.fit(x = W %*% X, y = W %*% Y)$coefficients
fit_lin <- lm.fit(x = matrix(X), y = Y)$coefficients

pred_lin <- X_test * fit_lin
pred_linA <- X_test * fit_linA


plot(X, Y, ylim = c(-20, 20), xlim = c(-10, 10))
points(X_test, Y_test, col = 'red')
points(X_test, pred_lin, col = 'blue', pch = 20, cex = 0.5)
points(X_test, pred_linA, col = 'green', pch = 20, cex = 0.5)

source("R/SDForest_gpu.r")

library('ranger')

m <- 5
p <- 200
n <- 200
q <- 4

data <- simulate_data_nonlinear(q, p, n, m)

X <- data$X
Y <- data$Y
#source("R/SDForest.r")
a <- Sys.time()
fit1 <- SDForest(x = X, y = Y, Q_type = 'no_deconfounding')
b <- Sys.time()
b - a

c <- Sys.time()
fit2 <- ranger(Y ~ ., data = data.frame(X, Y), num.trees = 100, mtry = floor(0.5 * p), 
    min.node.size = 3)
d <- Sys.time()

b - a
d - c

fit1$oob_loss
fit2$prediction.error

max(abs(fit1$predictions - fit2$predictions))
plot(fit1$predictions, fit2$predictions)

library('rpart')

fit3 <- rpart::rpart(Y ~ ., data = data.frame(X, Y), 
    control = rpart.control(cp = 0, minsplit = 6, xval =1))
max(abs(predict(fit3, data.frame(X)) - fit1$predictions))

plot(predict(fit3, data.frame(X)))
points(fit1$predictions, col = 'red')

print(fit1$tree, 'value', 's', 'j', 'label', 'cp')
fit3

sort(fit1$var_importance, decreasing = TRUE)[1:6]


data <- simulate_data_nonlinear(q, 3000, 3000, m)
X <- data$X
Y <- data$Y

X_gpu <- gpu.matrix(X)
Y_gpu <- gpu.matrix(Y)


c <- Sys.time()
lj <- lapply(1:10000, function(i)t(X_gpu) %*% X_gpu)
d <- Sys.time()

b - a
d - c



library(GPUmatrix)



tot <- 5000 * 5000
 
2e+07
1000000000
800000000
k <- tot / 1000

X <- matrix(0, nrow = k, ncol = 1000)
X_gpu <- gpu.matrix(X)
X_gpu@type
X_gpu %*% t(X_gpu)


gc()
X_gpu <- as.matrix(X_gpu)

rm(X_gpu)


for(i in 1:100){
    print(i)
    X_gpu <- gpu.matrix(X)
    Y <- t(X_gpu) %*% X
}



sdf$oob_loss

pred <- predictOOB(sdf, X_train)
mean((Y_train - pred)**2)
sdf$oob_loss


sdf$X

for(i in 1:100){
    predict_outsample(sdf$forest[[i]]$tree, X_train)
}


sdf$oob_ind

source("R/SDForest_gpu.r")
set.seed(2)
data <- simulate_data_nonlinear(1, 400, 100, 5)
X <- data$X
Y <- data$Y

X[, 1] <- 3

res <- SDTree(x = X, y = Y, Q_type = 'no_deconfounding', gpu = T)
res


library('rpart')
res2 <- rpart(Y ~ ., data = data.frame(X, Y), control = rpart.control(cp = 0.01, minsplit = 10, xval =1))
res2

max(res$predictions - predict(res2, data.frame(X)))

X_branch <- X
X_branch[, 1] <- 3

X <- matrix(rnorm(10), nrow = 10, ncol = 10, byrow = TRUE)
Y <- rnorm(10)

SDTree(x = X, y = Y)



fit1 <- SDTree(x = X, y = Y, Q_type = 'no_deconfounding')



start <- Sys.time()
sdf <- SDForest(x = X, y = Y, Q_type = 'no_deconfounding', gpu = T, multicore = T, return_data = F)
end <- Sys.time()
end - start

sdf$var_importance


path <- regPath(sdf, oob = T, X = X, Y = Y, multicore = T, mc.cores = 10)

plotOOB(path)
plot(path)

path2 <- stabilitySelection(sdf, multicore = T, mc.cores = 10)
plot(path2)

prune(sdf, cp = 0.1, oob = T, X = X, Y = Y)

predictOOB(sdf, X)



source('R/SDForest.r')
library(gridExtra)
library(ggplot2)
library(ranger)
library(ggsci)
library(ggpubr)


true_function <- function(beta, js){
    res <- list(beta = beta, js = js)
    class(res) <- 'true_function'
    return(res)
}

predict.true_function <- function(object, newdata){
    f_X <- apply(newdata, 1, function(x) f_four(x, object$beta, object$js))
    return(f_X)
}

plotDep <- function(object, n_examples = 19){
  ggdep <- ggplot2::ggplot() + ggplot2::theme_bw()
  preds <- object$preds
  x_seq <- object$x_seq
  
  sample_examples <- sample(1:dim(preds)[2], n_examples)
  for(i in sample_examples){
      pred_data <- data.frame(x = x_seq, y = preds[, i])
      ggdep <- ggdep + ggplot2::geom_line(data = pred_data, ggplot2::aes(x = x, y = y), col = 'grey')
  }

  ggdep <- ggdep + ggplot2::geom_line(data = data.frame(x = x_seq, y = object$preds_mean), 
                    ggplot2::aes(x = x, y = y), col = '#08cbba', linewidth = 1.5)
  ggdep <- ggdep + ggplot2::geom_point(data = data.frame(x = object$xj, y = -5), 
                    ggplot2::aes(x = x, y = y), col = 'black', size = 1,shape = 108)
  ggdep <- ggdep + ggplot2::ylab('f(x)')
  ggdep <- ggdep + ggplot2::xlim(quantile(object$xj, 0.05), quantile(object$xj, 0.95))
  if(is.character(object$j)){
    ggdep <- ggdep + ggplot2::xlab(object$j)
  }else{
    ggdep <- ggdep + ggplot2::xlab(paste('x', object$j, sep = ''))
  }
  ggdep + ggplot2::ylim(-5, 6)
}

library('ranger')
source("R/SDForest.r")
library(data.tree)


p <- 100
n <- 100
q <- 20

n_test <- 500

#set.seed(2024)
data <- simulate_data_nonlinear(q, p, n + n_test, 5)
data_test <- data
data_test$Y <- data_test$Y[(n+1):(n+n_test)]
data_test$X <- data_test$X[(n+1):(n+n_test),]
data_test$f_X <- data_test$f_X[(n+1):(n+n_test)]

data$X <- data$X[1:n,]
data$Y <- matrix(data$Y[1:n])
data$f_X <- data$f_X[1:n]

colnames(data$X) <- paste('cov', 1:p, sep = '')

fit1 <- SDForest(x = data$X, y = data$Y, Q_type = 'no_deconfounding', return_data = T, nTree = 100)
fit2 <- SDForest(x = data$X, y = data$Y, return_data = T, nTree = 100)

cp_seq <- unique((unlist(lapply(fit1$forest, function(x) x$tree$Get('cp_max')))))
cp_seq[cp_seq > 1] <- 1
cp_seq <- unique(ceiling(cp_seq * 1000)/1000)
cp_seq <- c(0, cp_seq)
path1 <- regPath(fit1, cp_seq = cp_seq)
cp_seq <- unique((unlist(lapply(fit2$forest, function(x) x$tree$Get('cp_max')))))
cp_seq[cp_seq > 1] <- 1
cp_seq <- unique(ceiling(cp_seq * 1000)/1000)
cp_seq <- c(0, cp_seq)
path2 <- regPath(fit2, cp_seq = cp_seq)

plotOOB(path1)
plotOOB(path2)

imp1 <- fit1$var_importance / max(fit1$var_importance)
imp2 <- fit2$var_importance / max(fit2$var_importance)
impcol <- rep('black', p)
impcol[data$j] <- 'blue'
plot(imp1, imp2, col = impcol)

fit2 <- toYamlForest(fit)
save(fit2, file = 'ytree.rda')
rm(fit2)
load('ytree.rda')
fit2 <- fromYamlForest(fit2)

fit2$oob_predictions
fit2$oob_loss
oob_pred <- predictOOB(fit2)

plot(fit$oob_predictions, oob_pred)

fit2$oob_loss
path <- regPath(fit)
plotOOB(path)
pred <- predict(fit, as.data.frame(data$X))
pred2 <- predict(prune(fit, 0), as.data.frame(data$X))

fit3 <- prune(fit, 0)

fit$oob_SDloss
fit3$oob_SDloss

fit2$oob_loss
path2$loss_path[1, ]

path2 <- regPath(fit2)
plotOOB(path2)

max(abs(predictOOB(fit2) - fit2$oob_predictions))


yTree <- as.yaml(as.list(tree))
tree

save(tree, file = 'tree.rda')
save(tree2, file = 'ytree.rda')


toYamlTree <- function(tree){
  tree$tree <- as.yaml(as.list(tree$tree))
  return(tree)
}

fromYamlTree <- function(tree){
  tree$tree <- as.Node(yaml.load(tree$tree))
  return(tree)
}

toYamlForest <- function(forest){
  forest$forest <- lapply(forest$forest, toYamlTree)
  return(forest)
}

fromYamlForest <- function(forest){
  forest$forest <- lapply(forest$forest, fromYamlTree)
  return(forest)
}




as.Node(yaml.load(yTree))


fit2 <- SDForest(x = data$X, y = data$Y, nTree = 10)

fit1$forest


fit2 <- SDForest(x = data$X, y = data$Y, nTree = 10)
fit <- mergeForest(fit1, fit2)

fit1$oob_SDloss
fit2$oob_SDloss
fit$oob_SDloss

fit <- SDForest(x = data$X, y = data$Y, nTree = 10, mtry = 5)
path <- regPath(fit)
plotOOB(path)
plot(path)

mergeForest <- function(fit1, fit2){
  if(any(fit1$var_names != fit2$var_names)) stop('forest must be trained using the same covariates')

  len_1 <- length(fit1$forest)
  len_2 <- length(fit2$forest)
  len_new <- len_1 + len_2

  fit1$forest <- c(fit1$forest, fit2$forest)
  fit1$var_importance <- (fit1$var_importance * len_1 + 
    fit2$var_importance * len_2) / len_new

  if(all(fit1$X == fit2$X, fit1$Y == fit2$Y, fit1$Q == fit2$Q, 
    !is.null(fit1$X), !is.null(fit1$Y), !is.null(fit1$Q))){
    fit1$predictions <- (fit1$predictions * length(fit1$forest) + 
      fit2$predictions * length(fit2$forest)) / len_new

    oob_weights_1 <- sapply(fit1$oob_ind, length)
    oob_weights_2 <- sapply(fit2$oob_ind, length)
    oob_weights_new <- oob_weights_1 + oob_weights_2

    oob_predictions_1 <- fit1$oob_predictions * oob_weights_1
    oob_predictions_2 <- fit2$oob_predictions * oob_weights_2

    fit1$oob_predictions <- rowSums(cbind(oob_predictions_1, oob_predictions_2), na.rm = T)
    fit1$oob_predictions <- fit1$oob_predictions / oob_weights_new
    
    fit1$oob_ind <- lapply(1:length(fit1$Y), function(i){
      c(fit1$oob_ind[[i]], fit2$oob_ind[[i]] + len_1)
    })
    fit1$oob_SDloss <- loss(fit1$Q %*% fit1$Y, fit1$Q %*% fit1$oob_predictions)
    fit1$oob_loss <- loss(fit1$Y, fit1$oob_predictions)
  }else{
    warning('forests migth be trained on different data')
    fit1$X <- NULL
    fit1$Y <- NULL
    fit1$Q <- NULL
    fit1$predictions <- NULL
    fit1$oob_prediction <- NULL
    fit1$oob_ind <- NULL
    fit1$oob_loss <- NULL
    fit1$oob_SDloss <- NULL
  }
  return(fit1)
}




fit2 <- ranger(x = data$X, y = data$Y, importance = 'impurity', num.trees = 200, mtry = 90)


plot(fit$var_importance, fit2$variable.importance)


tree$Do(function(node) node$cp_max <- max(node$Get('cp')))
tree$Do(function(node) {
  cp_max <- data.tree::Aggregate(node, 'cp_max', max)
  node$children[[1]]$cp_max <- cp_max
  node$children[[2]]$cp_max <- cp_max
  }, filterFun = data.tree::isNotLeaf
)

update_forest <- function(object){
  object$forest <- lapply(object$forest, function(tree) {
    tree$tree$Do(function(node) node$cp_max <- max(node$Get('cp')))
    tree$tree$Do(function(node) {
      cp_max <- data.tree::Aggregate(node, 'cp_max', max)
      node$children[[1]]$cp_max <- cp_max
      node$children[[2]]$cp_max <- cp_max
      }, filterFun = data.tree::isNotLeaf
    )
    return(tree)
  })
}


update_forest(fit2)
path <- regPath(fit2)



prune.SDTree <- function(object, cp){
  data.tree::Prune(object$tree, function(x) max(x$Get('cp')) > cp)
  object$tree$Do(leave_names, filterFun = data.tree::isLeaf)
  object$predictions <- NULL
  object$var_importance <- varImp(object)
  return(object)
}


f2 <- function(object, cp){
  data.tree::Prune(object$tree, function(x) x$res_cp > cp)
  object$tree$Do(leave_names, filterFun = data.tree::isLeaf)
  object$predictions <- NULL
  object$var_importance <- varImp(object)
  return(object)
}




set.seed(42)
fit2 <- SDForest(x = data$X, y = data$Y, return_data = T, mtry = 90)


data$j
sort(fit$var_importance, decreasing = T)[1:6]
sort(fit2$var_importance, decreasing = T)[1:6]

path <- regPath(fit)
path2 <- regPath(fit2)

plot(path, log_scale = T)
plot(path2)

plotOOB(path)
plotOOB(path2)

prune(fit, cp = path$cp_min)
fit$forest

prune(fit2, cp = path2$cp_min)
fit2$forest


spath <- stabilitySelection(fit)
spath2 <- stabilitySelection(fit2)

plot(spath)
plot(spath2)



true_f <- true_function(data$beta, data$j)

dep_f_1 <- condDependence(true_f, data$j[1], data$X)
dep_f_2 <- condDependence(true_f, data$j[2], data$X)
dep_f_3 <- condDependence(true_f, data$j[3], data$X)
dep_f_4 <- condDependence(true_f, data$j[4], data$X)

dep_1 <- condDependence(fit, data$j[1], multicore = T)
dep_2 <- condDependence(fit, data$j[2], multicore = T)
dep_3 <- condDependence(fit, data$j[3], multicore = T)
dep_4 <- condDependence(fit, data$j[4], multicore = T)

library(ggplot2)
library(ggpubr)
ggdep1 <- plotDep(dep_1) + 
  geom_line(aes(x = dep_f_1$x_seq, y = dep_f_1$preds_mean, col = 'red'), linewidth = 0.2) + 
  ggplot2::labs(col = "") + 
  ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

ggdep2 <- plotDep(dep_2) + 
  geom_line(aes(x = dep_f_2$x_seq, y = dep_f_2$preds_mean, col = 'red'), linewidth = 0.2) + 
  ggplot2::labs(col = "") + 
  ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

ggdep3 <- plotDep(dep_3) +
  geom_line(aes(x = dep_f_3$x_seq, y = dep_f_3$preds_mean, col = 'red'), linewidth = 0.2) + 
  ggplot2::labs(col = "") + 
  ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

ggdep4 <- plotDep(dep_4) +
  geom_line(aes(x = dep_f_4$x_seq, y = dep_f_4$preds_mean, col = 'red'), linewidth = 0.2) + 
  ggplot2::labs(col = "") + 
  ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

gg_cond_rf <- ggarrange(ggdep1, ggdep2, ggdep3, ggdep4,
  ncol = 2, nrow = 2, common.legend = T, legend = 'bottom')
gg_cond_rf


dep2_1 <- condDependence(fit2, data$j[1], multicore = T)
dep2_2 <- condDependence(fit2, data$j[2], multicore = T)
dep2_3 <- condDependence(fit2, data$j[3], multicore = T)
dep2_4 <- condDependence(fit2, data$j[4], multicore = T)

ggdep1 <- plotDep(dep2_1) + 
  geom_line(aes(x = dep_f_1$x_seq, y = dep_f_1$preds_mean, col = 'red'), linewidth = 0.2) + 
  ggplot2::labs(col = "") + 
  ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

ggdep2 <- plotDep(dep2_2) +
    geom_line(aes(x = dep_f_2$x_seq, y = dep_f_2$preds_mean, col = 'red'), linewidth = 0.2) + 
    ggplot2::labs(col = "") + 
    ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

ggdep3 <- plotDep(dep2_3) + 
    geom_line(aes(x = dep_f_3$x_seq, y = dep_f_3$preds_mean, col = 'red'), linewidth = 0.2) + 
    ggplot2::labs(col = "") + 
    ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

ggdep4 <- plotDep(dep2_4) + 
    geom_line(aes(x = dep_f_4$x_seq, y = dep_f_4$preds_mean, col = 'red'), linewidth = 0.2) + 
    ggplot2::labs(col = "") + 
    ggplot2::scale_color_manual(values = c("red"), labels = c("True Function"))

gg_cond_rf2 <- ggarrange(ggdep1, ggdep2, ggdep3, ggdep4,
    ncol = 2, nrow = 2, common.legend = T, legend = 'bottom')
gg_cond_rf2


beta_strength <- 1
delta_strength <- 1
gamma_strength <- 1
alpha_1_strength <- 1
alpha_2_strength <- 1
alpha_3_strength <- 0


DiagrammeR::grViz("
digraph {
  graph []
  node [shape = plaintext]
    A [label = 'Smoking']
    Y [label = 'Lung cancer']
    C [label = 'Age']
  edge []
    A->Y
    C->A
    C->Y
  { rank = same; A; Y }
}
")


library(igraph)

nodes <- data.frame(name = c('X', 'H', 'Y', 'A'), 
                    size = c(30, 30, 12, 60), 
                    color = c('#2ad4f1', 'white', '#32f77d', '#df6c6c'),
                    label.color = c('black', 'black', 'black', 'black'), 
                    label.cex = 1.5)

edges <- data.frame(from = c('X', 'H', 'H', 'A', 'A', 'A'), 
                    to = c('Y', 'Y', 'X', 'X', 'H', 'Y'), 
                    width = c(1, 10, 1, 1, 1, 1), 
                    label = c('beta\n', 'delta\n', 'gamma\n', 'alpha 2\n', 'alpha 1\n', 'alpha 3\n'),
                    color = c('black', 'black', 'black', 'black', 'black', 'black'),
                    arrow.size = 1, 
                    label.color = c('black', 'black', 'black', 'black', 'black', 'black'),
                    label.cex = 1.5, 
                    label.dist = 1.5)

g <- graph_from_data_frame(edges, directed = T, vertices = nodes)



set.seed(6)
coords <- layout.reingold.tilford(barabasi.game(4))
plot(g, layout = coords)
