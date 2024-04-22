rm(list=ls())
library(MASS)
data(Boston)
# head(Boston)
# ?Boston #medv: response
# Boston$chas = as.factor(Boston$chas)
# Boston$rad = as.factor(Boston$rad)

data_X = model.matrix(medv~., Boston)[,-1]
data_y = Boston$medv

set.seed(20240420)


library(monomvn)

bridge_fit = bridge(data_X, data_y, T=10000, RJ=FALSE, lambda2=0.0001592283, normalize=FALSE)
mean(bridge_fit$mu)
round(colMeans(bridge_fit$beta),3)

cv_iter = 50
cv_err_vec = rep(NA, cv_iter)
for(k in 1:cv_iter){
    train_data_size = 450 #among 506
    train_data_indicator = sample(c(rep(TRUE,train_data_size), rep(FALSE,nrow(Boston)-train_data_size)))
    test_data_indicator = !(train_data_indicator)
    bridge_fit = bridge(data_X[train_data_indicator,], data_y[train_data_indicator], 
                            T=10000, RJ=FALSE, lambda2=0.0001592283, normalize=FALSE)
    pred = data_X[test_data_indicator,] %*% as.vector(colMeans(bridge_fit$beta)) + mean(bridge_fit$mu)
    cv_err_vec[k] = mean((data_y[test_data_indicator]-pred)^2)
}
mean(cv_err_vec)
sd(cv_err_vec)



blasso_fit = blasso(data_X, data_y, T=10000, RJ=FALSE, lambda2=0.02915053, normalize=FALSE)
mean(blasso_fit$mu)
round(colMeans(blasso_fit$beta),3)

cv_iter = 50
cv_err_vec = rep(NA, cv_iter)
for(k in 1:cv_iter){
    train_data_size = 450 #among 506
    train_data_indicator = sample(c(rep(TRUE,train_data_size), rep(FALSE,nrow(Boston)-train_data_size)))
    test_data_indicator = !(train_data_indicator)
    blasso_fit = blasso(data_X[train_data_indicator,], data_y[train_data_indicator], 
                            T=10000, RJ=FALSE, lambda2=0.02915053, normalize=FALSE)
    pred = data_X[test_data_indicator,] %*% as.vector(colMeans(blasso_fit$beta)) + mean(blasso_fit$mu)
    cv_err_vec[k] = mean((data_y[test_data_indicator]-pred)^2)
}
mean(cv_err_vec)
sd(cv_err_vec)