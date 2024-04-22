rm(list=ls())
library(MASS)
data(Boston)
# head(Boston)
# ?Boston #medv: response
# Boston$chas = as.factor(Boston$chas)
# Boston$rad = as.factor(Boston$rad)

data_X = model.matrix(medv~., Boston)[,-1]
data_y = Boston$medv

set.seed(20240419)


#regular least squares
ls_fit = lm(medv~., data=Boston)
round(coef(ls_fit),3)
plot(1:506, ls_fit$residuals, xlab='idx', ylab='residual')
abline(0,0)

ls_num_cv_iter = 100
ls_error_vec = rep(NA, ls_num_cv_iter)
for(k in 1:ls_num_cv_iter){
    train_data_size = 450 #among 506
    train_data_indicator = sample(c(rep(TRUE,train_data_size), rep(FALSE,nrow(Boston)-train_data_size)))
    test_data_indicator = !(train_data_indicator)
    ls_fit_full = lm(medv~., data=Boston[train_data_indicator,])
    pred = predict(ls_fit_full, as.data.frame(data_X[test_data_indicator,]))
    ls_error_vec[k] = mean((as.vector(pred) - data_y[test_data_indicator])^2)
}
mean(ls_error_vec)
sd(ls_error_vec)


#best subset (say 'bs')
library(leaps)
bs_fit_full = regsubsets(medv ~ ., Boston, nvmax=13)
summary(bs_fit_full)

#CV (not k-fold, but randomly select training/test dataset for each iteration)

bs_num_cv_iter = 100
bs_val_errors_mat = matrix(NA, bs_num_cv_iter, 13)
for(k in 1:bs_num_cv_iter){
    train_data_size = 450 #among 506
    train_data_indicator = sample(c(rep(TRUE,train_data_size), rep(FALSE,nrow(Boston)-train_data_size)))
    test_data_indicator = !(train_data_indicator)
    bs_fit = regsubsets(medv ~ ., Boston[train_data_indicator,], nvmax=13)
    test_data_X = model.matrix(medv~., data=Boston[test_data_indicator,])
    val_errors = rep(NA, 13)
    for(i in 1:13){
        coef_i = coef(bs_fit, id=i)
        pred = test_data_X[,names(coef_i)] %*% coef_i
        val_errors[i] = mean((Boston$medv[test_data_indicator]-pred)^2)
    }
    bs_val_errors_mat[k,] = val_errors
}
bs_cv_test_err_est = colMeans(bs_val_errors_mat)
bs_cv_test_err_sd = apply(bs_val_errors_mat, 2, sd)

which.min(bs_cv_test_err_est) #11
plot(1:13, bs_cv_test_err_est, col='red', ylim=c(20,40), type='l', main="test error, with 1sd")
points(1:13, bs_cv_test_err_est, col='red')
points(1:13, bs_cv_test_err_est+bs_cv_test_err_sd, col='black')
points(1:13, bs_cv_test_err_est-bs_cv_test_err_sd, col='black')
#use 5 or 11
round(coef(bs_fit_full, id=11),3)
bs_cv_test_err_est[11] #test err est
sd(bs_val_errors_mat[,11])

#ridge
library(glmnet)
data_X = model.matrix(medv~., Boston)[,-1]
data_y = Boston$medv
# ?glmnet
ridge_fit = glmnet(data_X, data_y, family="gaussian", standardize=TRUE, alpha=0, lambda=c(10000, 1000, 100, 10, 1, 0.1)) 
#use a decreasing sequence of lambda
sqrt(sum(coef(ridge_fit)[-1,1]^2)) 
#return to the original scale(covered back from standardization)
sqrt(sum(coef(ridge_fit)[-1,2]^2))
sqrt(sum(coef(ridge_fit)[-1,3]^2))
sqrt(sum(coef(ridge_fit)[-1,4]^2))
sqrt(sum(coef(ridge_fit)[-1,5]^2))
sqrt(sum(coef(ridge_fit)[-1,6]^2))

set.seed(20240419)
lambda_grid = 10^seq(10, -3, length=100)
ridge_num_cv_iter = 100
ridge_val_errors_mat = matrix(NA, ridge_num_cv_iter, length(lambda_grid))
for(k in 1:ridge_num_cv_iter){
    train_data_size = 450 #among 506
    train_data_indicator = sample(c(rep(TRUE,train_data_size), rep(FALSE,nrow(Boston)-train_data_size)))
    test_data_indicator = !(train_data_indicator)
    ridge_fit = glmnet(data_X[train_data_indicator,], data_y[train_data_indicator], 
                        family="gaussian", standardize=TRUE, alpha=0, lambda=lambda_grid) 
    val_errors = rep(NA, length(lambda_grid))
    for(i in 1:length(lambda_grid)){
        ridge_pred = predict(ridge_fit, s=lambda_grid[i], newx=data_X[test_data_indicator,])
        val_errors[i] = mean((data_y[test_data_indicator]-ridge_pred)^2)
    }
    ridge_val_errors_mat[k,] = val_errors
}
ridge_cv_test_err_est = colMeans(ridge_val_errors_mat)
which.min(ridge_cv_test_err_est) #83
lambda_grid[which.min(ridge_cv_test_err_est)] #0.0001592283
ridge_cv_test_err_est[which.min(ridge_cv_test_err_est)] #23.09675
sd(ridge_val_errors_mat[,which.min(ridge_cv_test_err_est)])


##select
ridge_fit = glmnet(data_X, data_y, family="gaussian", standardize=TRUE, alpha=0, lambda=c(0.1, 0.001, 0.0001592283, 0.0000001)) 
round(coef(ridge_fit)[,3],3)


#lasso
library(glmnet)
data_X = model.matrix(medv~., Boston)[,-1]
data_y = Boston$medv
lasso_fit = glmnet(data_X, data_y, family="gaussian", standardize=TRUE, alpha=1, lambda=c(100, 10, 1, 0.1, 0.01)) 
#use a decreasing sequence of lambda
sum(abs(coef(lasso_fit)[-1,1]))
#return to the original scale(covered back from standardization)
sum(abs(coef(lasso_fit)[-1,2]))
sum(abs(coef(lasso_fit)[-1,3]))
sum(abs(coef(lasso_fit)[-1,4]))
sum(abs(coef(lasso_fit)[-1,5]))

set.seed(20240419)
lambda_grid = 10^seq(2, -5, length=100)
#CV (problem 5)
lasso_num_cv_iter = 100
lasso_val_errors_mat = matrix(NA, lasso_num_cv_iter, length(lambda_grid))
for(k in 1:lasso_num_cv_iter){
    train_data_size = 450 #among 506 #bootstrap: set it nrow(Boston), or 506
    train_data_indicator = sample(c(rep(TRUE,train_data_size), rep(FALSE,nrow(Boston)-train_data_size))) #bootstrap: sample it with replacement
    test_data_indicator = !(train_data_indicator)
    if(sum(test_data_indicator)==0){
        next
    }
    lasso_fit = glmnet(data_X[train_data_indicator,], data_y[train_data_indicator], 
                        family="gaussian", standardize=TRUE, alpha=1, lambda=lambda_grid) 
    val_errors = rep(NA, length(lambda_grid))
    for(i in 1:length(lambda_grid)){
        lasso_pred = predict(lasso_fit, s=lambda_grid[i], newx=data_X[test_data_indicator,])
        val_errors[i] = mean((data_y[test_data_indicator]-lasso_pred)^2)
    }
    lasso_val_errors_mat[k,] = val_errors
}
lasso_cv_test_err_est = colMeans(lasso_val_errors_mat)
which.min(lasso_cv_test_err_est) #51(random CV)
lambda_grid[which.min(lasso_cv_test_err_est)] #0.02915053
lasso_cv_test_err_est[which.min(lasso_cv_test_err_est)] #23.09(random CV)
sd(lasso_val_errors_mat[,which.min(lasso_cv_test_err_est)])

##select
lasso_fit = glmnet(data_X, data_y, family="gaussian", standardize=TRUE, alpha=1, lambda=c(1, 0.1, 0.02915053, 0.01)) 
round(coef(lasso_fit)[,3],3)
