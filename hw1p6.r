rm(list=ls())
library(MASS)
library(glmnet)

data(Boston)
# head(Boston)
# ?Boston #medv: response
# Boston$chas = as.factor(Boston$chas)
# Boston$rad = as.factor(Boston$rad)
data_X = model.matrix(medv~., Boston)[,-1]
data_y = Boston$medv

set.seed(20240420)

#k-fold (problem 6)
total_k = 506 #'k' fold

lambda_grid = 10^seq(1, -5, length=100)
lasso_val_errors_mat = matrix(NA, total_k*50, length(lambda_grid))
lasso_val_errors_mat_at_batch = matrix(NA, 50, length(lambda_grid))

for(rr in 1:50){
    fold_indicator = sample(rep(1:total_k, length=nrow(Boston)), replace=FALSE)
    sum_val_errors_at_batch = rep(0, length(lambda_grid))
    for(k in 1:total_k){ #total_k
        train_data_indicator = (fold_indicator!=k)
        test_data_indicator = (fold_indicator==k)
        lasso_fit = glmnet(data_X[train_data_indicator,], data_y[train_data_indicator], 
                            family="gaussian", standardize=TRUE, alpha=1, lambda=lambda_grid) 
        val_errors = rep(NA, length(lambda_grid))
        for(i in 1:length(lambda_grid)){
            lasso_pred = predict(lasso_fit, s=lambda_grid[i], newx=data_X[test_data_indicator,])
            val_errors[i] = mean((data_y[test_data_indicator]-lasso_pred)^2)
        }
        sum_val_errors_at_batch = sum_val_errors_at_batch + val_errors
        lasso_val_errors_mat[((rr-1)*total_k+k),] = val_errors
    }
    lasso_val_errors_mat_at_batch[rr,] = sum_val_errors_at_batch/total_k
}
# lasso_val_errors_mat_at_batch: mild lines
lasso_cv_test_err_est = colMeans(lasso_val_errors_mat, na.rm=TRUE) #bold line
which.min(lasso_cv_test_err_est) #52(10fold, 50rep)
lambda_grid[which.min(lasso_cv_test_err_est)]
lasso_cv_test_err_est[which.min(lasso_cv_test_err_est)] #23.54373(10fold, 50rep)

plot(lambda_grid, lasso_cv_test_err_est, type='l', col='red')
for(i in 1:50) lines(lambda_grid, lasso_val_errors_mat_at_batch[i,])
lines(lambda_grid, lasso_cv_test_err_est, col='red')

plot(lambda_grid[35:50], lasso_cv_test_err_est[35:50], type='l', col='red', ylim=c(10,25))
for(i in 1:50) lines(lambda_grid[35:50], lasso_val_errors_mat_at_batch[i,35:50])
lines(lambda_grid[35:50], lasso_cv_test_err_est[35:50], col='red')



#bootstrap
lambda_grid = 10^seq(1, -5, length=100)
lasso_val_errors_mat = matrix(NA, boot_iter*50, length(lambda_grid))
lasso_val_errors_mat_at_batch = matrix(NA, 50, length(lambda_grid))
boot_iter = 10
for(rr in 1:50){
    sum_val_errors_at_batch = rep(0, length(lambda_grid))
    for(k in 1:boot_iter){
        train_data_index = sample(1:506, replace=TRUE)
        test_data_indicator = rep(TRUE, 506)
        for(i in train_data_index) test_data_indicator[i] = FALSE
        if(sum(test_data_indicator)==0){
            next
        }
        lasso_fit = glmnet(data_X[train_data_index,], data_y[train_data_index], 
                            family="gaussian", standardize=TRUE, alpha=1, lambda=lambda_grid) 
        val_errors = rep(NA, length(lambda_grid))
        for(i in 1:length(lambda_grid)){
            lasso_pred = predict(lasso_fit, s=lambda_grid[i], newx=data_X[test_data_indicator,])
            val_errors[i] = mean((data_y[test_data_indicator]-lasso_pred)^2)
        }
        lasso_val_errors_mat[(rr-1)*boot_iter+k,] = val_errors
        sum_val_errors_at_batch = sum_val_errors_at_batch + val_errors
    }
    lasso_val_errors_mat_at_batch[rr,] = sum_val_errors_at_batch / boot_iter
}
lasso_cv_test_err_est = colMeans(lasso_val_errors_mat_at_batch)
plot(lambda_grid, lasso_cv_test_err_est, type='l', col='red')
for(i in 1:boot_iter) lines(lambda_grid, lasso_val_errors_mat_at_batch[i,])
lines(lambda_grid, lasso_cv_test_err_est, col='red')

plot(lambda_grid[35:50], lasso_cv_test_err_est[35:50], type='l', col='red', ylim=c(24,26))
for(i in 1:boot_iter) lines(lambda_grid[35:50], lasso_val_errors_mat_at_batch[i,35:50])
lines(lambda_grid[35:50], lasso_cv_test_err_est[35:50], col='red')
