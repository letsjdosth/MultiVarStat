rm(list=ls())
library(MASS)
library(glmnet)

#simulation
set.seed(20240516)

sim_function = function(p, num_nonzero_elem_of_colLambda){
    n_train_size = 200
    n_test_size = 50
    k = 5
    Sigma = diag(1/rgamma(p+1, shape=3, rate=3))
    Lambda = matrix(0, p+1, k)
    for(j in 1:k){
        col_j = rep(0, dim(Lambda)[1])
        for(i in 1:num_nonzero_elem_of_colLambda){
            val = rnorm(1, 0,1)
            col_j[i] = ifelse(val>0, val+1, val-1)
        }
        col_j = sample(col_j)
        Lambda[,j]=col_j
    }
    Omega = Lambda %*% t(Lambda) + Sigma
    z_samples = mvrnorm(n_train_size + n_test_size, mu=rep(0,p+1), Sigma=Omega)
    print(dim(z_samples))

    sim_data_list = list()
    sim_data_list$x_train_samples = z_samples[1:n_train_size, 1:p]
    sim_data_list$y_train_samples = z_samples[1:n_train_size, p+1]
    sim_data_list$x_test_samples = z_samples[(n_train_size+1):(n_train_size+n_test_size), 1:p]
    sim_data_list$y_test_samples = z_samples[(n_train_size+1):(n_train_size+n_test_size), p+1]
    sim_data_list$true_beta0 = 0
    sim_data_list$true_beta1 = t(Omega[1:p,p+1] %*% solve(Omega[1:p, 1:p]))
    sim_data_list$true_epsilon_var = Omega[p+1,p+1] - Omega[p+1,1:p] %*% solve(Omega[1:p,1:p]) %*% Omega[1:p, p+1]
    return(sim_data_list)
}

sim_data_scenario1 = sim_function(100, 2*5)
sim_data_scenario2 = sim_function(20, 2*5)
sim_data_scenario3 = sim_function(100, 100+1) #full: p+1
sim_data_scenario4 = sim_function(20, 20+1)
names(sim_data_scenario1)
dim(sim_data_scenario1$x_train_samples)
dim(sim_data_scenario1$x_test_samples)
length(sim_data_scenario1$y_train_samples)
length(sim_data_scenario1$y_test_samples)
length(sim_data_scenario1$true_beta1)
sim_data_scenario1$true_beta1

cv_y_err_vec_lm = rep(0,100)
cv_y_err_vec_ridge = rep(0,100)
cv_y_err_vec_lasso = rep(0,100)
testset_beta_err_vec_lm = rep(0,100)
testset_beta_err_vec_ridge = rep(0,100)
testset_beta_err_vec_lasso = rep(0,100)
testset_y_err_vec_lm = rep(0,100)
testset_y_err_vec_ridge = rep(0,100)
testset_y_err_vec_lasso = rep(0,100)

scenario_sim = sim_data_scenario1 # <<< choose here
#repete below, 100 times, for each scenario
for(rrr in 1:1){
    #generate data here

    #cv
    num_cv_iter = 100
    ls_error_vec = rep(NA, num_cv_iter)
    ridge_lambda_grid = 10^seq(4, -2, length=100)
    ridge_val_errors_mat = matrix(NA, num_cv_iter, length(ridge_lambda_grid))
    lasso_lambda_grid = 10^seq(2, -5, length=100)
    lasso_val_errors_mat = matrix(NA, num_cv_iter, length(ridge_lambda_grid))

    for(k in 1:num_cv_iter){
        cv_data = data.frame(x=scenario_sim$x_train_samples, y=scenario_sim$y_train_samples)
        cv_train_data_size = 160 #among 200
        cv_train_data_indicator = sample(c(rep(TRUE,cv_train_data_size), rep(FALSE,200-cv_train_data_size)))
        cv_test_data_indicator = !(cv_train_data_indicator)
        cv_data_X = scenario_sim$x_train_samples
        cv_data_y = scenario_sim$y_train_samples

        #lm
        lm_fit = lm(y~., cv_data[cv_train_data_indicator,])
        lm_pred = predict(lm_fit, cv_data[cv_test_data_indicator,])
        pred_mse = mean((as.vector(lm_pred) - scenario_sim$y_train_samples[cv_test_data_indicator])^2)
        ls_error_vec[k] = pred_mse

        #ridge
        ridge_fit = glmnet(cv_data_X[cv_train_data_indicator,], 
                            cv_data_y[cv_train_data_indicator], 
                            family="gaussian", standardize=TRUE, alpha=0, 
                            lambda=ridge_lambda_grid) 
        ridge_val_errors = rep(NA, length(ridge_lambda_grid))
        for(i in 1:length(ridge_lambda_grid)){
            ridge_pred = predict(ridge_fit, s=ridge_lambda_grid[i], newx=cv_data_X[cv_test_data_indicator,])
            ridge_val_errors[i] = mean((cv_data_y[cv_test_data_indicator]-ridge_pred)^2)
        }
        ridge_val_errors_mat[k,] = ridge_val_errors

        #lasso
        lasso_fit = glmnet(cv_data_X[cv_train_data_indicator,], 
                            cv_data_y[cv_train_data_indicator], 
                            family="gaussian", standardize=TRUE, alpha=1, 
                            lambda=lasso_lambda_grid) 
        lasso_val_errors = rep(NA, length(lasso_lambda_grid))
        for(i in 1:length(lasso_lambda_grid)){
            lasso_pred = predict(lasso_fit, s=lasso_lambda_grid[i], newx=cv_data_X[cv_test_data_indicator,])
            lasso_val_errors[i] = mean((cv_data_y[cv_test_data_indicator]-lasso_pred)^2)
        }
        lasso_val_errors_mat[k,] = lasso_val_errors
    }

    mean(ls_error_vec)
    sd(ls_error_vec)

    ridge_cv_test_err_est = colMeans(ridge_val_errors_mat)
    ridge_cv_test_err_est
    which.min(ridge_cv_test_err_est)
    ridge_lambda_grid[which.min(ridge_cv_test_err_est)]
    ridge_cv_test_err_est[which.min(ridge_cv_test_err_est)]
    sd(ridge_val_errors_mat[,which.min(ridge_cv_test_err_est)])

    lasso_cv_test_err_est = colMeans(lasso_val_errors_mat)
    lasso_cv_test_err_est
    which.min(lasso_cv_test_err_est) #51(random CV)
    lasso_lambda_grid[which.min(lasso_cv_test_err_est)] #0.02915053
    lasso_cv_test_err_est[which.min(lasso_cv_test_err_est)] #23.09(random CV)
    sd(lasso_val_errors_mat[,which.min(lasso_cv_test_err_est)])



    #beta mse and testset performance
    training_data_frame = data.frame(x=scenario_sim$x_train_samples, 
                            y=scenario_sim$y_train_samples)
    lm_training_fit = lm(y~., data=training_data_frame)
    lm_beta_mse = mean((lm_training_fit$coefficients - c(scenario_sim$true_beta0, scenario_sim$true_beta1))^2)
    lm_beta_mse
    lm_test_pred = predict(lm_training_fit, data.frame(x=scenario_sim$x_test_samples))
    lm_test_mse = mean((lm_test_pred - scenario_sim$y_test_samples)^2)
    lm_test_mse
    sd((lm_test_pred - scenario_sim$y_test_samples)^2)


    ridge_training_fit = glmnet(scenario_sim$x_train_samples, 
                            scenario_sim$y_train_samples, 
                            family="gaussian", standardize=TRUE, alpha=0, 
                            lambda=ridge_lambda_grid[which.min(ridge_cv_test_err_est)])
    ridge_beta_mse = mean((c(ridge_training_fit$a0, as.vector(ridge_training_fit$beta)) - c(scenario_sim$true_beta0, scenario_sim$true_beta1))^2)
    ridge_beta_mse
    ridge_pred = predict(ridge_training_fit, s=ridge_lambda_grid[which.min(ridge_cv_test_err_est)], newx=scenario_sim$x_test_samples)
    ridge_test_mse = mean((ridge_pred - scenario_sim$y_test_samples)^2)
    ridge_test_mse
    sd((ridge_pred - scenario_sim$y_test_samples)^2)


    lasso_training_fit = glmnet(scenario_sim$x_train_samples, 
                            scenario_sim$y_train_samples, 
                            family="gaussian", standardize=TRUE, alpha=1, 
                            lambda=lasso_lambda_grid[which.min(lasso_cv_test_err_est)])
    lasso_beta_mse = mean((c(lasso_training_fit$a0, as.vector(lasso_training_fit$beta)) - c(scenario_sim$true_beta0, scenario_sim$true_beta1))^2)
    lasso_beta_mse
    lasso_pred = predict(lasso_training_fit, s=lasso_lambda_grid[which.min(lasso_cv_test_err_est)], newx=scenario_sim$x_test_samples)
    lasso_test_mse = mean((lasso_pred - scenario_sim$y_test_samples)^2)
    lasso_test_mse
    sd((lasso_pred - scenario_sim$y_test_samples)^2)

}