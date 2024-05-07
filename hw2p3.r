x1 = rnorm(200)
x2 = x1 + 0.001*rnorm(200)
x3 = 10*rnorm(200)

#all are mean-zero

# non-standardized
mat_X = as.matrix(cbind(x1, x2, x3))
mat_S = t(mat_X) %*% mat_X
eigen_of_S = eigen(mat_S, TRUE)
eigen_of_S$values
eigen_of_S$vectors
#scree plot
#biplot

#stndardized
mat_stdX = as.matrix(cbind(x1/sd(x1), x2/sd(x2), x3/sd(x3)))
mat_stdS = t(mat_stdX) %*% mat_stdX
eigen_of_stdS = eigen(mat_stdS, TRUE)
eigen_of_stdS$values
eigen_of_stdS$vectors
