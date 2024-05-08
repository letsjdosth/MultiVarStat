set.seed(20240508)
x1 = rnorm(200)
x2 = x1 + 0.001*rnorm(200)
x3 = 10*rnorm(200)

#all are mean-zero

# non-standardized ===============================
mat_X = as.matrix(cbind(x1, x2, x3)) #(200, 3)
mat_S = t(mat_X) %*% mat_X/200
eigen_of_S = eigen(mat_S, TRUE)
eigen_of_S$values
eigen_of_S$vectors

# Or, using prcomp (r std)
pr_out = prcomp(mat_X, scale=FALSE)
names(pr_out)
pr_out$center
pr_out$scale
pr_out$sdev^2
pr_out$rotation #loadings(eigenvecs)
dim(pr_out$x) #200, 3, score vector.

mat_X[1,]
(pr_out$rotation %*% pr_out$x[1,]) + pr_out$center #reconstructed

#scree plot
pr_out_var = pr_out$sdev^2 #eigenvals
pve = pr_out_var / sum(pr_out_var)
pve #total variance explained by each principal component
par(mfrow=c(1,2))
plot(pve,
    xlab="principal component",
    ylab="proportion of variance explained",
    ylim=c(0,1), type="b")
plot(cumsum(pve),
    xlab="principal component",
    ylab="cumulative proportion of variance explained",
    ylim=c(0,1), type="b")
#or
par(mfrow=c(1,1))
screeplot(pr_out, type="lines")

#biplot
par(mfrow=c(1,2))
pr_out$rotation = -pr_out$rotation
pr_out$x = -pr_out$x
biplot(pr_out, scale=0, pc.biplot=TRUE)
biplot(pr_out, scale=1/2, pc.biplot=TRUE)





# standardized ===============================
mat_stdX = as.matrix(cbind(x1/sd(x1), x2/sd(x2), x3/sd(x3)))

mat_stdS = t(mat_stdX) %*% mat_stdX/200
eigen_of_stdS = eigen(mat_stdS)
eigen_of_stdS$values
eigen_of_stdS$vectors

# Or, using prcomp (r std)
pr_out_std = prcomp(mat_X, scale=TRUE) #<<<<
pr_out_std$center
pr_out_std$scale
pr_out_std$rotation #loadings
pr_out_std$sdev^2
dim(pr_out_std$x) #200, 3, score vector.

mat_X[1,]
(pr_out_std$rotation %*% pr_out_std$x[1,])*pr_out_std$scale + pr_out_std$center #reconstructed

#scree plot
pr_out_std_var = pr_out_std$sdev^2
pve_std = pr_out_std_var / sum(pr_out_std_var)
pve_std #total variance explained by each principal component
par(mfrow=c(1,2))
plot(pve_std,
    xlab="principal component",
    ylab="proportion of variance explained",
    ylim=c(0,1), type="b")
plot(cumsum(pve_std),
    xlab="principal component",
    ylab="cumulative proportion of variance explained",
    ylim=c(0,1), type="b")
#or
par(mfrow=c(1,1))
screeplot(pr_out_std, type="lines")

#biplot
par(mfrow=c(1,2))
pr_out_std$rotation = -pr_out_std$rotation
pr_out_std$x = -pr_out_std$x
biplot(pr_out_std, scale=0, pc.biplot=TRUE)
biplot(pr_out_std, scale=1/2, pc.biplot=TRUE)
