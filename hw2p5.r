library(bootstrap)
data(scor)
?scor
scor$mec
X_mat = scor[c("mec","vec")]
Y_mat = scor[c("alg","ana","sta")]
dim(X_mat) #88,2
dim(Y_mat) #88,3
XY_full_cor = cor(cbind(X_mat, Y_mat))
XY_full_cor
heatmap(XY_full_cor, Rowv = NA, Colv = NA)

library(CCA)
?cancor
cca_fit = cancor(X_mat, Y_mat)
names(cca_fit)
cca_fit$cor #eigenval. of R. cor(xi_j, omega_j)
cca_fit$xcoef #[,j] = g_j
cca_fit$ycoef #[,j] = h_j
cca_fit$xcenter #sample mean
cca_fit$ycenter
xi_1 = as.matrix(X_mat) %*% cca_fit$xcoef[,1] #88,1
xi_2 = as.matrix(X_mat) %*% cca_fit$xcoef[,2] #88,1
omega_1 = as.matrix(Y_mat) %*% cca_fit$ycoef[,1] #88,1
omega_2 = as.matrix(Y_mat) %*% cca_fit$ycoef[,2] #88,1

cor(xi_1, omega_1) #coincide with cca_fit$cor
cor(xi_2, omega_2) #coincide with cca_fit$cor
cor(xi_1, omega_2) #0
cor(xi_2, omega_1) #0 (good)

#choose t=2
ccvar = cbind(as.matrix(X_mat) %*% cca_fit$xcoef[,1:2], as.matrix(Y_mat) %*% cca_fit$ycoef[,1:2])
cor_ccvar = cor(ccvar)
dim(cor_ccvar)
rownames(cor_ccvar) = c("xi_1", "xi_2", "omega_1", "omega_2")
colnames(cor_ccvar) = c("xi_1", "xi_2", "omega_1", "omega_2")

round(cor_ccvar,6)
heatmap(cor_ccvar, Rowv = NA, Colv = NA)

plot(xi_1, omega_1)
plot(xi_2, omega_2)
plot(xi_1, omega_2)
plot(xi_2, omega_1)


cor_XYccvar = cor(cbind(X_mat, Y_mat), ccvar)
colnames(cor_XYccvar) = c("xi_1"," xi_2", "omega_1", "omega_2")
round(cor_XYccvar,6)
heatmap(cor_XYccvar, Rowv = NA, Colv = NA)
