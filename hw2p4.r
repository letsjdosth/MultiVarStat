library(PPCI)
data(pendigits)
?pendigits
dim(pendigits$x)
length(pendigits$c)

pr_out = prcomp(pendigits$x, scale=FALSE)
pr_out$center
pr_out$scale
pr_out$sdev^2
pr_out$rotation #loadings(eigenvecs)
dim(pr_out$x) #200, 3, score vector.

cumsum(pr_out$sdev^2/sum(pr_out$sdev^2))

par(mfrow=c(1,1))
screeplot(pr_out, type="lines") #4(elbow), 8(upper 0.9)
biplot(pr_out, scale=1/2, pc.biplot=TRUE)

score_pca = function(rank_){
    pr_out1 = prcomp(pendigits$x, scale=FALSE, rank.=rank_)
    return(pr_out1$x)
}

reconstruction_pca = function(rank_){
    pr_out2 = prcomp(pendigits$x, scale=FALSE, rank.=rank_)
    pr_out2_reconst = pr_out2$x %*% t(pr_out2$rotation)   #reconstructed
    for(i in 1:dim(recon_x)[1]) pr_out2_reconst[i,] = pr_out2_reconst[i,] + pr_out$center
    return(pr_out2_reconst)
}
score2 = score_pca(2)
plot(score2, col=factor(pendigits$c))
recon2 = reconstruction_pca(2)
pairs(recon2, col=factor(pendigits$c))

score3 = score_pca(3)
pairs(score3, col=factor(pendigits$c))
recon3 = reconstruction_pca(3)
pairs(recon3, col=factor(pendigits$c))


#elbow
score4 = score_pca(4)
pairs(score4, col=factor(pendigits$c))

#90% var
score8 = score_pca(8)
pairs(score8, col=factor(pendigits$c))
