data(USArrests)
names(USArrests)
dim(USArrests) #50, 4
states = row.names(USArrests)
states

apply(USArrests, 2, mean)
apply(USArrests, 2, sd)

pr_out = prcomp(USArrests, scale=TRUE)
names(pr_out)
pr_out$center
pr_out$scale
pr_out$rotation #loadings
dim(pr_out$x) #50, 4, score vector.

USArrests[1,]
(pr_out$rotation %*% pr_out$x[1,]) * pr_out$scale + pr_out$center #reconstructed

biplot(pr_out, scale=0)
biplot(pr_out, scale=1/2)

# ===
# loadings and PC comps are unique up to sign
pr_out$rotation = -pr_out$rotation
pr_out$x = -pr_out$x
biplot(pr_out, scale=0)

pr_out$sdev
pr_out_var = pr_out$sdev^2
pr_out_var # eigenval

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
