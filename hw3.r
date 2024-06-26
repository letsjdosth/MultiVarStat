data(USArrests)
?USArrests
# A data frame with 50 observations on 4 variables.
# [,1]	Murder	numeric	Murder arrests (per 100,000)
# [,2]	Assault	numeric	Assault arrests (per 100,000)
# [,3]	UrbanPop	numeric	Percent urban population
# [,4]	Rape	numeric	Rape arrests (per 100,000)

# ============
library(mclust)
mclust_bic = mclustBIC(USArrests)
mclust_fit = Mclust(data=USArrests, x=mclust_bic)
summary(mclust_fit)
names(mclust_fit)
mclust_fit$modelName
mclust_fit$G
mclust_fit$BIC
mclust_fit$icl
mclust_fit$z
mclust_fit$classification
mclust_fit$uncertainty


# ============
library(stats)
library(cluster)
kmeans_fit = kmeans(USArrests, centers=3)
kmeans_fit$cluster

# ============

scaled_USArrests = scale(USArrests, center = FALSE, scale = apply(USArrests, 2, sd, na.rm = TRUE))
kmeans_fit2 = kmeans(scaled_USArrests, centers=3)
kmeans_fit2$cluster


silhouette_score <- function(k){
  km = kmeans(scaled_USArrests, centers = k)
  sil = silhouette(km$cluster, dist(scaled_USArrests))
  mean(sil[, 3])
}
k = 2:10
avg_sil = sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#2 is the best


mclust_bic2 = mclustBIC(scaled_USArrests) #3 is the best
mclust_fit2 = Mclust(data=scaled_USArrests, x=mclust_bic2)
mclust_fit2$modelName
mclust_fit2$G
mclust_fit2$BIC
mclust_fit2$icl
mclust_fit2$z
mclust_fit2$classification
mclust_fit2$uncertainty


as.vector(kmeans_fit$cluster)
as.vector(mclust_fit$classification)
as.vector(kmeans_fit2$cluster)
as.vector(mclust_fit2$classification)


kmeans_fit3 = kmeans(scaled_USArrests, centers=2)
kmeans_fit3$cluster

# =========
# dendrogram
dist = dist(scaled_USArrests, diag=TRUE)
hc_fit = hclust(dist, method='complete')
names(hc_fit)
plot(hc_fit)
abline(h=hc_fit$height[length(hc_fit$height)-1], col='red')

dend_fit = as.dendrogram(hc_fit)
plot(cut(dend_fit,length(hc_fit$height)-1)$lower[[1]])
plot(cut(dend_fit,length(hc_fit$height)-1)$lower[[2]])
superclasses_hc_fit <- cutree(hc_fit, 3)
superclasses_hc_fit


# =========
library(aweSOM)
set.seed(20240610)
som_init = somInit(scaled_USArrests, 2, 2)
som_fit = kohonen::som(scaled_USArrests, grid = kohonen::somgrid(2, 2, "rectangular"), 
                rlen = 100, alpha = c(0.05, 0.01), radius = c(2.65,-2.65), 
                dist.fcts = "sumofsquares", init = som_init)
somQuality(som_fit, scaled_USArrests)
names(som_fit)
som_fit$unit.classif


superclust_pam = cluster::pam(som_fit$codes[[1]], 3)
superclasses_pam = superclust_pam$clustering

superclust_hclust = hclust(dist(som_fit$codes[[1]]), "complete")
superclasses_hclust = cutree(superclust_hclust, 3)

scaled_USArrests
aweSOMplot(som = som_fit, type = "Barplot", data = scaled_USArrests, 
           variables = c("Murder", "Assault", "UrbanPop", "Rape"), 
           superclass = superclasses_hclust)

