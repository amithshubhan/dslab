# Library required for fviz_cluster function
install.packages("factoextra")
library(factoextra)
 
# Loading dataset
df <- mtcars
 
# Omitting any NA values
df <- na.omit(df)
 
# Scaling dataset
df <- scale(df)
 
# output to be present as PNG file
png(file = "KMeansExample.png")
 
km <- kmeans(df, centers = 4, nstart = 25)
 
# Visualize the clusters
fviz_cluster(km, data = df)
 
# saving the file
dev.off()
 
# output to be present as PNG file
png(file = "KMeansExample2.png")
 
km <- kmeans(df, centers = 5, nstart = 25)
 
# Visualize the clusters
fviz_cluster(km, data = df)
 
# saving the file
dev.off()


#here

library(ggplot2)
library(ggfortify)
library(dplyr)

data = select(iris,c(1,4))
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

kmean = kmeans(data,2)
kmean$centers
autoplot(kmean,data,frame=TRUE)



#here 22



install.packages("factoextra") 
 install.packages("cluster") 
  
 library(factoextra) 
 library(cluster) 
  
 df <- USArrests 
  
 df <- na.omit(df) 
  
 head(df) 
 m <- c( "average", "single", "complete", "ward") 
 names(m) <- c( "average", "single", "complete", "ward") 
  
 #function to compute agglomerative coefficient 
 ac <- function(x) { 
   agnes(df, method = x)$ac 
 } 
  
 #calculate agglomerative coefficient for each clustering linkage method 
 sapply(m, ac) 
 #perform hierarchical clustering using Ward's minimum variance 
 clust <- agnes(df, method = "ward") 
  
 #produce dendrogram 
 pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 
  
 #calculate gap statistic for each number of clusters (up to 10 clusters) 
 gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50) 
  
 #produce plot of clusters vs. gap statistic 
 fviz_gap_stat(gap_stat) 
  
 #compute distance matrix 
 d <- dist(df, method = "euclidean") 
  
 #perform hierarchical clustering using Ward's method 
 final_clust <- hclust(d, method = "ward.D2" ) 
  
 #cut the dendrogram into 4 clusters 
 groups <- cutree(final_clust, k=4) 
  
 #find number of observations in each cluster 
 table(groups) 
  
 #append cluster labels to original data 
 final_data <- cbind(USArrests, cluster = groups) 
  
 #display first six rows of final data 
 head(final_data) 
  
 aggregate(final_data, by=list(cluster=final_data$cluster), mean)





