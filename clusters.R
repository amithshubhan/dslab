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

