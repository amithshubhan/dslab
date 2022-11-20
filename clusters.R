install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
library("ggplot2")
library("dplyr")
library("ggfortify")
summary(iris)
head(iris)
data <- select(iris, c(1:4))
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
kmean <- kmeans(data, 2)
kmean$centers
autoplot(kmean, data, frame = TRUE)
kmean <- kmeans(data, 3)
kmean$centers
