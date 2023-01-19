
#libraries to use

library(caTools)
library(ggplot2)
library(dplyr)
library(Metrics)
library(ggfortify)
library(dplyr)
library(arules)
library(arulesViz)
library(randomForest)


###################################################

#kmeans

data(iris)
data = iris

data_x = data[,-5]

wssplot = function(data,nc = 7,seed = 1234){
  
  wss = (nrow(data) - 1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] = sum(kmeans(data,centers = i)$withinss)
    
  }
  plot(1:nc,wss,type="b",xlab="no of clusters",ylab="wss")
  wss
  
}

wssplot(data_x)

model = kmeans(data_x,3)

cm<-table(iris[,5],model$cluster)

autoplot(model,data_x,frame=TRUE)
clusplot(iris,model$cluster)


########################################

##hclust

data(mtcars)
data = mtcars


d = dist(mtcars,method="euclidian")
model = hclust(d,method = "average")
plot(model)

rect.hclust(model,k=3)
fit <- cutree(model,k=2)
print(fit)

#######################################

##apriori

data(Groceries)
data = Groceries

summary(data)

model = apriori(data,parameter=list(support = 0.0004,confidence=0.2))

plot(model,method="grouped")

itemFrequencyPlot(data,topN=10)

######################################

##randomForest

data(iris)

data = iris

split<-sample.split(iris,SplitRatio=0.7)

train<- subset(iris,split==TRUE)
test<-subset(iris,split==FALSE)


data_x = iris[,-5]
data_y = iris[,5]


train_x = train[,-5] 
train_y = train[,5]


test_x = test[,-5]
test_y = test[,5]

model = randomForest(train_x,train_y,ntree= 500)

plot(model)

varImpPlot(model)

importance(model)

#######################################


##linear regression

data = read.csv('linear.csv')
head(data)



split <- sample.split(data,SplitRatio=0.7)
training<-subset(data,split==TRUE)
test <- subset(data,split==FALSE)

summary(data)

head(training)
training$w

model = lm(formula=w ~ h ,data = training)


mode = predict(model,test[1])

print(test[2])

ans = rmse(mode,test$w)
print(ans)


train_pred = predict(model,training[1])

ggplot() + geom_point(aes(x = training$h,y = training$w)) + geom_line(aes(x = training$h,y = train_pred))


######################################################


##logistic regression


library(ROCR)

head(mtcars)
split<- sample.split(mtcars$vs,SplitRatio = 0.7)#vs is target variable
train<-subset(mtcars,split==TRUE)
test<-subset(mtcars,split==FALSE)

model = glm(formula = vs~wt+disp,data = train,family="binomial"(link = "logit"))

preds = predict(model,test)

preds = ifelse(preds > 0.5,1,0)

Roc_pred = prediction(preds,test$vs)
roc = performance(Roc_pred,"tpr","fpr")
plot(roc)

auc = performance(Roc_pred,"auc")
print(auc@y.values[[1]])


##################################################
