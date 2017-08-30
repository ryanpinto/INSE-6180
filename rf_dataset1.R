library(readr)
data <- read_csv("C:/Users/ryanp/Desktop/brst.csv")
# dat2 <- data.frame(lapply(data, function(x) as.integer(as.character(x))))
str(data)
attach(data)
data$benormal= as.factor(benormal)
table(data$benormal)
set.seed(123)
sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
ind=sample(2,nrow(data),replace=TRUE,prob=c(0.7,0.3))
train=data[ind==1,]
test=data[ind==2,]
library(randomForest)
set.seed(222)
rf=randomForest(benormal~.,data=train, ntree=700)
print(rf)


library(caret)
p1=predict(rf,test)
confusionMatrix(p1,test$benormal)

plot(rf)

hist(treesize(rf),main="no. of nodes for trees",col="green")

varImpPlot(rf, sort=T, n.var=10, main="Top 10 variables")

importance(rf)
varUsed(rf)

getTree(rf,1,labelVar=TRUE)

