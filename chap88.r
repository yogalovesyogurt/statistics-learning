library(tree)
library(ISLR)
attach(Carseats)
#show(Carseats)
set.seed(1)
train=sample(1:nrow(Carseats),nrow(Carseats)/2)
tree.Carseats=tree(Sales~.,Carseats,subset=train)
summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats,pretty = 0)

Carseats.test=Carseats[-train,]
tree.pred=predict(tree.Carseats,Carseats.test)
C.test=Carseats[-train,"Sales"]
plot(tree.pred,C.test)
abline(0,1)
mean((tree.pred-C.test)^2)

cv.Carseat=cv.tree(tree.Carseats)
summary(cv.Carseat)
plot(cv.Carseat$size,cv.Carseat$dev,type = 'b')
prune.Carseat=prune.tree(tree.Carseats,best = 10)
summary(prune.Carseat)
plot(prune.Carseat)
text(prune.Carseat,pretty = 0)
mean((predict(prune.Carseat,Carseats.test)-C.test)^2)

library(randomForest)
set.seed(1)
bag.Carseats=randomForest(Sales~.,data = Carseats,subset = train,mtry=11,importance=TRUE)
bag.Carseats
yhat.bag=predict(bag.Carseats,newdata = Carseats[-train,])
plot(yhat.bag,C.test)
abline(0,1)
mean((yhat.bag-C.test)^2)
importance(bag.Carseats)

rf.Carseats=randomForest(Sales~.,data = Carseats,subset = train,mtry=5,importance=TRUE)
rf.Carseats
yhat.rf=predict(rf.Carseats,newdata = Carseats[-train,])
plot(yhat.rf,C.test)
abline(0,1)
mean((yhat.rf-C.test)^2)
importance(rf.Carseats)

varImpPlot(bag.Carseats)
varImpPlot(rf.Carseats)
