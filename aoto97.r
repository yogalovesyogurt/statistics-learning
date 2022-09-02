D=read.table("E:\\统计\\统计学习\\R_book_data\\auto.data",header=T,na.strings="?")
D=na.omit(D)
attach(D)
med=median(mpg)
oil=ifelse(mpg>=med,1,0)

Data=data.frame(D[1:8],oil)
Data1 = data.frame(D[2:9],oil=as.factor(oil))
set.seed(1)
train=sample(nrow(Data1),nrow(Data1)/2)

library(e1071)
svmclass=svm(oil~.,data=Data1,kernel='linear',cost=0.01,scale=FALSE)
summary(svmclass)             
svmclass$index

set.seed(1)
tune.out=tune(svm,oil~.,data = Data,kernel='linear',ranges = list(cost=c(0.01,0.1,1,5,10,100)))
#tune.out=tune.svm(oil~.,data = Data,kernel='linear',cost=c(0.01,0.1,1,5,10,100))
summary(tune.out)
bestmod=tune.out$best.model

summary(tune(svm,oil~.,data = Data1,kernel='radial',ranges = list(cost=c(0.01,0.1,1,5,10,100))))

summary(tune(svm,oil~.,data = Data1,kernel='polynomial',ranges = list(cost=c(0.01,0.1,1,5,10,100))))

svmrad=svm(oil~.,data=Data1,kernel='radial',cost=100,scale=FALSE)
svmpoly=svm(oil~.,data=Data1,kernel='polynomial',cost=100,scale=FALSE)


plot(svmclass,Data1,weight~acceleration)

plot(svmrad,Data1,weight~acceleration)

plot(svmpoly,Data1,horsepower~acceleration)
