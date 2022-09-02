D=read.table("E:\\统计\\统计学习\\R_book_data\\auto.data",header=T,na.strings="?")
D=na.omit(D)
attach(D)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
Auto = data.frame(D, mpg01)
write.csv(Auto,"E:\\统计\\统计学习\\R_book_data\\mgp01.csv",row.names = FALSE)

pairs(Auto)
boxplot(mpg01 ~ cylinders, data = Auto, xlab = "cylinders", ylab = "mpg01")
boxplot(mpg01 ~ displacement, data = Auto, xlab = "displacement", ylab = "mpg01")
boxplot(mpg01 ~ horsepower, data = Auto, xlab = "horsepower", ylab = "mpg01")
boxplot(mpg01 ~ weight, data = Auto, xlab = "weight", ylab = "mpg01")
boxplot(mpg01 ~ acceleration, data = Auto, xlab = "acceleration", ylab = "mpg01")
boxplot(mpg01 ~ year, data = Auto, xlab = "year", ylab = "mpg01")
boxplot(mpg01 ~ origin, data = Auto, xlab = "origin", ylab = "mpg01")
boxplot(mpg01 ~ mpg, data = Auto, xlab = "mpg", ylab = "mpg01")

train=sort(sample(nrow(Auto), nrow(Auto)*.7))
Auto.train = Auto[train, ]
Auto.test = Auto[-train, ]
mpg01.test = mpg01[-train]
mpg01.train = mpg01[train]
write.csv(Auto.train,"E:\\统计\\统计学习\\R_book_data\\train.csv",row.names = FALSE)
write.csv(Auto.test,"E:\\统计\\统计学习\\R_book_data\\test.csv",row.names = FALSE)

library(MASS)
lda = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
lda.pred = predict(lda, Auto.test)
mean(lda.pred$class != mpg01.test)

qda = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
qda.pred = predict(qda, Auto.test)
mean(qda.pred$class != mpg01.test)

glm = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = train)
glm.probs = predict(glm, Auto.test, type = "response")
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mean(glm.pred != mpg01.test)

library(class)
train.X = cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X = cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 = mpg01[train]
set.seed(1)
# KNN(k=1)
knn.pred1 = knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred1 != mpg01.test)

# KNN(k=10)
knn.pred2 = knn(train.X, test.X, train.mpg01, k = 10)
mean(knn.pred2 != mpg01.test)

# KNN(k=100)
knn.pred3 = knn(train.X, test.X, train.mpg01, k = 300)
mean(knn.pred3 != mpg01.test)

