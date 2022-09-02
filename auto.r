D=read.csv("E:\\统计\\统计学习\\R_book_data\\auto.csv",header=T,na.strings="?")
D=na.omit(D)
y=D[,'mpg']
x=D[,'horsepower']
lm=lm(y~x)
summary(lm)
l=length(y)
l
RSS=deviance(lm)
sqrt(RSS/(l-2))

predict(lm,data.frame(x=98),interval = "prediction",level=0.95)
predict(lm,data.frame(x=98),interval = "confidence",level=0.95)


plot(x,y,xlab = 'horsepower',ylab = 'mpg')
abline(lm)

par(mfrow=c(2,2))
plot(lm)

pairs(D)

cor(subset(D, select=-name))

lmm= lm(mpg~.-name, data=D)
summary(lmm)

par(mfrow=c(2,2))
plot(lmm)

lmm2 = lm(mpg~displacement+weight+year+origin+cylinders:displacement+cylinders:year+displacement:weight+displacement:year+horsepower:weight+horsepower:year,data=D)
  #+cylinders displacement horsepower weight acceleration year origin,data=D)
summary(lmm2)
#cylinders:displacement+cylinders:year+displacement:weight+

lmm3=lm(mpg~log(displacement)+log(weight)+log(year)+log(origin),data=D)
summary(lmm3)
par(mfrow=c(2,2))
plot(lmm3)

lmm4=lm(mpg~sqrt(displacement)+sqrt(weight)+sqrt(year)+sqrt(origin),data=D)
summary(lmm4)
par(mfrow=c(2,2))
plot(lmm4)

lmm5=lm(mpg~displacement+I(displacement^2)+weight+I(weight^2)+year+I(year^2)+origin+I(origin^2),data=D)
summary(lmm5)
par(mfrow=c(2,2))
plot(lmm5)

