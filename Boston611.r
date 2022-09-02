library(MASS)
fix(Boston)
dim(Boston)
sum(is.na(Boston$crim))
attach(Boston)
summary(Boston)

#train=sample(nrow(Boston), nrow(Boston)*.7)
#test=(-train)

#最优子集选择法
library(leaps)
regfit.best=regsubsets(crim~.,data=Boston,nvmax=13)
regfit.sum=summary(regfit.best)
regfit.sum
par(mfrow=c(1,1))
plot(regfit.sum$rss,xlab ="Number  of  Variablas" ,ylab = "RSS",type = "l")
plot(regfit.sum$adjr2, xlab="Number  of  Variablas" ,  ylab="adjusted rsq" ,type = 'l')
points(which.max(regfit.sum$adjr2),regfit.sum$adjr2[which.max(regfit.sum$adjr2)],col='red',cex=2,pch=20)
plot(regfit.sum$cp,xlab ="Number  of  Variablas" ,ylab = "Cp",type = "l")
points(which.min(regfit.sum$cp),regfit.sum$cp[which.min(regfit.sum$cp)],col='red',cex=2,pch=20)
plot(regfit.sum$bic, xlab="Number  of  Variablas" ,  ylab="BIC" ,type = 'l')
points(which.min(regfit.sum$bic),regfit.sum$bic[which.min(regfit.sum$bic)],col='red',cex=2,pch=20)
coef(regfit.best,3)
coef(regfit.best,8)

regfit.fwd=regsubsets(crim~.,data=Boston,nvmax=13,method = "forward")
summary(regfit.fwd)
which.min(summary(regfit.fwd)$cp)
which.min(summary(regfit.fwd)$bic)
coef(regfit.fwd,8)
coef(regfit.fwd,3)
regfit.bwd=regsubsets(crim~.,data=Boston,nvmax=13,method = "backward")
summary(regfit.bwd)
which.min(summary(regfit.bwd)$cp)
which.min(summary(regfit.bwd)$bic)
coef(regfit.bwd,8)
coef(regfit.bwd,4)

set.seed(1)
train=sample(c(TRUE,FALSE),nrow(Boston),rep=TRUE)
test=(!train)
regfit.b=regsubsets(crim~.,data=Boston[train,],nvmax=13)
test.mat=model.matrix(crim~.,data=Boston[test,])
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(regfit.b,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((crim[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.b,9)

#rindge
x=model.matrix(crim~.,Boston)[,-1]
y=Boston$crim
sum(is.na(Boston))
library(glmnet)
train1=sample(nrow(Boston),nrow(Boston)/2)
test1=(-train1)
grid=10^seq(10,-2,nrow(Boston)/2)
ridge.mod=glmnet(x[train1,],y[train1],alpha=0,lambda=grid)
dim(coef(ridge.mod))
set.seed(1)
cv.out=cv.glmnet(x[train1,],y[train1],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test1,])
mean((ridge.pred-y[test1])^2)
out=glmnet(x,y,alpha=0,lambda=grid)
predict(out,type="coefficients",s=bestlam)[1:14,]


#lasso

lasso.mod=glmnet(x[train1,],y[train1],alpha=1,lambda=grid)
dim(coef(lasso.mod))
set.seed(1)
cv.out=cv.glmnet(x[train1,],y[train1],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test1,])
mean((lasso.pred-y[test1])^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:14,]
lasso.coef[lasso.coef!=0]


#pcr
library(pls)
set.seed(2)
pcr.fit=pcr(crim~.,data=Boston,subset=train1,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.pred=predict(pcr.fit,x[test1,],ncomp=13)
mean((pcr.pred-y[test1])^2)


pcr.fit1=pcr(crim~.,data=Boston,scale=TRUE,ncomp=13)
summary(pcr.fit1)

#pls
set.seed(1)
pls.fit=plsr(crim~.,data=Boston,subset=train1,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
set.seed(1)
pls.pred=predict(pls.fit,x[test1,],ncomp=6)
mean((pls.pred-y[test1])^2)


pls.fit1=plsr(crim~.,data=Boston,scale=TRUE,ncomp=6)
summary(pls.fit1)
