library(ISLR)

#Problem 6-8

x=rnorm(100, mean = 1, sd = 1)
err = rnorm(100, mean = 0, sd = 1)

beta_0 <- 1
beta_1 <- 2
beta_2 <- 3
beta_3 <- 2

y <- beta_0 + beta_1*x + beta_2*x^2 + beta_3*x^3 + err

data_set <- data.frame(x,x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9, x^10, y)
library(leaps)
regfit.full=regsubsets(y~.,data=data_set)
reg.summary <- summary(regfit.full)
names(summary(regfit.full))

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
       ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)], col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type='l')
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)], col="red",cex=2,pch=20)
#_____________ Forward and backward selection
regfit.full=regsubsets(y~.,data=data_set, method="forward")
reg.summary <- summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)], col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type='l')
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)], col="red",cex=2,pch=20)


regfit.full=regsubsets(y~.,data=data_set, method="backward")
reg.summary <- summary(regfit.full)

par(mfrow=c(2,2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(reg.summary$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)

plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type='l')
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)], col="red",cex=2,pch=20)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type='l')
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)], col="red",cex=2,pch=20)

#Validation set approach
set.seed (1)
train=sample(c(TRUE,FALSE), nrow(data_set),rep=TRUE)
test =(! train )
regfit.full=regsubsets(y~.,data=data_set[train,])
test.mat=model.matrix(y~.,data=data_set[test,])
summary(regfit.full)

val.errors=rep(NA,8)
for(i in 1:8){
  coefi=coef(regfit.full,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((y[test]-pred)^2) }
coef(regfit.full ,which.min(val.errors)[1])

regfit.best=regsubsets(y~.,data=data_set)
coef(regfit.best ,which.min(val.errors)[1])

# function for validation set and cross-validation

predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi}

# Cross-Validation approach
k=10
set.seed(1)
folds=sample(1:k,nrow(data_set),replace=TRUE)
cv.errors=matrix(NA,k,8, dimnames=list(NULL, paste(1:8)))

for(j in 1:k){
  best.fit = regsubsets( y~., data = data_set [folds != j , ])
  for (i in 1:8){
    predict=predict(best.fit, data_set[folds==j, ], id=i)
    cv.errors[j,i]=mean((y[folds==j]-pred)^2)
  }
}
cv.errors  
which.min(apply(cv.errors, 2, mean))[1]

# Lasso regression using cross validation
library(glmnet)
grid=10^seq(10,-2,length=100)
model.matrix(y~.,data_set)
x=model.matrix(y~.,data_set)[,-1]


k=10
cv.errors=matrix(NA,k,length(grid), dimnames=list(NULL, paste(1:length(grid))))
for (i in 1:k){
# for k-fold
ridge.mod=glmnet(x[folds != i,], y[folds != i],alpha=1,lambda=grid)
#for lambda in column 1
for (j in 1:length(grid)){
  coefi = coef(ridge.mod)[,j]
  xvars=names(coefi)
  test.mat=model.matrix(y~.,data=data_set)[folds==i,]
  pred = test.mat[,xvars]%*%coefi
  y_new = y[folds == i]
  cv.errors[i,j]=mean((pred-y_new)^2)
  }
}
cv.errors
apply(cv.errors, 2, mean)
idx = which.min(apply(cv.errors, 2, mean))[1]
lasso.mod=glmnet(x, y,alpha=1,lambda=grid[idx])
coef(lasso.mod)
#Problem 6-9
train=sample(c(TRUE,FALSE), nrow(College),rep=TRUE)
test =(! train )
# Linear regression
lm.fit=lm(Apps~. ,data=College,subset=train)
attach(College)
test.err <- mean((Apps-predict(lm.fit,College))[test]^2)
print(test.err)
# Ridge and Laso regression
grid=10^seq(10,-2,length=100)
x=model.matrix(Apps~.,College)
k=10
folds=sample(1:k,nrow(College),replace=TRUE)
cv.errors=matrix(NA,k,length(grid), dimnames=list(NULL, paste(1:length(grid))))
for (i in 1:k){
  # for k-fold
  # alpha=0 for Ridge and alpha=1 for Laso
  ridge.mod=glmnet(x[folds != i,], Apps[folds != i],alpha=0,lambda=grid)
  #for lambda in column 1
  for (j in 1:length(grid)){
    coefi = coef(ridge.mod)[,j]
    xvars=names(coefi)
    test.mat=model.matrix(Apps~.,data=College)[folds==i,]
    pred = test.mat[,xvars]%*%coefi
    y_new = Apps[folds == i]
    cv.errors[i,j]=mean((pred-y_new)^2)
  }
}
cv.errors
apply(cv.errors, 2, mean)
idx = which.min(apply(cv.errors, 2, mean))[1]
ridge.pred=predict(ridge.mod,s=0.01,newx=x[test,])
mean((ridge.pred-College$Apps[test])^2)
#PCR
library(pls)
set.seed (2)
pcr.fit=pcr(Apps~., data=College , subset=train, scale=TRUE,
              validation ="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
x=model.matrix(Apps~.,College)[,-1]
pcr.pred=predict(pcr.fit,x[test,],ncomp=17)
mean((pcr.pred-College$Apps[test])^2)
#PLS
plsr.fit=plsr(Apps~., data=College , subset=train, scale=TRUE,
            validation ="CV")
validationplot(plsr.fit,val.type="MSEP")
summary(plsr.fit)
pls.pred=predict(plsr.fit,x[test,],ncomp=17)
mean((pls.pred-College$Apps[test])^2)




