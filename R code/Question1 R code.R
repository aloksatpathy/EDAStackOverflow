#reading and transforming the data
df1= read.csv(file="QueryResults1.csv",header=TRUE)
df2<- read.csv(file="QueryResults2.csv",header=TRUE)
StackOverflow<- rbind(df1,df2)
#str(StackOverflow)
#names(StackOverflow)
nrow(StackOverflow)

#replacing NA with 1001
StackOverflow [is.na(StackOverflow)] <- 1001

#removing column that contain text or are unrelated to this question
StackOverflowdata<- StackOverflow[,-c(1, 2,3,12,14,15,22,23,25,26,27,28,29,30,31,32,33,34)]
#str(StackOverflowdata)
nrow(StackOverflowdata)

#to check uniqueness of data
lapply(StackOverflowdata["RejectionReasonId"], unique)
StackOverflowdata$IsSpam= ifelse(StackOverflowdata$RejectionReasonId==101,1,0)
#str(StackOverflowdata)
#names(StackOverflowdata)
StackOverflowdata<- StackOverflowdata[,-c(16)]
lapply(StackOverflowdata["IsSpam"], unique)

#dividing data for training and testing purpose
smp_size<- floor(0.75*nrow(StackOverflowdata))
set.seed(123)
train_ind<- sample(seq_len(nrow(StackOverflowdata)), size=smp_size)
training_data<- StackOverflowdata[train_ind,]
testing_data<- StackOverflowdata[-train_ind,]
testing_y<- testing_data$IsSpam
nrow(training_data)
nrow(testing_data)
#str(training_data)
#names(training_data)

#GLM
logistics_model<-glm(IsSpam~.,data=training_data,family="binomial")
#to discover better model
#step(logistics_model, direction = "forward")

logistics_model = glm(IsSpam ~ PostTypeId + PostScore + post_length + 
                        owner_reputation + owner_profile_summary + owner_views + 
                        owner_upvotes + owner_downvotes + editDurationAfterCreation +
                        q_num_tags + AnswerCount + CommentCount + has_code +post_views + 
                        UserId, family = "binomial", data = training_data)
summary(logistics_model)

#performing trial modelling
#step(logistics_model, direction = "backward")

#final regression model
logistics_model = glm(formula = IsSpam ~ PostTypeId + PostScore + post_length + 
                        owner_reputation + owner_profile_summary + 
                        owner_upvotes+ editDurationAfterCreation + 
                        q_num_tags + AnswerCount + CommentCount+ post_views, family = "binomial", data = training_data)
summary(logistics_model)

#Bayesian information criterion
BIC(logistics_model)
logistics_probs<-predict(logistics_model,training_data,type="response")
head(logistics_probs)
logistics_pred_y=rep(0,length(testing_y))
logistics_pred_y[logistics_probs>0.55]=1
training_y=training_data$IsSpam
table(logistics_pred_y,training_y)
mean(logistics_pred_y!=training_y,na.rm=TRUE)
logistics_probs<- predict(logistics_model,testing_data, type="response")
head(logistics_probs)
logistics_pred_y=rep(0,length(testing_y))
logistics_pred_y[logistics_probs>0.55]=1
table(logistics_pred_y,testing_y)
mean(logistics_pred_y!=testing_y,na.rm=TRUE)


#kfold for regression
library(boot)
MSE_10_Fold_CV=cv.glm(training_data,logistics_model,K=10)$delta[1]
MSE_10_Fold_CV
MSE_10_Fold_CV=NULL
for(i in 1:10){
  model=glm(IsSpam~poly(PostTypeId + PostScore + post_length + 
                          owner_reputation + owner_profile_summary + 
                          owner_upvotes+ editDurationAfterCreation + 
                          q_num_tags + AnswerCount + CommentCount+ post_views),data=training_data)
  MSE_10_Fold_CV[i]=cv.glm(training_data,model,K=10)$delta[1]
}
#summary(model)
MSE_10_Fold_CV

#ROC logistic regression
#install.packagesll.packages("ROCR")
library(ROCR)
ROCRpred=prediction(logistics_probs,testing_y)
ROCRperf=performance(ROCRpred,"tpr","fpr")
#plot(ROCRperf)
#plot(ROCRperf,colorize=TRUE)
#plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.05),text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred,"auc")@y.values)

#Linear Discriminant Analysis
library(MASS)
lda.model<- lda(IsSpam~PostTypeId + PostScore + post_length +
                  owner_reputation + owner_profile_summary +
                  owner_upvotes+ editDurationAfterCreation +
                  q_num_tags + AnswerCount + CommentCount+ post_views,data=training_data)

#lda.model
#summary(lda.model)
lda_pred<- predict(lda.model,training_data)
lda_class<- lda_pred$class
table(lda_class,training_data$IsSpam)
mean(lda_class!=training_data$IsSpam)

lda_pred<- predict(lda.model,testing_data)
lda_class<- lda_pred$class
table(lda_class,testing_data$IsSpam)
mean(lda_class!=testing_data$IsSpam)

#Cross Validation LDA
library(rpart) 
library(ipred)
ip.lda <- function(object, newdata) predict(object, newdata = newdata)$class
errorest(factor(training_data$IsSpam)~ training_data$PostTypeId + training_data$PostScore + training_data$post_length +
           training_data$owner_reputation + training_data$owner_profile_summary +
           training_data$owner_upvotes+ training_data$editDurationAfterCreation +
           training_data$q_num_tags + training_data$AnswerCount + training_data$CommentCount+ training_data$post_views, data=training_data, model=lda, estimator="cv",est.para=control.errorest(k=10), predict=ip.lda)$err

#ROC LDA
S=lda_pred$posterior[,2]
roc.curve=function(s,print=FALSE){
  Ps=(S>s)*1
  FP=sum((Ps==1)*(testing_data$IsSpam==0))/sum(testing_data$IsSpam==0)
  TP=sum((Ps==1)*(testing_data$IsSpam==1))/sum(testing_data$IsSpam==1)
  if(print==TRUE){
    print(table(Observed=testing_data$IsSpam,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.53
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l",main="ROC for LDA")
abline(0,1)

#Quadratic Discriminant Analysis
qda.model<- qda(IsSpam~PostTypeId + PostScore + post_length +
                  owner_reputation + owner_profile_summary +
                  owner_upvotes+ editDurationAfterCreation +
                  q_num_tags + AnswerCount + CommentCount+ post_views,data=training_data)
#qda.model
qda_pred=predict(qda.model,training_data)
qda_class=qda_pred$class
table(qda_class,training_data$IsSpam)
mean(qda_class!=training_data$IsSpam)
qda_pred1=predict(qda.model,testing_data)
qda_class_n=qda_pred1$class
table(qda_class_n,testing_data$IsSpam)
mean(qda_class_n!=testing_data$IsSpam)

#Cross Validation QDA
ip.qda <- function(object, newdata) predict(object, newdata = newdata)$class
errorest(factor(training_data$IsSpam)~ training_data$PostTypeId + training_data$PostScore + training_data$post_length +
           training_data$owner_reputation + training_data$owner_profile_summary +
           training_data$owner_upvotes+ training_data$editDurationAfterCreation +
           training_data$q_num_tags + training_data$AnswerCount + training_data$CommentCount+ training_data$post_views, data=training_data, model=qda, estimator="cv",est.para=control.errorest(k=10), predict=ip.qda)$err

#ROC QDA
qda.S=qda_pred1$posterior[,2]
roc.curve=function(s,print=FALSE){
  Ps=(qda.S>s)*1
  FP=sum((Ps==1)*(testing_data$IsSpam==0))/sum(testing_data$IsSpam==0)
  TP=sum((Ps==1)*(testing_data$IsSpam==1))/sum(testing_data$IsSpam==1)
  if(print==TRUE){
    print(table(Observed=testing_data$IsSpam,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.85
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l",main="ROC for LDA")
abline(0,1)

#k nearest neighbours
library(class)
#install.packagesll.packages("cars")
#library(cars)
train.x<- cbind(training_data$PostTypeId + training_data$PostScore + training_data$post_length + training_data$owner_reputation + training_data$owner_profile_summary + training_data$owner_upvotes + training_data$editDurationAfterCreation + training_data$q_num_tags + training_data$AnswerCount + training_data$CommentCount + training_data$post_views)

test.x<- cbind(testing_data$PostTypeId + testing_data$PostScore + testing_data$post_length + testing_data$owner_reputation + testing_data$owner_profile_summary + testing_data$owner_upvotes + testing_data$editDurationAfterCreation + testing_data$q_num_tags + testing_data$AnswerCount + testing_data$CommentCount + testing_data$post_views)

train1.x=train.x[!duplicated(train.x),drop=FALSE]
test1.x=test.x[!duplicated(test.x), drop=FALSE]
tt<- training_data$IsSpam[duplicated(train.x)=='FALSE']
head(tt)
length(tt)

knn.pred<- knn(data.frame(train1.x),data.frame(test1.x),tt,k=1)
tt1<- testing_data$IsSpam[duplicated(test.x)=='FALSE']
length(tt1)
table(knn.pred,tt1)
mean(knn.pred!=tt1)

knn.pred<- knn(data.frame(train1.x),data.frame(test1.x),tt,k=2)
table(knn.pred,tt1)
mean(knn.pred!=tt1)



#Classification and Regression Trees
#CART Modeling:
#install.packagesll.packages("tree")
library(tree)
tree.training_data=tree(as.factor(IsSpam)~PostTypeId + PostScore + post_length +
                          owner_reputation + owner_profile_summary +
                          owner_upvotes+ editDurationAfterCreation +
                          q_num_tags + AnswerCount + CommentCount+ post_views,training_data)
text(tree.training_data,pretty=0)
summary(tree.training_data)
plot(tree.training_data)
text(tree.training_data,pretty=0)
lf<- seq(1,nrow(training_data))
tree.training_data=tree(as.factor(IsSpam)~PostTypeId + PostScore + post_length +
                          owner_reputation + owner_profile_summary +
                          owner_upvotes+ editDurationAfterCreation +
                          q_num_tags + AnswerCount + CommentCount+ post_views,training_data,subset=lf)
tree.pred=predict(tree.training_data,testing_data,type="class")
table(tree.pred,testing_y)
mean(tree.pred!=testing_data$IsSpam)


#Cross Validation and Pruning for the Classification Tree:
cv.training_data=cv.tree(tree.training_data,FUN=prune.misclass)
names(cv.training_data)
#cv.training_data
par(mfrow=c(1,2))
plot(cv.training_data$size,cv.training_data$dev,type="b")
plot(cv.training_data$k,cv.training_data$dev,type="b")

par(mfrow=c(1,1))
prune.training_data=prune.misclass(tree.training_data,best=5)
plot(prune.training_data)
text(prune.training_data,pretty=0)
tree.pred=predict(prune.training_data,testing_data,type="class")
table(tree.pred,testing_y)
mean(tree.pred!=testing_data$IsSpam)


#ROC for CART:
tree.pred=predict(tree.training_data,testing_data,type="vector",prob=TRUE)
#tree.pred
tree.S=tree.pred[,2]
roc.curve=function(s,print=FALSE){
  Ps=(tree.S>s)*1
  FP=sum((Ps==1)*(testing_data$IsSpam==0))/sum(testing_data$IsSpam==0)
  TP=sum((Ps==1)*(testing_data$IsSpam==1))/sum(testing_data$IsSpam==1)
  if(print==TRUE){
    print(table(Observed=testing_data$IsSpam,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold=0.55
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l",main="ROC for CART")
abline(0,1)

#Random Forest Modeling:
#install.packagesll.packages("randomForest")
library(randomForest)


bag.training_data=randomForest(as.factor(IsSpam)~PostTypeId + PostScore + post_length +
                                 owner_reputation + owner_profile_summary +
                                 owner_upvotes+ editDurationAfterCreation +
                                 q_num_tags + AnswerCount + CommentCount+ post_views,data=training_data,subset=lf,importance=TRUE)



#bag.training_data
xyz = predict(bag.training_data, newdata = testing_data)
table(testing_y, xyz)
mean(xyz!=testing_y) 

#C5.0
#install.packagesll.packages("C50")
library(C50)
c50_model <- C5.0(training_data[-16], as.factor(training_data$IsSpam))
c50_model
#summary(c50_model)

#testing C50
c50_pred <- predict(c50_model, testing_data)
#install.packagesll.packages("gmodels")
library(gmodels)
CrossTable(testing_data$IsSpam, c50_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#improving C50 with adaptive boosting
c50_boost10 <- C5.0(training_data[-16], as.factor(training_data$IsSpam),
                    trials = 10)
c50_boost10

c50_pred10 <- predict(c50_boost10, testing_data)
CrossTable(testing_data$IsSpam, c50_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Dimensional Reduction
#install.packages("ISLR")
library(ISLR)
#fix(StackOverflowdata)
#names(StackOverflowdata)
dim(StackOverflowdata)
sum(is.na(StackOverflowdata$IsSpam))
StackOverflowdata=na.omit(StackOverflowdata)
dim(StackOverflowdata)
sum(is.na(StackOverflowdata))

#install.packages("pls")
library(pls)
set.seed(2)
pcr.fit=pcr(IsSpam~., data=StackOverflowdata,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(IsSpam~., data=training_data,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,testing_data,ncomp=7)
mean((pcr.pred-testing_y)^2)
pcr.fit=pcr(IsSpam~.,data=testing_data, scale=TRUE,ncomp=7)
mean((pcr.pred-testing_y)^2)
summary(pcr.fit)

# Partial Least Squares
set.seed(1)
pls.fit=plsr(IsSpam~., data=StackOverflowdata,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,testing_data,ncomp=2)
mean((pls.pred-testing_y)^2)
pls.fit=plsr(IsSpam~., data=StackOverflowdata,scale=TRUE,ncomp=2)
summary(pls.fit)

# Subset Selection Methods
# Best Subset Selection
#install.packages("ISLR")
library(ISLR)
#sum(is.na(StackOverflow$IsSpam))
lapply(StackOverflowdata["IsSpam"], unique)
#Confirms NO NA data
#install.packages("leaps")
library(leaps)
regfit.full=regsubsets(IsSpam~.,StackOverflowdata)
summary(regfit.full)

regfit.full=regsubsets(IsSpam~.,data=StackOverflowdata,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)

reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted 
     RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
coef(regfit.full,6)

# Forward and Backward Stepwise Selection
regfit.fwd=regsubsets(IsSpam~.,data=StackOverflowdata,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(IsSpam~.,data=StackOverflowdata,nvmax=19,method="backward"
)
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(StackOverflowdata),rep=TRUE)
test=(!train)
regfit.best=regsubsets(IsSpam~.,data=StackOverflowdata[train,],nvmax=19)
test.mat=model.matrix(IsSpam~.,data=StackOverflowdata[test,])
val.errors=rep(NA,19)
for(i in 1:11){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((StackOverflowdata$IsSpam[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(IsSpam~.,data=StackOverflowdata,nvmax=19)
coef(regfit.best,10)
k=10
set.seed(1)
folds=sample(1:k,nrow(StackOverflowdata),replace=TRUE)
cv.errors=matrix(NA,k,11, dimnames=list(NULL, paste(1:11)))
for(j in 1:k){
  best.fit=regsubsets(IsSpam~.,data=StackOverflowdata[folds!=j,],nvmax=11)
  for(i in 1:11){
    pred=predict(best.fit,StackOverflowdata[folds==j,],id=i)
    cv.errors[j,i]=mean( (StackOverflowdata$IsSpam[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(IsSpam~.,data=StackOverflowdata, nvmax=11)
coef(reg.best,11)

# Ridge Regression and LASSO
#install.packages("ISLR")
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

#install.packages("glmnet")
library(glmnet)

#the package invokes inputs and outputs separately unlike lm and glm
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# set vector of lambda values to study range from 10^10 to 0.01, total length=100
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))

# let us look at a few results here

#first lambda=50

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#next, lambda=60

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

#prediction of the coefficients for lambda=50 (play with this)
predict(ridge.mod,s=500,type="coefficients")[1:20,]

#prepare for training and validation set testing

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])

#evaluate and compare test MSE and the spread of y.test
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

#test wth two other lambdas
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)


# compare with lm
# The following two are the same
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

#Cross validation to get the best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#now predict with the best lambda
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# Lasso
#only difference in model building is to use aloha=1
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

# use CV to get best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

#use best lambda for prediction
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
```
# Support Vector Classifier
set.seed(1)
install.packages("e1071")
librarIsSpam(e1071)
svmfit=svm(IsSpam~., data=training_data, kernel="linear", cost=10,scale=FALSE)
plot(svmfit, training_data)

#which ones are support vectors
svmfit$index
summary(svmfit)

svmfit=svm(IsSpam~., data=training_data, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, training_data)
svmfit$index

#cross validation
set.seed(1)
tune.out=tune(svm,IsSpam~.,data=training_data,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)


ypred=predict(bestmod,testing_data)
table(predict=ypred, truth=testing_y)

svmfit=svm(IsSpam~., data=training_data, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testing_data)
table(predict=ypred, truth=testing_y)


plot(x, col=(y+5)/2, pch=19)
dat=data.frame(x=x,y=as.factor(IsSpam))
svmfit=svm(IsSpam~., data=dat, kernel="linear", cost=1e5)
summarIsSpam(svmfit)
plot(svmfit, dat)

svmfit=svm(IsSpam~., data=dat, kernel="linear", cost=1)
summarIsSpam(svmfit)
plot(svmfit,dat)

set.seed(1)
svmfit=svm(IsSpam~., data=training_data, kernel="radial",  gamma=1, cost=1)
plot(svmfit, training_data)
summary(svmfit)
svmfit=svm(IsSpam~., data=training_data, kernel="radial",gamma=1,cost=1e5)
plot(svmfit,training_data)
set.seed(1)
tune.out=tune(svm, IsSpam~., data=training_data, kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=testing_data, pred=predict(tune.out$best.model,newx=testing_data))

#ROC
install.packages("ROCR")
library(ROCR)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}
svmfit.opt=svm(IsSpam~., data=training_data, kernel="radial",gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,training_data,decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
rocplot(fitted,training_data,main="Training Data")
svmfit.flex=svm(IsSpam~., data=training_data, kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,training_data,decision.values=T))$decision.values
rocplot(fitted,training_data,add=T,col="red")
fitted=attributes(predict(svmfit.opt,testing_y,decision.values=T))$decision.values
rocplot(fitted,testing_y,main="Test Data")
fitted=attributes(predict(svmfit.flex,testing_data,decision.values=T))$decision.values
rocplot(fitted,testing_y,add=T,col="red")
