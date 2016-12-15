#---
#title: "Question2"
#author: "Alok Satpathy"
#date: "December 6, 2016"
#output: word_document
#---
  
  ###Logistic

getwd()
setwd("C:/Users/Alok Satpathy/Desktop/Fall 2016/EDA/Final Project")

Q2csv=read.csv("Question2Dataset.csv")
attach(Q2csv)

#Selecting columns that would be used for preparing model
Q2df=data.frame(a_score, a_body_length, a_body_has_code, a_DaysOld, a_has_edited, a_num_comment, a_owner_reputation, a_owner_profile_summary, a_owner_views, a_owner_upvotes, a_owner_downvotes, q_score, q_num_views, q_body_length, q_body_has_code, q_DaysOld, q_has_edited, q_title_length, q_num_tags, q_num_answers, q_num_comment, q_owner_reputation, q_owner_profile_summary, q_owner_views, q_owner_upvotes, q_owner_downvotes, accepted_answer_flag, a_votes_up, a_votes_down, q_votes_up, q_votes_down)

str(Q2df)

#Replacing NA with 0
Q2df[is.na(Q2df)]=0
str(Q2df)


##########################################################################################
##########################################################################################
# Subset Selection Methods

# Best Subset Selection
lapply(Q2df["accepted_answer_flag"], unique)

#install.packages("leaps")
library(leaps)
regfit.full=regsubsets(accepted_answer_flag~.,Q2df)
summary(regfit.full)
#
regfit.full=regsubsets(accepted_answer_flag~.,data=Q2df,nvmax=32)
reg.summary=summary(regfit.full)
names(reg.summary)
#
reg.summary$rsq
#
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
#plot(regfit.full,scale="r2")
#plot(regfit.full,scale="adjr2")
#plot(regfit.full,scale="Cp")
#plot(regfit.full,scale="bic")
coef(regfit.full,6)

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(accepted_answer_flag~.,data=Q2df,nvmax=32,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(accepted_answer_flag~.,data=Q2df,nvmax=32,method="backward"
)
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Q2df),rep=TRUE)
test=(!train)
regfit.best=regsubsets(accepted_answer_flag~.,data=Q2df[train,],nvmax=32)
test.mat=model.matrix(accepted_answer_flag~.,data=Q2df[test,])
val.errors=rep(NA,31)
for(i in 1:11){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Q2df$accepted_answer_flag[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,30)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(accepted_answer_flag~.,data=Q2df,nvmax=32)
coef(regfit.best,30)
k=30
set.seed(1)
folds=sample(1:k,nrow(Q2df),replace=TRUE)
cv.errors=matrix(NA,k,31, dimnames=list(NULL, paste(1:31)))
for(j in 1:k){
  best.fit=regsubsets(accepted_answer_flag~.,data=Q2df[folds!=j,],nvmax=30)
  for(i in 1:30){
    pred=predict(best.fit,Q2df[folds==j,],id=i)
    cv.errors[j,i]=mean( (Q2df$accepted_answer_flag[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(accepted_answer_flag~.,data=Q2df, nvmax=30)
coef(reg.best,3)

##########################################################################################
##########################################################################################


# Ridge Regression and LASSO
fix(Q2df)
names(Q2df)
dim(Q2df)
sum(is.na(Q2df$accepted_answer_flag))
Q2df=na.omit(Q2df)
dim(Q2df)
sum(is.na(Q2df))
#
#
install.packages("glmnet")
library(glmnet)

#the package invokes inputs and outputs separately unlike lm and glm
x=model.matrix(accepted_answer_flag~.,Q2df)[,-1]
y=Q2df$accepted_answer_flag

# set vector of lambda values to study range from 10^10 to 0.01, total length=100
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
#
# let us look at a few results here

#first lambda=50
#
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

#next, lambda=60
#
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

#prediction of the coefficients for lambda=50 (play with this)
predict(ridge.mod,s=500,type="coefficients")[1:31,]

#prepare for training and validation set testing

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
#

#evaluate and compare test MSE and the spread of y.test
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
#
#test wth two other lambdas
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)

#
#compare with lm
# The following two are the same
#
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:31,]

#
#Cross validation to get the best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

#now predict with the best lambda
#
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:31,]

# Lasso

#only difference in model building is to use alpha=1
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
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:31,]
lasso.coef
lasso.coef[lasso.coef!=0]



#Dimensionality Reduction PCA:
install.packages("pls")
library(pls)
set.seed(2)
pcr.fit=pcr(accepted_answer_flag~., data=Q2df,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(accepted_answer_flag~., data=Q2df,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)


# Partial Least Squares
set.seed(1)
pls.fit=plsr(accepted_answer_flag~., data=Q2df,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(accepted_answer_flag~., data=Q2df,scale=TRUE,ncomp=2)
summary(pls.fit)
##########################################################################################
##########################################################################################



#Dividing dataset 75% for training and 25% for testing
Q2smp_size=floor(0.75*nrow(Q2df))
set.seed(123)
Q2train_ind=sample(seq_len(nrow(Q2df)), size = Q2smp_size)
Q2Train=Q2df[Q2train_ind, ]
Q2Test=Q2df[-Q2train_ind, ]

#Initial logistic regression model with all parameters
Q2lm=glm(accepted_answer_flag~.,data=Q2Train,family="binomial")
summary(Q2lm)

#Checking for significant parameters with forward step
Q2modelstepforward=step(Q2lm, direction="forward")
summary(Q2modelstepforward)

#Checking for significant parameters with backward step
Q2modelstepbackward=step(Q2lm, direction="backward")
summary(Q2modelstepbackward)

#Removing insignificant parameters and remodelling
Q2lm2=glm(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_num_views+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary+q_owner_downvotes,data=Q2Train,family="binomial")
summary(Q2lm2)

#Again removing insignificant parameters and remodelling
Q2lm3=glm(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary,data=Q2Train,family="binomial")
summary(Q2lm3)

BIC(Q2lm3)


#Predicting based on the logistic model built
logistic_probs=predict(Q2lm3, Q2Train, type="response")
head(logistic_probs)

testing_y=Q2Test$accepted_answer_flag
logistic_pred_y=rep(0,length(testing_y))
logistic_pred_y[logistic_probs>0.5]=1

training_y=Q2Train$accepted_answer_flag
table(logistic_pred_y,training_y)

mean(logistic_pred_y!=training_y,na.rm=TRUE)



logistic_probs=predict(Q2lm3, Q2Test, type="response")
head(logistic_probs)

logistic_pred_y=rep(0,length(testing_y))
logistic_pred_y[logistic_probs>0.5]=1

table(logistic_pred_y,testing_y)
mean(logistic_pred_y!=testing_y,na.rm=TRUE)

#Cross Validation for logistic regression
#LOOCV
#library(boot)
#MSE_LOOCV=cv.glm(Q2Train, Q2lm3)$delta[1]
#MSE_LOOCV
#
#MSE_LOOCV=NULL
#for(i in 1:10){
#  model=glm(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_o#wner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_#comment+q_owner_profile_summary,data=Q2Train)
#  MSE_LOOCV[i]=cv.glm(Q2Train,model)$delta[1]
#}
#MSE_LOOCV


#K-fold
library(boot)
MSE_10_Fold_CV=cv.glm(Q2Train,Q2lm3,K=10)$delta[1]
MSE_10_Fold_CV

MSE_10_Fold_CV=NULL
for(i in 1:10){
  model=glm(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary,data=Q2Train)
  MSE_10_Fold_CV[i]=cv.glm(Q2Train,model,K=10)$delta[1]
}
MSE_10_Fold_CV


#ROC of Logistic Regression
#install.packages("ROCR")
library(ROCR)
ROCRpred<- prediction(logistic_probs,testing_y)
ROCRperf<- performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf, colorize = TRUE, print.cutoffs.at = seq(0,1,0.05), text.adj = c(-0.2,1.7))

as.numeric(performance(ROCRpred,"auc")@y.values)







###LDA
#install.packages("lda")
library(lda)
library(MASS)

lda.model = lda(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary, data=Q2Train, family = binomial)
lda.model

#predicting the LDA model with training data
lda_pred=predict(lda.model,Q2Train)
lda_class=lda_pred$class

#CONFUSION MATRIX with LDA Training data
table(lda_class,Q2Train$accepted_answer_flag)

#Misclassification Rate with LDA training data
mean(lda_class!=Q2Train$accepted_answer_flag)

#predicting the LDA model with test data
lda_prediction=predict(lda.model,Q2Test)
lda_class_n=lda_prediction$class

#CONFUSION MATRIX with LDA Testing data
table(lda_class_n,Q2Test$accepted_answer_flag)

#Misclassification Rate with LDA testing data
mean(lda_class_n!=Q2Test$accepted_answer_flag)


#CROSS VALIDATION of LDA Model
library(rpart)
library(ipred)
ip.lda <- function(object, newdata) predict(object, newdata = newdata)$class
errorest(factor(Q2Train$accepted_answer_flag)~ Q2Train$a_body_length+Q2Train$a_body_has_code+Q2Train$a_DaysOld+Q2Train$a_has_edited+Q2Train$a_num_comment+Q2Train$a_owner_reputation+Q2Train$a_owner_profile_summary+Q2Train$a_owner_downvotes+Q2Train$q_DaysOld+Q2Train$q_title_length+Q2Train$q_num_answers+Q2Train$q_num_comment+Q2Train$q_owner_profile_summary, data=Q2Train, model=lda, estimator="cv",est.para=control.errorest(k=10), predict=ip.lda)$err


#ROC for LDA
S = lda_prediction$posterior[,2]
roc.curve=function(s,print=FALSE){
  Ps=(S>s)*1
  FP=sum((Ps==1)*(Q2Test$accepted_answer_flag == 0))/sum(Q2Test$accepted_answer_flag == 0)
  TP=sum((Ps==1)*(Q2Test$accepted_answer_flag == 1))/sum(Q2Test$accepted_answer_flag == 1)
  if(print==TRUE){
    print(table(Observed=Q2Test$accepted_answer_flag,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
roc.curve(threshold,print=TRUE)

ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l", main = "ROC for LDA")
abline(0,1)



###QDA
qda_model = qda(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary, data=Q2Train, family=binomial)
qda_model

#predicting the model with training data
qda_pred=predict(qda_model, Q2Train)
qda_class=qda_pred$class

#CONFUSION MATRIX for QDA training data
table(qda_class,Q2Train$accepted_answer_flag)

#Misclassification Rate for QDA training data
mean(qda_class!=Q2Train$accepted_answer_flag)

#predicting the model with test data for QDA
qda_pred1=predict(qda_model,Q2Test)
qda_class_n=qda_pred1$class

#CONFUSION MATRIX for QDA test data
table(qda_class_n,Q2Test$accepted_answer_flag)

#Misclassification Rate for QDA test data
mean(qda_class_n!=Q2Test$accepted_answer_flag)

#CROSS VALIDATION for QDA
ip.qda <- function(object, newdata) predict(object, newdata = newdata)$class
errorest(factor(Q2Train$accepted_answer_flag)~ Q2Train$a_body_length+Q2Train$a_body_has_code+Q2Train$a_DaysOld+Q2Train$a_has_edited+Q2Train$a_num_comment+Q2Train$a_owner_reputation+Q2Train$a_owner_profile_summary+Q2Train$a_owner_downvotes+Q2Train$q_DaysOld+Q2Train$q_title_length+Q2Train$q_num_answers+Q2Train$q_num_comment+Q2Train$q_owner_profile_summary, data=Q2Train, model=qda, estimator="cv",est.para=control.errorest(k=10), predict=ip.qda)$err

#ROC for QDA
qda.S = qda_pred1$posterior[,2]
roc.curve=function(s,print=FALSE){
  Ps=(qda.S>s)*1
  FP=sum((Ps==1)*(Q2Test$accepted_answer_flag == 0))/sum(Q2Test$accepted_answer_flag == 0)
  TP=sum((Ps==1)*(Q2Test$accepted_answer_flag == 1))/sum(Q2Test$accepted_answer_flag == 1)
  if(print==TRUE){
    print(table(Observed= Q2Test$accepted_answer_flag, Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
roc.curve(threshold,print=TRUE)

ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))

plot(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l", main = "ROC for QDA")
abline(0,1)



###KNN
library(class)
train.x<-cbind(Q2Train$a_body_length+Q2Train$a_body_has_code+Q2Train$a_DaysOld+Q2Train$a_has_edited+Q2Train$a_num_comment+Q2Train$a_owner_reputation+Q2Train$a_owner_profile_summary+Q2Train$a_owner_downvotes+Q2Train$q_DaysOld+Q2Train$q_title_length+Q2Train$q_num_answers+Q2Train$q_num_comment+Q2Train$q_owner_profile_summary)

test.x<- cbind(Q2Test$a_body_length+Q2Test$a_body_has_code+Q2Test$a_DaysOld+Q2Test$a_has_edited+Q2Test$a_num_comment+Q2Test$a_owner_reputation+Q2Test$a_owner_profile_summary+Q2Test$a_owner_downvotes+Q2Test$q_DaysOld+Q2Test$q_title_length+Q2Test$q_num_answers+Q2Test$q_num_comment+Q2Test$q_owner_profile_summary)
set.seed(123)

#predicting the model with testing data for KNN with k=1
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=1,prob=TRUE)

#CONFUSION matrix for KNN
table(knn.pred,Q2Test$accepted_answer_flag)

#misclassification rate for KNN
mean(knn.pred!=Q2Test$accepted_answer_flag)




#Trying to predict KNN with different k values to get the lowest misclassification rate
#predicting the KNN model with k=2
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=2,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)




#predicting the KNN model with k=3
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=3,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)




#predicting the KNN model with k=4
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=4,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)




#predicting the KNN model with k=5
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=5,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=6
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=6,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=7
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=7,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=8
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=8,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)




#predicting the KNN model with k=9
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=9,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=10
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=10,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)


#predicting the KNN model with k=11
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=11,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)




#predicting the KNN model with k=12
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=12,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=13
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=13,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=14
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=14,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)




#predicting the KNN model with k=15
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=15,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=17
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=17,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=18
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=18,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=19
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=19,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=20
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=20,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=21
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=21,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)



#predicting the KNN model with k=22
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=22,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)


#predicting the KNN model with k=100
knn.pred<- knn(data.frame(train.x),data.frame(test.x),Q2Train$accepted_answer_flag,k=100,prob=TRUE)
table(knn.pred,Q2Test$accepted_answer_flag)
mean(knn.pred!=Q2Test$accepted_answer_flag)




#taking the model built using knn=8
#CROSS VALIDATION of KNN
bwpredict.knn <- function(object, newdata) predict.ipredknn(object, newdata, type="class")
errorest(factor(Q2Train$accepted_answer_flag) ~ Q2Train$a_body_length+Q2Train$a_body_has_code+Q2Train$a_DaysOld+Q2Train$a_has_edited+Q2Train$a_num_comment+Q2Train$a_owner_reputation+Q2Train$a_owner_profile_summary+Q2Train$a_owner_downvotes+Q2Train$q_DaysOld+Q2Train$q_title_length+Q2Train$q_num_answers+Q2Train$q_num_comment+Q2Train$q_owner_profile_summary, data=Q2Train, model=ipredknn, estimator="cv", est.para=control.errorest(k=10), predict=bwpredict.knn, kk=10)$err


#ROC for KNN
library(ROCR)
prob<- attr(knn.pred,"prob")
prob

prob <- (2*(ifelse(knn.pred == "0", 1-prob, prob)) - 1)
knn <- prediction(prob, testing_y)
pred_knn <- prediction(prob, testing_y)
pred_knn <- performance(pred_knn, "tpr", "fpr")
plot(pred_knn, avg= "threshold", colorize=T, lwd=3, main="ROC curve for KNN")
as.numeric(performance(ROCRpred,"auc")@y.values)







###CART
#install.packages("tree")
library(tree)

tree.Q2Train = tree(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary,Q2Train)
summary(tree.Q2Train)

plot(tree.Q2Train)
text(tree.Q2Train,pretty=0)

#Classification tree
lf=seq(1,nrow(Q2Train))
tree.Q2Train=tree(as.factor(accepted_answer_flag)~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary,subset=lf)

summary(tree.Q2Train)

plot(tree.Q2Train)
text(tree.Q2Train,pretty=0)

tree.pred=predict(tree.Q2Train,Q2Test,type="class")

#Confusion matrix for CART
table(tree.pred,testing_y)

#Misclassification rate for CART
mean(tree.pred!=Q2Test$accepted_answer_flag)

#Pruning and Cross Validation of the Classification Tree
cv.Q2Train=cv.tree(tree.Q2Train,FUN=prune.misclass)
names(cv.Q2Train)

cv.Q2Train

par(mfrow=c(1,2))
plot(cv.Q2Train$size, cv.Q2Train$dev, type="b")
plot(cv.Q2Train$k, cv.Q2Train$dev, type="b")


par(mfrow=c(1,1))
prune.Q2Train=prune.misclass(tree.Q2Train, best=9)

plot(prune.Q2Train)
text(prune.Q2Train,pretty=0)

tree.pred=predict(prune.Q2Train, Q2Test, type="class")
table(tree.pred, testing_y)

mean(tree.pred!=Q2Test$accepted_answer_flag)




###Random Forest
library(randomForest)
set.seed(123)

bag.Q2Train=randomForest(accepted_answer_flag~a_body_length+a_body_has_code+a_DaysOld+a_has_edited+a_num_comment+a_owner_reputation+a_owner_profile_summary+a_owner_downvotes+q_DaysOld+q_title_length+q_num_answers+q_num_comment+q_owner_profile_summary,data=Q2Train, subset=lf, mtry=4, importance=TRUE)

bag.Q2Train
yhat.bag=predict(bag.Q2Train, Q2Test)

plot(yhat.bag, testing_y)
abline(0,1)

mean((yhat.bag-testing_y)^2)
mean(yhat.bag!=testing_y)

#ROC for CART
tree.pred=predict(tree.Q2Train,Q2Test,type="vector",prob=TRUE)
tree.pred

tree.S = tree.pred[,2]
roc.curve=function(s,print=FALSE){
  Ps=(tree.S>s)*1
  FP=sum((Ps==1)*(Q2Train$accepted_answer_flag == 0))/sum(Q2Train$accepted_answer_flag == 0)
  TP=sum((Ps==1)*(Q2Train$accepted_answer_flag == 1))/sum(Q2Train$accepted_answer_flag == 1)
  if(print==TRUE){
    print(table(Observed=Q2Test$accepted_answer_flag,Predicted=Ps))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}
threshold = 0.5
roc.curve(threshold,print=TRUE)
ROC.curve=Vectorize(roc.curve)
M.ROC=ROC.curve(seq(0,1,by=.01))
plot(M.ROC[1,],M.ROC[2,],col="blue",lwd=2,type="l", main = "ROC for CART")
abline(0,1)





