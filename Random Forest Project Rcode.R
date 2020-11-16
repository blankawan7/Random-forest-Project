###Creat binary response
datraw <- read.csv("C:/Users/chloe/OneDrive - University of Calgary/641project/StudentMath.txt", sep=";")
View(datraw)
dat <- within(datraw, {ylatent= (G1+G2+G3)/3
    y=ifelse(ylatent>median(ylatent),1,0)})
dat <- subset(dat,select = -c(G1,G2,G3,ylatent)) #remove columns

###Partition into training set and test sets, stratified by response class
with(new.env(),{
     set.seed(1)
     u=runif(nrow(dat))
     #stratify with index y
     cutoff=tapply(u, dat$y, function(x) {quantile(x,3/4)})
     #cutoff: 0 1; [dat$y+1]=1,2
     group=ifelse(u>cutoff[dat$y+1],"test","train")
     write.csv(dat[group=="train",],"./train.csv",row.names = FALSE)
     write.csv(dat[group=="test",],"./test.csv",row.names = FALSE)
     message("see files created in ",getwd())
     })

###Naive variable selection
train=read.csv("./train.csv")
features=c("studytime","famsup","school","sex","Fedu")
naivemodel=glm(y~.,data=train[,c(features,"y"),drop=FALSE],family = binomial)
summary(naivemodel)
cf1train = table(train$y==1,predict(naivemodel)>0,dnn=c("prediction","act"))/nrow(train)
cf1train
message("naive training accuracy is ",sum(diag(cf1train))*100," percent")

###Algorithmic variable selection
# Obtaining variable importances
# Random forest
set.seed(1)
rfmodel=randomForest::randomForest(factor(y)~.,data=train,ntree=10e3,
                                    na.action=na.omit)
rfmodel
sortimp=order(rfmodelr$importance,decreasing=TRUE) # order by importance

## Function of cross validation error
# function: get cross-validated error
getcverr = function(y,x,fold) {
  # assert
  stopifnot(length(y)==nrow(x))
  stopifnot(length(y)==length(fold))
  stopifnot(sort(unique(y))==(0:1))
  # work
  accu = lapply(sort(unique(fold)),function(k) {
    test = fold==k
    fit = glm(y[!test]~.,data=x[!test,,drop=FALSE],
              family="binomial")
    pred = ifelse(predict(fit,type="response",newdata=x[test,,drop=FALSE])>0.5,1,0)
    pred==y[test]
  })
  return(1-mean(unlist(accu)))
}


### Cross validation 
train_sort=train[,-1][,sortimp] # columns(features) sorted by importance
train_fold=with(new.env(),{ # shuffle train data
  numfolds=5 # number of folds
  fold=(1:nrow(train)) %% numfolds+1 # assign all numbers with 1:5
  set.seed(1)
  s=sample(1:nrow(train)) #shuffle all numbers
  fold[order(s)] # order(location), take the location of shuffled numbers (ASC)
}) # assign observations to folds for cross validation
cv_err=sapply(1:ncol(train_sort), function(feature_num){ # apply function to Optimal No.of features
  x=train_sort[,1:feature_num,drop=FALSE]
  getcverr(train$y,x,train_fold)
})
feature_num=which.min(cv_err)
plot(cverr,xlab="number of predictors",ylab="CV misclass rate")




