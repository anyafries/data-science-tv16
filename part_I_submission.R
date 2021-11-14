###############################################################
##                        Best Models
###############################################################

## ------------------------------------------------------------
##                        Classification
## ------------------------------------------------------------

# Build logistic model on training data with all variables and a threshold of 5
t <- 5
fm.logistic <- glm(formula = votetrump ~ . , family = 'binomial', data=train)

# Cross entropy loss
calc_class_err = function(actual, predicted) {
  mean(actual != predicted, na.rm=TRUE)
}

# Get errors, FPR, TPR
errors <- function(predictions, truth, thresh = 0.5) {
  binary.predictions <- as.numeric(predictions>thresh)
  CE.loss <- calc_class_err(binary.predictions,truth)
  
  TP <- sum(as.numeric(binary.predictions[binary.predictions==truth]),na.rm=TRUE)
  TN <- sum(as.numeric(1-binary.predictions[binary.predictions==truth]),na.rm=TRUE)
  FP <- sum(as.numeric(binary.predictions[binary.predictions!=truth]),na.rm=TRUE)
  FN <- sum(as.numeric(1-binary.predictions[binary.predictions!=truth]),na.rm=TRUE)
  TPR <- TP/(FN+TP)
  FPR <- FP/(TN+FP)

  return(c(CE.loss, TPR, FPR))
}

# CV: get errors, FPR, TPR
set.seed(42)
num_folds <- 5
train1 <- train[sample(nrow(train)),]
folds <- cut(seq(1,nrow(train1)),breaks=num_folds,labels=FALSE)

logistic.CV.errs <- data.frame(threshold=c(), err.train=c(), err.cv=c(), tpr=c(), fpr=c())
for(i in 1:num_folds){
  cv.testIndexes <- which(folds==i,arr.ind=TRUE)
  cv.testData <- train1[cv.testIndexes, ] 
  cv.trainData <- train1[-cv.testIndexes, ] 
  
  ## Logistic model with all variables 
  fm.logistic = glm(formula = votetrump ~ . , family = 'binomial', data=cv.trainData)
  pred.logistic <- predict(fm.logistic, newdata = cv.testData, type="response")
  err <- errors(pred.logistic, cv.testData$votetrump, thresh=t)
  err_train <- errors(predict(fm.logistic, newdata = cv.trainData, type="response"),
                      cv.trainData$votetrump,
                      thresh=t)
  df.tmp <- data.frame(threshold=c(t),
                        err.train=c(err_train[1]),
                        err.cv=c(err[1]), 
                        tpr = c(err[2]),
                        fpr = c(err[3]))
  logistic.CV.errs <- rbind(logistic.CV.errs,df.tmp)
}

## ------------------------------------------------------------
##                        Regression
## ------------------------------------------------------------

# OLS model and CV using R packages
OLS <- lm(famincr~.,train)
OLS.cv = cvFit(OLS, data=train, y=train$famincr, K=100, seed=161, cost=rmspe)

#Calculate CV Error
train2<-train[sample(nrow(train)),]
folds <- cut(seq(1,nrow(train2)),breaks=10,labels=FALSE)
OLS_errors = rep(NA,10)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- train2[testIndexes, ]
  trainData <- train2[-testIndexes, ]
  OLS_CV_model = lm(famincr~.,trainData)
  OLS_CV_pred = predict(OLS_CV_model,select(testData,-famincr))
  OLS_errors[i] = sqrt(mean((OLS_CV_pred - as.numeric(testData$famincr))^2))
}





