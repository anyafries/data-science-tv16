# OLS model and CV using R packages
OLS <- lm(famincr~.,steve)
OLS.cv = cvFit(OLS, data=steve, y=steve$famincr, K=100, seed=161, cost=rmspe)

#Calculate CV Error
steve<-steve[sample(nrow(steve)),]
folds <- cut(seq(1,nrow(steve)),breaks=10,labels=FALSE)
OLS_errors = rep(NA,10)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- steve[testIndexes, ]
  trainData <- steve[-testIndexes, ]
  OLS_CV_model = lm(famincr~.,steve)
  OLS_CV_pred = predict(OLS_CV_model,select(testData,-famincr))
  OLS_errors[i] = sqrt(mean((OLS_CV_pred - as.numeric(testData$famincr))^2))
}





