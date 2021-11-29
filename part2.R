library(stevedata)
library(tidyverse)
library(cvTools)
library(glmnet)
data(TV16)

###############################################################
## ------------------------------------------------------------
##                 Setting aside a holdout set
## ------------------------------------------------------------
###############################################################

### Basic data cleaning
TV16 <- TV16 %>% 
  select(-uid,-state,-lrelig,-lcograc,-lemprac) %>%   # do not need individual ID + remove latent variables
  mutate(female=as.factor(female),                    # convert non-ordinal covariates to factors! 
         collegeed=as.factor(collegeed),
         race=as.factor(racef),
         bornagain=as.factor(bornagain),
         votetrump=as.factor(votetrump)
  ) %>%
  select(-racef)

# Random data split
set.seed(2724)
n_rows <- nrow(TV16)
n_rows_test <- as.integer(0.2 * n_rows)
n_rows_steve <- n_rows - n_rows_test
test_indices <- sample(n_rows, n_rows_test)
steve <- TV16[-test_indices,] 
test <- TV16[test_indices,]

## this should've been done earlier... but didn't do it before in Part 1
steve <- steve[complete.cases(steve),]
test <- test[complete.cases(test),]

###############################################################
## ------------------------------------------------------------
##                        Prediction
##             ("best" classification models)
## ------------------------------------------------------------
###############################################################


all.1s <- trump_votes/total_votes   # - all 1s classifier  
all.0s <- other_votes/total_votes   # - all 0s classifier 

# cross entropy loss 
calc_class_err = function(actual, predicted) {
  mean(actual != predicted, na.rm=TRUE)
}

# CE loss, TPR, FPR
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


#########################################
#      *** Logistic Regression ***
#  (CV to estimate error on train set)
#########################################
# Note: our best threshold was always 0.5 so we use that
set.seed(42)
steve1 <- steve[sample(nrow(steve)),]

num_folds <- 5
folds <- cut(seq(1,nrow(steve1)),breaks=num_folds,labels=FALSE)

logistic.CV.errs <- data.frame(err.train=c(), err.cv=c(), tpr=c(), fpr=c()) # model with all coefficients
logistic.CV.errs2 <- data.frame(err.train=c(), err.cv=c(), tpr=c(), fpr=c()) # model with coefficients  ideo, pid7na, whiteadv

for(i in 1:num_folds){
  cv.testIndexes <- which(folds==i,arr.ind=TRUE)
  cv.testData <- steve1[cv.testIndexes, ] 
  cv.trainData <- steve1[-cv.testIndexes, ] 
  
  ## Logistic model with all variables 
  fm.logistic = glm(formula = votetrump ~ . , family = 'binomial', data=cv.trainData)
  pred.logistic <- predict(fm.logistic, newdata = cv.testData, type="response")
  err <- errors(pred.logistic, cv.testData$votetrump, thresh=0.5)
  err_train <- errors(predict(fm.logistic, newdata = cv.trainData, type="response"), cv.trainData$votetrump, thresh=0.5)
  df.tmp <- data.frame(err.train=c(err_train[1]), err.cv=c(err[1]), tpr = c(err[2]), fpr = c(err[3]))
  logistic.CV.errs <- rbind(logistic.CV.errs,df.tmp)
  
  ## Logistic model with originally chosen variables: ideo, pid7na, whiteadv
  fm.logistic2 = glm(formula = votetrump ~ ideo + pid7na + whiteadv +
                       collegeed + bornagain + whiteadv + race,
                     family = 'binomial', data=cv.trainData)
  pred.logistic2 <- predict(fm.logistic2, newdata = cv.testData, type="response")
  err2 <- errors(pred.logistic2, cv.testData$votetrump, thresh=0.5)
  err_train2 <- errors(predict(fm.logistic2, newdata = cv.trainData, type="response"), cv.trainData$votetrump, thresh=0.5)
  df.tmp2 <- data.frame(err.train=c(err_train2[1]), err.cv=c(err2[1]), tpr = c(err2[2]), fpr = c(err2[3]))
  logistic.CV.errs2 <- rbind(logistic.CV.errs2,df.tmp2)
}

logistic.means <- logistic.CV.errs %>%
  group_by(threshold) %>%
  summarize(mean.err.train=mean(err.train),
            mean.err=mean(err.cv),
            mean.fpr=mean(fpr),
            mean.tpr=mean(tpr))

logistic.means2 <- logistic.CV.errs2 %>%
  group_by(threshold) %>%
  summarize(mean.err.train=mean(err.train),
            mean.err=mean(err.cv),
            mean.fpr=mean(fpr),
            mean.tpr=mean(tpr))

## ROC curves removed because we use a threshold of 0.5

## Plot boxplots of CV error
cv.err.across.logistic <- data.frame(Model=c(
  rep("Logistic A",num_folds),
  rep("Logistic B",num_folds)),
  CV.Error.Train=c(logistic.CV.errs[,"err.train"], logistic.CV.errs2[,"err.train"]),
  CV.Error.Test=c(logistic.CV.errs[,"err.cv"], logistic.CV.errs2[,"err.cv"]),
  FPR=c(logistic.CV.errs[,"fpr"], logistic.CV.errs2[,"fpr"]),
  TPR=c(logistic.CV.errs[,"tpr"], logistic.CV.errs2[,"tpr"]))

cv.err.across.logistic %>% 
  group_by(Model) %>%
  summarize(`Estimated Error`=mean(CV.Error.Test)) %>%
  ggplot(aes(Model, `Estimated Error`)) + 
  geom_col()

#########################################
#    *** Final Model Choice ***   
#########################################
fm.logistic <- glm(formula = votetrump ~ . , family = 'binomial', data=steve)
pred.logistic.train <- predict(fm.logistic, newdata = steve, type="response")
err.train <- errors(pred.logistic.train, steve$votetrump)

#########################################
#    *** Prediction on test set ***   
#########################################
pred.logistic.test <- predict(fm.logistic, newdata = test, type="response")
err.test <- errors(pred.logistic.test, test$votetrump)




###############################################################
## ------------------------------------------------------------
##                        Inference
## ------------------------------------------------------------
###############################################################

# Model picked is fm.logistic, rename so its easy to work with
fm <- fm.logistic
summary(fm)

# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -8.955825   0.187349 -47.803  < 2e-16 ***
#   age                  0.014694   0.001379  10.657  < 2e-16 ***
#   female1             -0.118997   0.042528  -2.798  0.00514 ** 
#   collegeed1          -0.468675   0.046259 -10.131  < 2e-16 ***
#   famincr              0.005145   0.007106   0.724  0.46903    
#   ideo                 0.492134   0.028096  17.516  < 2e-16 ***
#   pid7na               0.696462   0.012494  55.746  < 2e-16 ***
#   bornagain1           0.271161   0.052633   5.152 2.58e-07 ***
#   religimp             0.246493   0.031993   7.705 1.31e-14 ***
#   churchatd           -0.074328   0.017753  -4.187 2.83e-05 ***
#   prayerfreq           0.019158   0.014135   1.355  0.17529    
#   angryracism          0.281496   0.023563  11.947  < 2e-16 ***
#   whiteadv             0.562623   0.017266  32.586  < 2e-16 ***
#   fearraces            0.195736   0.018105  10.811  < 2e-16 ***
#   racerare             0.246342   0.018690  13.181  < 2e-16 ***
#   raceBlack           -0.787222   0.156060  -5.044 4.55e-07 ***
#   raceHispanic        -0.108925   0.143283  -0.760  0.44713    
#   raceMiddle Eastern  -0.420978   0.520922  -0.808  0.41901    
#   raceMixed           -0.269227   0.187969  -1.432  0.15206    
#   raceNative American  0.257607   0.240359   1.072  0.28383    
#   raceOther           -0.175786   0.217149  -0.810  0.41822    
#   raceWhite            0.065678   0.122457   0.536  0.59172    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# fit chosen model on the test data, and look at whether the same coefficients are significant
# X = different
fm.test <- glm(formula = votetrump ~ . , family = 'binomial', data=test)
summary(fm.test)
# Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)         -9.096781   0.379397 -23.977  < 2e-16 ***
#   age                  0.016182   0.002715   5.961 2.51e-09 ***
#   female1             -0.135265   0.084704  -1.597   0.1103          X
#   collegeed1          -0.401816   0.093020  -4.320 1.56e-05 ***
#   famincr              0.024564   0.014206   1.729   0.0838 .        X
#   ideo                 0.589378   0.055103  10.696  < 2e-16 ***
#   pid7na               0.645411   0.023956  26.942  < 2e-16 ***
#   bornagain1           0.482844   0.106410   4.538 5.69e-06 ***
#   religimp             0.139841   0.064820   2.157   0.0310 *        X
#   churchatd           -0.059646   0.036519  -1.633   0.1024          X
#   prayerfreq           0.032837   0.028782   1.141   0.2539    
#   angryracism          0.202479   0.046681   4.337 1.44e-05 ***
#   whiteadv             0.565824   0.034794  16.262  < 2e-16 ***
#   fearraces            0.225955   0.036266   6.230 4.65e-10 ***
#   racerare             0.266926   0.036886   7.236 4.61e-13 ***
#   raceBlack           -0.659053   0.311249  -2.117   0.0342 *        X
#   raceHispanic        -0.005121   0.291752  -0.018   0.9860    
#   raceMiddle Eastern  -0.634375   0.991651  -0.640   0.5224    
#   raceMixed            0.465848   0.390766   1.192   0.2332    
#   raceNative American  0.121464   0.470894   0.258   0.7965    
#   raceOther           -0.059624   0.442651  -0.135   0.8929    
#   raceWhite            0.030100   0.253021   0.119   0.9053    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


coeff_names = rownames(summary(fm)$coefficients)

#########################################
#   *** CIs from regression table ***   
#########################################

(CIs.regtable.train <- data.frame(
  coef = coeff_names,
  lower = summary(fm)$coefficients[,1] - 1.96*summary(fm)$coefficients[,2],
  upper = summary(fm)$coefficients[,1] + 1.96*summary(fm)$coefficients[,2]
))

(CIs.regtable.test <- data.frame(
  coef = coeff_names,
  lower = summary(fm.test)$coefficients[,1] - 1.96*summary(fm.test)$coefficients[,2],
  upper = summary(fm.test)$coefficients[,1] + 1.96*summary(fm.test)$coefficients[,2]
))


#########################################
#  *** Bootstrap for CI estimation ***   
#########################################

library(rsample)
n.boot <- 100
boots.train <- bootstraps(steve, times=n.boot)

## Get coeff estimates for each bootstrap
estimates.bootstrap.train <- sapply(boots.train$splits, function(x){
  data.train <- as.data.frame(x)
  fm.train <- glm(formula = votetrump ~ . , family = 'binomial', data=data.train)
  return(summary(fm.train)$coefficients[,1])
})
# TO DO: plot distributions of these to see quantile CI or SE CI? 

## Get quantile CIs
alpha <- 0.05

(CIs.bootstrap.train <- data.frame(
  coef = coeff_names,
  lower = sapply(1:length(coeff_names), function(row){quantile(estimates.bootstrap.train[row,], probs=c(alpha/2))}),
  upper = sapply(1:length(coeff_names),function(row){quantile(estimates.bootstrap.train[row,], probs=c(1-alpha/2))})
))
CIs.bootstrap.train[,2:3] - CIs.regtable.train[,2:3] # Difference between CIs reg table to this

# Get other kind of CIs?



###########################################
#  * Comparison of coeffs to other model *   
###########################################
## Choose a reasonable model with only a subset of the covariates for comparison. 
## Did the significant coefficients change? If so, explain the differences.

fm.other = glm(formula = votetrump ~ ideo + pid7na + whiteadv +
                     collegeed + bornagain + whiteadv + race,
                   family = 'binomial', data=steve)
summary(fm.other)





