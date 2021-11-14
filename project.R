library(MASS)
library(stevedata)
library(tidyverse)
library(knitr)
library(GGally)
library(cvTools)
library(glmnet)
library(randomForest)
data(TV16)

###############################################################
## ------------------------------------------------------------
##                 Setting aside a holdout set
## ------------------------------------------------------------
###############################################################

# Random data split
set.seed(2724)
n_rows <- nrow(TV16)
n_rows_test <- as.integer(0.2 * n_rows)
n_rows_steve <- n_rows - n_rows_test
test_indices <- sample(n_rows, n_rows_test)
steve <- TV16[-test_indices,] # steve is our test set
test <- TV16[test_indices,]

###############################################################
## ------------------------------------------------------------
##            Investigating and exploring your data
## ------------------------------------------------------------
###############################################################

### Basic data cleaning

steve <- steve %>% 
  # mutate(value = 1, 
  #        race=as.factor(racef), 
  #        racef=as.factor(racef))  %>% 
  # spread(racef, value, fill = 0) %>%                  # one-hot encodings of races, keep column with all races though
  select(-uid,-state,-lrelig,-lcograc,-lemprac) %>%   # do not need individual ID + remove latent variables
  mutate(female=as.factor(female),                    # convert non-ordinal covariates to factors! 
         collegeed=as.factor(collegeed),
         race=as.factor(racef),
         bornagain=as.factor(bornagain),
         votetrump=as.factor(votetrump)
         # Asian=as.factor(Asian),
         # Black=as.factor(Black),
         # `Middle Eastern`=as.factor(`Middle Eastern`),
         # Mixed=as.factor(Mixed),
         # `Native American`=as.factor(`Native American`),
         # Other=as.factor(Other),
         # White=as.factor(White)
  ) %>%
  select(-racef)


# Look at structure again
str(steve)

# ---------------------------------------------
#                Checking NAs 
# ---------------------------------------------

# check number of NAs
total_nas <- colSums(is.na(steve))
kable(total_nas)

total <-nrow(steve)
kable(total_nas/total)

steve_lab <- steve
steve_lab$type <- "All cases"

steve.complete <- steve[complete.cases(steve),]
steve.complete$type <- "Complete cases"
# sanity check
kable(colSums(is.na(steve.complete)))

complete.vs.incomplete <- rbind(steve_lab,steve.complete)

a <- ggplot(complete.vs.incomplete,aes(x=as.numeric(votetrump)-1,y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="votetrump",y="Density") +
  scale_x_continuous(breaks=seq(0,1)) +
  theme(legend.position = "none")

b <- ggplot(complete.vs.incomplete,aes(x=age, color=type)) +
  geom_density() + 
  labs(x="age",y="Density") +
  theme(legend.position = "none")

c <- ggplot(complete.vs.incomplete,aes(x=as.numeric(female)-1,y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="female",y="Density") +
  scale_x_continuous(breaks=seq(0,1))+
  theme(legend.position = "none")

d <- ggplot(complete.vs.incomplete,aes(x=as.numeric(collegeed)-1,y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="collegeed",y="Density") +
  scale_x_continuous(breaks=seq(0,1))+
  theme(legend.position = "none")

e <- ggplot(complete.vs.incomplete,aes(x=as.numeric(famincr),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="famincr",y="Density") +
  scale_x_continuous(breaks=0:11)+
  theme(legend.position = "none")

f <- ggplot(complete.vs.incomplete,aes(x=as.numeric(ideo),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="ideo",y="Density") +
  scale_x_continuous(breaks=0:5)+
  theme(legend.position = "none")

g <- ggplot(complete.vs.incomplete,aes(x=as.numeric(pid7na),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="pid7na",y="Density") +
  scale_x_continuous(breaks=1:7)+
  theme(legend.position = "none")

h <- ggplot(complete.vs.incomplete,aes(x=as.numeric(bornagain)-1,y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="bornagain",y="Density") +
  scale_x_continuous(breaks=seq(0,1))+
  theme(legend.position = "none")

i <- ggplot(complete.vs.incomplete,aes(x=as.numeric(churchatd),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="churchatd",y="Density") +
  scale_x_continuous(breaks=1:5)+
  theme(legend.position = "none")

j <- ggplot(complete.vs.incomplete,aes(x=as.numeric(religimp),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="religimp",y="Density") +
  scale_x_continuous(breaks=1:5)+
  theme(legend.position = "none")

k <- ggplot(complete.vs.incomplete,aes(x=as.numeric(prayerfreq),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="prayerfreq",y="Density") +
  scale_x_continuous(breaks=1:5)+
  theme(legend.position = "none")

l <- ggplot(complete.vs.incomplete,aes(x=as.numeric(angryracism),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="angryracism",y="Density") +
  scale_x_continuous(breaks=1:5)+
  theme(legend.position = "none")

m <- ggplot(complete.vs.incomplete,aes(x=as.numeric(whiteadv),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="whiteadv",y="Density") +
  scale_x_continuous(breaks=1:5)+
  theme(legend.position = "none")

n <- ggplot(complete.vs.incomplete,aes(x=as.numeric(fearraces),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="fearraces",y="Density") +
  scale_x_continuous(breaks=1:5)+
  theme(legend.position = "none")

o <- ggplot(complete.vs.incomplete,aes(x=as.numeric(racerare),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="racerare",y="Density") +
  scale_x_continuous(breaks=1:5)+
  theme(legend.position = "none")

p <- ggplot(complete.vs.incomplete,aes(x=as.numeric(race),y=..density.., fill=type)) +
  geom_histogram(alpha=0.5,binwidth = 1,position="dodge") + 
  labs(x="race",y="Density") +
  scale_x_continuous(breaks=1:8)+
  theme(legend.position = "none")

library(cowplot)
plot_grid(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,ncol=4)

# ---------------------------------------------
# ---------------------------------------------

steve<-steve.complete

## Correlation plot
library(ggcorrplot)

# all covars - race
model.matrix(~0+., data=select(steve,-race)) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2,
             colors = c("#6D9EC1", "white", "#E46726"))

# race + prediction variables
model.matrix(~0+., data=select(steve,c(race,votetrump,famincr))) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2,
             colors = c("#6D9EC1", "white", "#E46726"))

# Histograms

ggplot(steve, aes(x=age)) + 
  geom_histogram(color="black", fill="white")
ggplot(steve, aes(x=famincr)) + 
  geom_histogram(color="black", fill="white")

#Look at NAs
print(ggplot(steve[!is.na(steve$votetrump),], aes(x=ideo)) + 
  geom_histogram(color="black", fill="white"))
print(ggplot(steve[is.na(steve$votetrump),], aes(x=ideo)) + 
  geom_histogram(color="black", fill="white"))


### trumpvote
# most strongly corr w trumpvote:
# - ideo, pid7na, whiteadv
# - also plot race (was not in corr matrix)

# plot these and see a clear positive correlation
# could be interesting to plot % of each group that vote/don't vote trump
# eg esp race bc white ppl are much more numerous
ggplot(data=steve, aes(x=ideo)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1)
ggplot(data=steve, aes(x=pid7na)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1)
ggplot(data=steve, aes(x=whiteadv)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1)
ggplot(data=steve, aes(x=race)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1)

# also but a bit less but still there: racerace, angryracism
# these more just have a pattern with didn't vote trump
ggplot(data=steve, aes(x=racerare)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1) 
ggplot(data=steve, aes(x=angryracism)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1) 

# where we would expect corr but don't get
ggplot(data=steve, aes(x=bornagain)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1)
ggplot(data=steve, aes(x=collegeed)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1)
ggplot(data=steve, aes(x=famincr)) +
  geom_bar() + facet_wrap(vars(votetrump),ncol=1)

### faminc
# most strongly corr w famincr:
# - collegeed
# - also plot race (was not in corr matrix)

ggplot(data=steve, aes(x=famincr,color=collegeed, fill=collegeed)) +
  geom_density(position="identity",alpha=0.3,adjust=1.5) 
# the adjust adds some smoothing -> needed bc of categoricalness of famincr

ggplot(data=steve, aes(x=famincr, fill=race, color=race)) +
  geom_density(height=0.5, width=0.3, alpha=0.1, legend.show=FALSE) + 
  facet_wrap(vars(race),ncol=1)

# some where we expect corr but don't get

ggplot(data=steve, aes(x=age,y=famincr)) +
  geom_jitter(height=0.5, width=0, alpha=0.1)


## Useful numbers

trump_votes <- sum(steve$votetrump==1,na.rm=TRUE)
other_votes <- sum(steve$votetrump==0,na.rm=TRUE)
total_votes <- trump_votes + other_votes



###############################################################
## ------------------------------------------------------------
##                        Prediction
## ------------------------------------------------------------
###############################################################

## ------------------------------------------------------------
##                     Classification
## ------------------------------------------------------------

# comment on: 
# - all 1s classifier gives 
all.1s <- trump_votes/total_votes
# - all 0s classifier gives
all.0s <- other_votes/total_votes

### Classification ###

# - cross entropy loss
calc_class_err = function(actual, predicted) {
  mean(actual != predicted, na.rm=TRUE)
}
# - (possible extension: equal weight to each type of error)

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
#########################################

thresholds <- seq(from=0.1,to=0.9,by=0.1)
num_folds <- 5

set.seed(42)
steve1 <- steve[sample(nrow(steve)),]
folds <- cut(seq(1,nrow(steve1)),breaks=num_folds,labels=FALSE)

logistic.CV.errs <- data.frame(threshold=c(), err.train=c(), err.cv=c(), tpr=c(), fpr=c())
logistic.CV.errs2 <- data.frame(threshold=c(), err.train=c(), err.cv=c(), tpr=c(), fpr=c())
logistic.CV.errs3 <- data.frame(threshold=c(), err.train=c(), err.cv=c(), tpr=c(), fpr=c())
logistic.CV.errs4 <- data.frame(threshold=c(), err.train=c(), err.cv=c(), tpr=c(), fpr=c())

for(j in 1:length(thresholds)){
  for(i in 1:num_folds){
    print(j)
    print(i)
    t <- thresholds[j]
    cv.testIndexes <- which(folds==i,arr.ind=TRUE)
    cv.testData <- steve1[cv.testIndexes, ] 
    cv.trainData <- steve1[-cv.testIndexes, ] 
    
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
    
    ## Logistic model with originally chosen variables
    fm.logistic2 = glm(formula = votetrump ~ ideo + pid7na + whiteadv , family = 'binomial', data=cv.trainData)
    pred.logistic2 <- predict(fm.logistic2, newdata = cv.testData, type="response")
    err2 <- errors(pred.logistic2, cv.testData$votetrump, thresh=t)
    err_train2 <- errors(predict(fm.logistic2, newdata = cv.trainData, type="response"),
                        cv.trainData$votetrump,
                        thresh=t)
    df.tmp2 <- data.frame(threshold=c(t),
                         err.train=c(err_train2[1]),
                         err.cv=c(err2[1]),
                         tpr = c(err2[2]),
                         fpr = c(err2[3]))
    logistic.CV.errs2 <- rbind(logistic.CV.errs2,df.tmp2)
    
    ## Logistic model with variables that have greatest coefficient in OG model
    fm.logistic3 = glm(formula = votetrump ~ ideo + pid7na + whiteadv +
                                 collegeed + bornagain + whiteadv + race,
                       family = 'binomial', data=cv.trainData)
    pred.logistic3 <- predict(fm.logistic3, newdata = cv.testData, type="response")
    err3 <- errors(pred.logistic3,cv.testData$votetrump,thresh=t)
    err_train3 <- errors(predict(fm.logistic3, newdata = cv.trainData, type="response"),
                         cv.trainData$votetrump,
                         thresh=t)
    df.tmp3 <- data.frame(threshold=c(t),
                          err.train=c(err_train3[1]),
                          err.cv=c(err3[1]),
                          tpr = c(err3[2]),
                          fpr = c(err3[3]))
    logistic.CV.errs3 <- rbind(logistic.CV.errs3,df.tmp3)
    
    ## Logistic model with all interations
    fm.logistic4 = glm(formula = votetrump ~ . + .:.,
                       family = 'binomial', data=cv.trainData)
    pred.logistic4 <- predict(fm.logistic4, newdata = cv.testData, type="response")
    err4 <- errors(pred.logistic4, cv.testData$votetrump,thresh=t)
    err_train4 <- errors(predict(fm.logistic4, newdata = cv.trainData, type="response"),
                         cv.trainData$votetrump,
                         thresh=t)
    df.tmp4 <- data.frame(threshold=c(t),
                          err.train=c(err_train4[1]),
                          err.cv=c(err4[1]),
                          tpr = c(err4[2]),
                          fpr = c(err4[3]))
    logistic.CV.errs4 <- rbind(logistic.CV.errs4,df.tmp4)
  }
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

logistic.means3 <- logistic.CV.errs3 %>%
  group_by(threshold) %>%
  summarize(mean.err.train=mean(err.train),
            mean.err=mean(err.cv),
            mean.fpr=mean(fpr),
            mean.tpr=mean(tpr))

logistic.means4 <- logistic.CV.errs4 %>%
  group_by(threshold) %>%
  summarize(mean.err.train=mean(err.train),
            mean.err=mean(err.cv),
            mean.fpr=mean(fpr),
            mean.tpr=mean(tpr))

plot.tpr.vs.fpr <- function(errors, means.of.errors){
  ggplot(means.of.errors, aes(mean.fpr, mean.tpr)) + 
    geom_point(errors, mapping=aes(x=fpr, y=tpr, color=factor(threshold)), alpha=0.3, size=1) +
    geom_line(lty=2,color="grey") +
    geom_point(aes(color=factor(threshold)), size=2) +
    labs(x="FPR",y="TPR",color="Threshold")
}

## ROC curves for individual logistic models
plot.tpr.vs.fpr(logistic.CV.errs, logistic.means)
plot.tpr.vs.fpr(logistic.CV.errs2, logistic.means2)
plot.tpr.vs.fpr(logistic.CV.errs3, logistic.means3)
plot.tpr.vs.fpr(logistic.CV.errs4, logistic.means4) 

## ROC curves for all logistic models
ggplot() +
  geom_line(logistic.means, mapping=aes(mean.fpr, mean.tpr), lty=2,color="grey") +
  geom_point(logistic.means, mapping=aes(mean.fpr, mean.tpr, size=threshold,color="Logistic A"), shape=20,alpha=0.6) +
  
  geom_line(logistic.means2, mapping=aes(mean.fpr, mean.tpr), lty=2,color="grey") +
  geom_point(logistic.means2, mapping=aes(mean.fpr, mean.tpr,size=threshold, color="Logistic B"), shape=20,alpha=0.6) +
  
  geom_line(logistic.means3, mapping=aes(mean.fpr, mean.tpr), lty=2,color="grey") +
  geom_point(logistic.means3, mapping=aes(mean.fpr, mean.tpr,size=threshold, color="Logistic C"), shape=20,alpha=0.6) +
  
  geom_line(logistic.means4, mapping=aes(mean.fpr, mean.tpr), lty=2,color="grey") +
  geom_point(logistic.means4, mapping=aes(mean.fpr, mean.tpr,size=threshold, color="Logistic D"), shape=20,alpha=0.6) +
  
  labs(x="FPR",y="TPR",size="Threshold\n (0.1-0.9)", shape="Threshold") +
  scale_color_manual(name = "Model", values = c("Logistic A"="blue",
                                                "Logistic B"="red2",
                                                "Logistic C"="chartreuse4",
                                                "Logistic D"="orange")) 
  
## get the best threshold for each model
best.log <- logistic.means[which(logistic.means$mean.err == min(logistic.means$mean.err)),] 
best.log2 <- logistic.means2[which(logistic.means2$mean.err == min(logistic.means2$mean.err)),] 
best.log3 <- logistic.means3[which(logistic.means3$mean.err == min(logistic.means3$mean.err)),] 
best.log4 <- logistic.means4[which(logistic.means4$mean.err == min(logistic.means4$mean.err)),] 

## Plot boxplots of CV error
cv.err.across.logistic <- data.frame(Model=c(
                                      rep(paste("Logistic A\n (Threshold = ", best.log$threshold,")", sep=""),num_folds),
                                      rep(paste("Logistic B\n (Threshold = ", best.log2$threshold,")",sep=""),num_folds),
                                      rep(paste("Logistic C\n (Threshold = ", best.log3$threshold,")",sep=""),num_folds),
                                      rep(paste("Logistic D\n (Threshold = ", best.log4$threshold,")",sep=""),num_folds)),
                                     CV.Error.Train=c(
                                       logistic.CV.errs[which(logistic.CV.errs$threshold == best.log$threshold),"err.train"],
                                       logistic.CV.errs2[which(logistic.CV.errs2$threshold == best.log2$threshold),"err.train"],
                                       logistic.CV.errs3[which(logistic.CV.errs3$threshold == best.log3$threshold),"err.train"],
                                       logistic.CV.errs4[which(logistic.CV.errs4$threshold == best.log4$threshold),"err.train"]
                                     ),
                                     CV.Error.Test=c(
                                       logistic.CV.errs[which(logistic.CV.errs$threshold == best.log$threshold),"err.cv"],
                                       logistic.CV.errs2[which(logistic.CV.errs2$threshold == best.log2$threshold),"err.cv"],
                                       logistic.CV.errs3[which(logistic.CV.errs3$threshold == best.log3$threshold),"err.cv"],
                                       logistic.CV.errs4[which(logistic.CV.errs4$threshold == best.log4$threshold),"err.cv"]
                                     ),
                                     FPR=c(
                                       logistic.CV.errs[which(logistic.CV.errs$threshold == best.log$threshold),"fpr"],
                                       logistic.CV.errs2[which(logistic.CV.errs2$threshold == best.log2$threshold),"fpr"],
                                       logistic.CV.errs3[which(logistic.CV.errs3$threshold == best.log3$threshold),"fpr"],
                                       logistic.CV.errs4[which(logistic.CV.errs4$threshold == best.log4$threshold),"fpr"]
                                     ),
                                     TPR=c(
                                       logistic.CV.errs[which(logistic.CV.errs$threshold == best.log$threshold),"tpr"],
                                       logistic.CV.errs2[which(logistic.CV.errs2$threshold == best.log2$threshold),"tpr"],
                                       logistic.CV.errs3[which(logistic.CV.errs3$threshold == best.log3$threshold),"tpr"],
                                       logistic.CV.errs4[which(logistic.CV.errs4$threshold == best.log4$threshold),"tpr"]
                                     ))

cv.err.across.logistic %>% 
  group_by(Model) %>%
  summarize(`Estimated Error`=mean(CV.Error.Test)) %>%
  ggplot(aes(Model, `Estimated Error`)) + 
    geom_col()

#########################################
#              *** kNN ***
#########################################
library(class)

## CE loss + TPR, FPR
errors.for.knn <- function(predictions, truth) {
  CE.loss <- calc_class_err(truth, predictions)
  
  TP <- sum(predictions[predictions==truth] == 1)
  TN <- sum(predictions[predictions==truth] == 0)
  FP <- sum(predictions[predictions!=truth] == 1)
  FN <- sum(predictions[predictions!=truth] == 0)
  
  TPR <- TP/(FN+TP)
  FPR <- FP/(TN+FP)
  
  return(c(CE.loss, TPR, FPR))
}

# kNN needs all variables to be numerical to compute distances 
steve.kNN <- steve %>% 
  mutate(value = 1,                            # one-hot encodings of races
         rn = row_number())  %>%
  spread(race, value, fill = 0) %>%  
  mutate(female=as.integer(female)-1,          # 1/0 encodings of other factors (excluding response variable)         
         collegeed=as.integer(collegeed)-1,
         bornagain=as.integer(bornagain)-1) %>%
  select(-rn) 
steve.kNN <- steve.kNN[complete.cases(steve.kNN),]

#Perform cross validation for different values of k
num_folds <- 5
num_ngbhs <- c(1:20,seq(from=22,to=56,by=2))

set.seed(42) # bc of randomly broken ties and because we shuffle the data
steve.kNN.shuffled <- steve.kNN[sample(nrow(steve.kNN)),] #Randomly shuffle the data
folds <- cut(seq(1,nrow(steve.kNN.shuffled)),breaks=num_folds,labels=FALSE) #Create num_folds equally size folds

kNN.CV.errs <- data.frame(k=c(), err.train=c(), err.cv=c(), tpr=c(), fpr=c())

for(j in 1:length(num_ngbhs)){
  for(i in 1:num_folds){
    print(j)
    print(i)
    k = num_ngbhs[j]
    cv.testIndexes <- which(folds==i,arr.ind=TRUE)
    cv.testData <- steve.kNN.shuffled[cv.testIndexes, ] 
    cv.trainData <- steve.kNN.shuffled[-cv.testIndexes, ] 
    pred <- knn( train = select(cv.trainData,-votetrump),
                 test = select(cv.testData,-votetrump),
                 cl = cv.trainData$votetrump, 
                 k = k)
    err <- errors.for.knn(pred,cv.testData$votetrump)
    train.err <- calc_class_err(cv.trainData$votetrump,
                                knn( train = select(cv.trainData,-votetrump),
                                     test = select(cv.trainData,-votetrump),
                                     cl = cv.trainData$votetrump, 
                                     k = k))
    
    df.tmp <- data.frame(k=c(k),err.train=c(train.err),err.cv=c(err[1]),tpr=c(err[2]),fpr=c(err[3]))
    kNN.CV.errs <- rbind(kNN.CV.errs,df.tmp)
  }
}

kNN.CV.means <- kNN.CV.errs %>%
  group_by(k) %>%
  summarize(mean.err.train=mean(err.train),
            mean.err=mean(err.cv),
            mean.fpr=mean(fpr),
            mean.tpr=mean(tpr))

## Plot neighbours vs estimated error
min_err <- min(kNN.CV.means$mean.err, na.rm=TRUE)
ggplot(kNN.CV.errs, aes(x=k,y=err.cv)) + #,color=factor(k)
  geom_point(aes(color="Individual CV error"), size=0.8) +
  geom_point(data=kNN.CV.means,aes(x=k,y=mean.err, color="Mean CV error")) +
  labs(x="# of neighbours",y="CV Error") + 
  geom_hline(yintercept = min_err, color="deepskyblue4", lty=2) +
  annotate(geom="text", x = 1, hjust=0, vjust=0, size=3.5,
           y = min_err-0.002, label = "Minimum mean CV error", colour='deepskyblue4') +
  scale_color_manual(name = "", values = c("Individual CV error" = "grey",
                                           "Mean CV error" = "navyblue" )) 

## Plot ROC curve
ggplot(kNN.CV.means, aes(mean.fpr, mean.tpr,color=k)) + 
  geom_line(lty=2,color="grey") +
  geom_point(alpha=0.8) +
  labs(x="FPR",y="TPR",color="# neighbours")

## Find best k for kNN
best.kNN <- kNN.CV.means[which(kNN.CV.means$mean.err == min(kNN.CV.means$mean.err)),]
best.k <- best.kNN$k
# best k is 40 but 32 has virtually the same and would overfit less so we choose that
best.k <- 32
cv.err.best.kNN <- data.frame(Model=c(rep(paste("kNN A\n (k = ",best.k,")", sep=""),num_folds)),
                              CV.Error.Train=kNN.CV.errs[which(kNN.CV.errs$k == best.k),"err.train"],
                              CV.Error.Test=kNN.CV.errs[which(kNN.CV.errs$k == best.k),"err.cv"],
                              FPR=kNN.CV.errs[which(kNN.CV.errs$k == best.k),"fpr"],
                              TPR=kNN.CV.errs[which(kNN.CV.errs$k == best.k),"tpr"])


#########################################
#         *** Decision trees ***          
#########################################

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

errors.for.trees <- function(predictions, truth) {
  CE.loss <- calc_class_err(truth, predictions)
  
  TP <- sum(predictions[predictions==truth] == 1, na.rm = TRUE)
  TN <- sum(predictions[predictions==truth] == 0, na.rm = TRUE)
  FP <- sum(predictions[predictions!=truth] == 1, na.rm = TRUE)
  FN <- sum(predictions[predictions!=truth] == 0, na.rm = TRUE)
  
  TPR <- TP/(FN+TP)
  FPR <- FP/(TN+FP)
  
  return(c(CE.loss, TPR, FPR))
}

## 10-fold CV on trees of different depths
num_folds <- 5
depths <- 1:10
tree.CV.errs <- data.frame(depth=c(), err.train=c(), err.cv=c(), tpr=c(), fpr=c())

set.seed(42)
steve.tree <- steve[sample(nrow(steve)),]
folds <- cut(seq(1,nrow(steve.tree)),breaks=num_folds,labels=FALSE)

for(j in 1:length(depths)){
  for(i in 1:num_folds){
    print(j)
    print(i)
    depth = depths[j]
    cv.testIndexes <- which(folds==i,arr.ind=TRUE)
    cv.testData <- steve.tree[cv.testIndexes, ] 
    cv.trainData <- steve.tree[-cv.testIndexes, ] 
    
    tree <- rpart(
      votetrump~., 
      data=cv.trainData,
      method='class',
      maxdepth=depth,
      cp=0
    )
    err <- errors.for.trees(predictions=predict(tree, newdata = cv.testData, type = "class"), 
                            truth=cv.testData$votetrump)
    train.err <- calc_class_err(cv.trainData$votetrump,
                                predict(tree, newdata = cv.trainData, type = "class"))
    
    df.tmp <- data.frame(depth=c(depth),err.train=c(train.err),err.cv=c(err[1]),tpr=c(err[2]),fpr=c(err[3]))
    tree.CV.errs <- rbind(tree.CV.errs,df.tmp)
  }
}

tree.CV.means <- tree.CV.errs %>%
  group_by(depth) %>%
  summarize(mean.err.train=mean(err.train),
            mean.err=mean(err.cv),
            mean.fpr=mean(fpr),
            mean.tpr=mean(tpr))

## Plot neighbours vs estimated error
min_err <- min(tree.CV.means$mean.err, na.rm=TRUE)
ggplot(tree.CV.errs, aes(x=depth,y=err.cv)) + #,color=factor(k)
  geom_point(aes(color="Individual CV error"), size=0.8) +
  geom_point(data=tree.CV.means,aes(x=depth,y=mean.err, color="Mean CV error")) +
  labs(x="Depth of tree",y="CV Error") + 
  geom_hline(yintercept = min_err, color="deepskyblue4", lty=2) +
  annotate(geom="text", x = 1, hjust=0, vjust=0, size=3.5,
           y = min_err-0.002, label = "Minimum mean CV error", colour='deepskyblue4') +
  scale_color_manual(name = "", values = c("Individual CV error" = "grey",
                                           "Mean CV error" = "navyblue")) 

## Plot ROC curve
ggplot(tree.CV.means, aes(mean.fpr, mean.tpr)) + 
  geom_point(aes(color=factor(depth)),size=3) +
  labs(x="FPR",y="TPR",color="Depth of tree")

## Find best depth for decision trees
best.tree <- tree.CV.means[which(tree.CV.means$mean.err == min_err),]
best.depth <- best.tree$depth
cv.err.best.tree <- data.frame(Model=c(rep(paste("Decision Tree A\n (depth = ",best.depth,")", sep=""),num_folds)),
                              CV.Error.Train=tree.CV.errs[which(tree.CV.errs$depth == best.depth),"err.train"],
                              CV.Error.Test =tree.CV.errs[which(tree.CV.errs$depth == best.depth),"err.cv"],
                              FPR=tree.CV.errs[which(tree.CV.errs$depth == best.depth),"fpr"],
                              TPR=tree.CV.errs[which(tree.CV.errs$depth == best.depth),"tpr"])

## Plot best tree on all steve data (training data)
tree <- rpart(
  votetrump~.,
  data=steve,
  method='class',
  maxdepth=best.depth,
  cp=0
)
tree
fancyRpartPlot(tree, caption = NULL)

#########################################
#    *** Comparison across models ***   
#########################################

all.cv.errs <- rbind(cv.err.across.logistic,cv.err.best.kNN,cv.err.best.tree)

ggplot(all.cv.errs, aes(x=Model, y=CV.Error.Train)) +
  geom_boxplot() + 
  labs(y = "Estimated errors from cross validation")

mean.cv.errs <- all.cv.errs %>%
  group_by(Model) %>%
  summarize(`CV training error`=mean(CV.Error.Train),
            `CV test error`=mean(CV.Error.Test)#,
            # FPR=mean(FPR),
            # FNR=1-mean(TPR)
            ) %>%
  pivot_longer(., cols = c(`CV training error`,`CV test error`), names_to = "Statistic", values_to = "Val")  #,FPR,FNR in cols

## Grouped by model on x axis
ggplot(mean.cv.errs, aes(x=Model,y=Val, fill=Statistic)) +
  geom_col(position="dodge") +
  scale_fill_brewer(palette = "Accent") + 
  labs(y="",x="") #+ 
  #coord_flip() + 
  #theme(legend.position = "bottom", legend.direction = "vertical")

## Grouped by statistic on x axis
ggplot(mean.cv.errs, aes(x=Statistic,y=Val, fill=Model)) +
  geom_col(position="dodge") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(y="",x="") 



#########################################
#    *** Final Model Choice ***   
#########################################
fm.logistic <- glm(formula = votetrump ~ . , family = 'binomial', data=steve)
pred.logistic <- predict(fm.logistic, newdata = steve, type="response")
err <- errors(pred.logistic, steve$votetrump, thresh=0.5)


## ------------------------------------------------------------
##                          Regression
## ------------------------------------------------------------
# - SSE/L2 norm
# * OLS
# * OLS with interaction/higher order terms/regularisation

# Last step: see if we can do it on 2020 data 
# -> differing population model?


# OLS



#Remove rows where any value is NA for now *******
#TODO: Align on how to handle NA values
#steve <- steve[!is.na(steve$famincr),]
steve <- drop_na(steve)

#Convert factors to numbers
#steve$Asian = as.numeric(steve$Asian) - 1
#steve$Black = as.numeric(steve$Black) - 1
#steve$Hispanic = as.numeric(steve$Hispanic) 
#steve$"Middle Eastern" = as.numeric(steve$"Middle Eastern") - 1
#steve$Mixed = as.numeric(steve$Mixed) - 1
#steve$"Native American" = as.numeric(steve$"Native American") - 1
#steve$Other = as.numeric(steve$Other) - 1

steve_race_factor <- steve %>% select(-Asian, -Black, -Hispanic, -"Middle Eastern", -Mixed, -"Native American", -Other, -White)

# Remove race and white column (because otherwise with have rank-deficiency)
steve <- steve %>% select(-race, -Other)

#Save a checkpoint for steve
steve_org <- steve

# OLS model and CV for estimate
OLS <- lm(famincr~.,steve)
OLS.cv = cvFit(OLS, data=steve, y=steve$famincr, K=100, seed=161, cost=rmspe)



#calculate cross validation error
#Randomly shuffle the data
steve<-steve[sample(nrow(steve)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(steve)),breaks=10,labels=FALSE)
OLS_errors = rep(NA,10)


#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- steve[testIndexes, ]
  trainData <- steve[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  OLS_CV_model = lm(famincr~.,steve)
  OLS_CV_pred = predict(OLS_CV_model,select(testData,-famincr))
  OLS_errors[i] = sqrt(mean((OLS_CV_pred - as.numeric(testData$famincr))^2))
  
  
}
print(OLS_errors)





steve_removed <- steve %>% select(famincr, age)
OLS_removed <- lm(famincr~.,steve_removed)
OLS_removed.cv = cvFit(OLS, data=steve_removed, y=steve$famincr, K=100, seed=161, cost=rmspe)
#Baseline 
mean_inc <- mean(steve_removed$famincr)
baseline_RMSE = sqrt(sum((steve_removed$famincr - mean_inc)^2)/(length(steve_removed$famincr)))


print(baseline_RMSE)





# * OLS with interaction/higher order terms/regularisation

#Scaling columns
scaled_cols = c("pid7na","ideo","churchatd", "religimp", "prayerfreq" ,"angryracism", "whiteadv", "fearraces", "racerare")
for(col in colnames(steve)){
  if(col %in% scaled_cols){
    steve[[col]] = scale(steve[[col]])
  }
}

#Log transformation of age
steve$age = log(steve$age)

#Add some interactions terms
steve$ideo_party = steve$ideo * steve$pid7na
steve$ideo_racism = steve$ideo * steve$racerare
steve$ideo_religion = steve$ideo * steve$religimp
steve$sex_race = as.numeric(steve$female) * as.numeric(steve$Black)#discussion on intersectionality? 

#Remove race variables 
#steve <- steve %>% select(-Asian,-Black,-Hispanic,-"Middle Eastern", -Mixed, -"Native American", -"Other") 


#Perform CV on transformed data
OLS_plus <- lm(famincr~.+.*.,steve)
OLS_plus.cv = cvFit(OLS_plus, data=steve, y=steve$famincr, K=10, R=10, seed=161, cost=rmspe)

print(summary(OLS))
print(OLS.cv)
print(summary(OLS_plus))
print(OLS_plus.cv)



#calculate cross validation error
#Randomly shuffle the data
steve<-steve[sample(nrow(steve)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(steve)),breaks=10,labels=FALSE)
OLS_plus_errors = rep(NA,10)


#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- steve[testIndexes, ]
  trainData <- steve[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  OLS_plus_CV_model = lm(famincr~.,steve)
  OLS_plus_CV_pred = predict(OLS_plus_CV_model,select(testData,-famincr))
  OLS_plus_errors[i] = sqrt(mean((OLS_plus_CV_pred - as.numeric(testData$famincr))^2))
  
}
print(OLS_plus_errors)





print("Ridge Regularization ---------------------------------------")
#Perform ridge regularization 

steve <- steve_org

lam = exp(seq(-5,5,.5))

ridge <- glmnet(select(steve,-famincr), steve$famincr, alpha=0, lambda = lam)
ridge_cv <- cv.glmnet(x=data.matrix(select(steve,-famincr)), y=steve$famincr, nfolds = 10, type.measure="mse", alpha=0, lambda=lam)


#calculate cross validation error
#Randomly shuffle the data
steve<-steve[sample(nrow(steve)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(steve)),breaks=10,labels=FALSE)

ridge_errors = rep(NA,10)


#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- steve[testIndexes, ]
  trainData <- steve[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  ridge_CV_model = glmnet(select(trainData,-famincr), trainData$famincr, alpha=0, lambda = lam)
  ridge_CV_pred = predict(ridge_CV_model,data.matrix(select(testData,-famincr)))
  ridge_errors[i] = sqrt(mean((ridge_CV_pred - as.numeric(testData$famincr))^2))
  
  
}
print(ridge_errors)

print("Random Forest ---------------------------------------")



#Implement random forest
steve <- rename(steve, "MiddleEastern" = "Middle Eastern")
steve <- rename(steve, "NativeAmerican" = "Native American")

#steve_sample = sample(steve, 2)
#rf <- randomForest(famincr~., steve, type = "regression", ntree=100, nodesize = 1000)
#rf.cv <- rfcv(trainx = data.matrix(select(steve,-famincr)), trainy = steve$famincr, cv.fold =10, type = "regression", ntree=100, maxnodes = 10)



print("Plots ---------------------------------------")

# Residual plot of OLS
steve <- steve_org

OLS_confusion_matrix = table(round(predict(OLS,steve_org),digits=0),steve_org$famincr)

plot_subset <- data.frame(x = steve$famincr, y = OLS$residuals)
plot_subset <- plot_subset[sample(nrow(plot_subset),300),]

resid_plot <- ggplot(plot_subset) +  geom_point(mapping = aes(x, y), position="jitter") +
    labs(x = "Income", y = "Residuals", title = paste("Residuals of OLS model (with jitter)")) + scale_x_continuous(breaks=seq(1,12,1)) 


# Error as a function of lambda

mean_cv_error = sqrt(ridge_cv$cvm)
upper_cv_error = sqrt(ridge_cv$cvup)
lower_cv_error = sqrt(ridge_cv$cvlo)
lambdas = log(ridge_cv$lambda)

lambda_data = data.frame(x=lambdas, y = mean_cv_error, upper = upper_cv_error, lower = lower_cv_error)

lambda_plot = ggplot(lambda_data, aes(x=x, y=y)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_point() + labs(title="Ridge Regression - 10-fold CV error by lambda" ,x="log(lambda)", y="RMSE")


# Coeff for each variable

#Scaling columns
#TODO: check scaling
steve_scaled <- steve
for(col in colnames(steve)){
    steve_scaled[[col]] = scale(as.numeric(steve[[col]]))
}
OLS_scaled = lm(famincr ~ ., steve_scaled)
print(summary(OLS_scaled))

factor_names = rownames(data.frame(coef(OLS_scaled)))
coefs = data.frame(names = factor_names, val = coef(OLS_scaled))
ggplot(data=coefs) + geom_bar(mapping = aes(x = names, y=val), stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Random forest
print("Random Forest start---")
#compare the importance to the coefficients on the linear OLS model as well
#importance(rf)

steve <- steve_org
steve <- rename(steve, "MiddleEastern" = "Middle Eastern")
steve <- rename(steve, "NativeAmerican" = "Native American")


#Plot both hyper parameters and also no. of features as well 

trees=c(100)
nodes = c(10000)

#trees = c(10,50,100, 500, 1000)
#nodes = c(10000,5000,1000,500,100)
rf_error = data.frame(trees=rep(NA,25), nodesize = rep(NA,25), RMSE=rep(NA,25))

i = 1
for(nt in trees){
  for(nn in nodes){
    rf_tune <- randomForest(famincr~., steve, type = "regression", ntree=nt, nodesize = nn)
    RMSE = sqrt(sum((rf_tune$predicted - steve$famincr)^2)/length(steve$famincr))
    rf_error$trees[i] = nt
    rf_error$nodesize[i] = nn
    rf_error$RMSE[i] = RMSE
    i <- i+1
  }
}
print(rf_error)
#rf.cv <- rfcv(trainx = data.matrix(select(steve,-famincr)), trainy = steve$famincr, cv.fold =10, type = "regression", ntree=100, maxnodes = 10)


#calculate cross validation error
#Randomly shuffle the data
steve<-steve[sample(nrow(steve)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(steve)),breaks=10,labels=FALSE)

rf_errors = rep(NA,10)


#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- steve[testIndexes, ]
  trainData <- steve[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  rf_CV_model = randomForest(famincr~., steve, type = "regression", ntree=100, nodesize = 5000)
  rf_CV_pred = as.numeric(predict(rf_CV_model, testData))
  rf_errors[i] = sqrt(mean((rf_CV_pred - as.numeric(testData$famincr))^2))
}




# Ordinal logistic regression

#reveret back to before the transformations
steve <- steve_race_factor

#remove age because too many levels and preventing from converging
steve<- steve %>% select(-age)

# Make sure everything is as a factor

ordered_cols = c("famincr", "ideo", "pid7na", "religimp", "churchatd", "prayerfreq", "angryracism", "whiteadv", "fearraces", "racerare")

for(col in colnames(steve)){
  if(col %in% ordered_cols){
    steve[[col]] = factor(steve[[col]], ordered=TRUE, levels = sort(as.numeric(unique(steve[[col]]))))
  }
  else{
    steve[[col]] = as.factor(steve[[col]])
  }
}




OLR = polr(famincr~., steve, method="logistic")
#calculate training error
predictincr = predict(OLR,select(steve,-famincr))
confusion_matrix = table(steve$famincr, predictincr)
predict_rmse= sqrt(mean((as.numeric(predictincr) - as.numeric(steve$famincr))^2))


#calculate cross validation error
#Randomly shuffle the data
steve<-steve[sample(nrow(steve)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(steve)),breaks=10,labels=FALSE)

OLR_errors = rep(NA,10)


#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- steve[testIndexes, ]
  trainData <- steve[-testIndexes, ]
  #Use the test and train data partitions however you desire...
  OLR_CV_model = polr(famincr~., trainData, method="logistic")
  OLR_CV_pred = as.numeric(predict(OLR_CV_model, testData))
  OLR_errors[i] = sqrt(mean((OLR_CV_pred - as.numeric(testData$famincr))^2))
}




# Generate summary plot **********************



OLS_errors = data.frame(model = "OLS", rmse=OLS_errors)
OLS_plus_errors= data.frame(model = "OLS w/ add'l terms", rmse=OLS_plus_errors)
ridge_errors = data.frame(model = "Ridge", rmse=ridge_errors)
rf_errors = data.frame(model = "Random Forest", rmse=rf_errors)
#OLR_errors = data.frame(model = "Ordinal Regression", rmse=OLR_errors)
baseline_RMSE = data.frame(model="Baseline", rmse = baseline_RMSE)

sum_data = rbind(OLS_errors, OLS_plus_errors,ridge_errors,rf_errors,baseline_RMSE)

sum_plot <- ggplot(sum_data, aes(x=model, y=rmse)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

mean_OLS_CV_error = mean(OLS_errors$rmse.rmse)
mean_OLS_plus_CV_error = mean(OLS_plus_errors$rmse.rmse)
mean_ridge_CV_error = mean(ridge_errors$rmse.rmse)
mean_rf_CV_error = mean(rf_errors$rmse.rmse)
baseline_RMSE_error = baseline_RMSE$rmse.rmse

training_OLS_RMSE = sqrt(mean((OLS$residuals)^2))
training_OLS_plus_RMSE = sqrt(mean((OLS_plus$residuals)^2))
training_ridge_RMSE = sqrt(mean((predict(ridge,data.matrix(select(steve_org,-famincr)), lambda=.01)- steve_org$famincr)^2))
training_RF_RMSE = sqrt(8.136521)

bar_chart_data = data.frame(
  type = c("CV", "CV","CV","CV","CV","training","training","training","training","training"),
  model = c("OLS", "OLS plus", "Ridge", "RF", "Baseline","OLS", "OLS plus", "Ridge", "RF", "Baseline"),
  error = c(mean_OLS_CV_error,mean_OLS_plus_CV_error,mean_ridge_CV_error,mean_rf_CV_error,baseline_RMSE_error,training_OLS_RMSE, training_OLS_plus_RMSE, training_ridge_RMSE, training_RF_RMSE, NA )
)
ggplot(data=bar_chart_data, aes(x=model, y=error, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) + coord_cartesian(ylim=c(2.5,3.25)) + labs(title="Training vs CV RMSE of Regression Models")

steve <- steve_org
steve$votetrump <- as.factor(steve$votetrump)

ggplot(steve, aes(x=famincr)) +     geom_histogram(color="black", fill="white", binwidth=1) + scale_x_continuous(breaks=seq(1,12,1)) + labs(title="Family Income: 1(<$10k)-12(>$150k)")

ggplot(steve, aes(x=pid7na)) +    geom_histogram(color="black", fill="white", binwidth=1) + scale_x_continuous(breaks=seq(1,7,1)) + labs(title="Political Party (higher = more Republican)")

ggplot(steve, aes(x=ideo)) +    geom_histogram(color="black", fill="white", binwidth=1) + scale_x_continuous(breaks=seq(1,5,1)) + labs(title="Political Spectrum (higher = more conservative)")

ggplot(data = steve) +    geom_bar(mapping = aes(x = pid7na, fill=votetrump), position = "dodge") + labs(title="Political Party by Trump Vote", x="Political Party (higher = more Republican)") + scale_x_continuous(breaks=seq(1,7,1)) +  scale_fill_manual(name="Voted for Trump", labels = c("No", "Yes"), values = c("blue", "red"))

ggplot(data = steve) + 
  geom_bar(mapping = aes(x = famincr, fill=collegeed), position = "dodge") + labs(title="Family Income by College Education", x="Income: 1(<$10k)-12(>$150k)") + scale_x_continuous(breaks=seq(1,12,1)) +  scale_fill_manual(name="College Educated", labels = c("No", "Yes"), values = c("grey", "orange"))

ggplot(data=coefs) + geom_bar(mapping = aes(x = names, y=val), stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ labs(title="Scaled OLS Coefficients", x = "Factor", y="Value")
