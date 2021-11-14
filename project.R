library(stevedata)
library(tidyverse)
library(knitr)
library(GGally)
data(TV16)

###############################################################
## ------------------------------------------------------------
##                 Setting aside a holdout set
## ------------------------------------------------------------
###############################################################

# Random data split
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

# Look at structure
str(steve)

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

# this isn't useful yet I thought I was doing something else lol
col_names <- c(
  'votetrump' = "Voted Trump",
  'age' = "Age",
  'female' = "Female",
  'collegeed' = "College Degree",
  'race' = "Race",
  'famincr' = "Household Income",
  'ideo' = "Ideology (liberal-conservative)",
  'pid7na' = "Partisanship (democrat-republican)",
  'bornagain' = "Born-again Christian",
  'religimp' = "Importance of religion",
  'churchatd' = "Church attendance",
  'prayerfreq' = "Frequency of prayer",
  'angryracism' = "Anger at racism",
  'whiteadv' = "Belief in white priviledge",
  'fearraces' = "Fears other races",
  'racerare' = "Believes racism is rare"
)

# ---------------------------------------------
### Checking NAs ###
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

# • Look for mutual correlations between these variables you identified in the last part. 
# Create scatterplots for pairs of covariates you believe correlates well to the response variable.
# Are correlations transitive in your data? That is, if A is correlated strongly with B, and B with C, 
# is A also correlated strongly with C in your data?
# • Can you visualize interesting patterns in your data?


### Some more exploration

# Ideally, you will look at your data many different ways; for example, it’s useful to 
# look at means and variances of columns, grouped based on the level of a categorical variable.

trump_votes <- sum(steve$votetrump==1,na.rm=TRUE)
other_votes <- sum(steve$votetrump==0,na.rm=TRUE)
total_votes <- trump_votes + other_votes

# Variables to possibly add:
# - Interaction terms, higher order terms
# - One hots of certain states

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

      ## This line crashes R 
      ## but it plots the tree
      ##     :(

# fancyRpartPlot(tree, caption = NULL)

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
            `CV test error`=mean(CV.Error.Test),
            FPR=mean(FPR),
            FNR=1-mean(TPR)) %>%
  pivot_longer(., cols = c(`CV training error`,`CV test error`,FPR,FNR), names_to = "Statistic", values_to = "Val") 

## Grouped by model on x axis
ggplot(mean.cv.errs, aes(x=Model,y=Val, fill=Statistic)) +
  geom_col(position="dodge") +
  scale_fill_brewer(palette = "Accent") + 
  labs(y="")

## Grouped by statistic on x axis
ggplot(mean.cv.errs, aes(x=Statistic,y=Val, fill=Model)) +
  geom_col(position="dodge") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(y="")

## ------------------------------------------------------------
##                          Regression
## ------------------------------------------------------------
# - SSE/L2 norm
# * OLS
# * OLS with interaction/higher order terms/regularisation

# Last step: see if we can do it on 2020 data 
# -> differing population model?









