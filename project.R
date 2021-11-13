library(MASS)
library(stevedata)
library(tidyverse)
library(knitr)
library(GGally)
library(cvTools)
library(glmnet)
library(randomForest)
data(TV16)
set.seed(2724)

## ------------------------------------------------------------
##                 Setting aside a holdout set
## ------------------------------------------------------------

# Random data split
n_rows <- nrow(TV16)
n_rows_test <- as.integer(0.2 * n_rows)
n_rows_steve <- n_rows - n_rows_test
test_indices <- sample(n_rows, n_rows_test)
steve <- TV16[-test_indices,] # steve is our test set
test <- TV16[test_indices,]

## ------------------------------------------------------------
##            Investigating and exploring your data
## ------------------------------------------------------------

### Basic data cleaning

# Look at structure
#str(steve)


             
#  select(-uid,-state,-bornagain,-religimp, - churchatd, -prayerfreq, -racerare, -whiteadv, -fearraces, -angryracism) %>%  
#   select(-uid,-state,-lrelig,-lcograc,-lemprac) %>% 


steve <- steve %>% 
  mutate(value = 1, 
         race=as.factor(racef), 
         racef=as.factor(racef))  %>% 
  spread(racef, value, fill = 0) %>%                  # one-hot encodings of races, keep column with all races though
  select(-uid,-state,-lrelig,-lcograc,-lemprac) %>%     # do not need individual ID + remove latent variables
  mutate(female=as.factor(female),                    # convert non-ordinal covariates to factors! 
         collegeed=as.factor(collegeed),
         bornagain=as.factor(bornagain),
         race=as.factor(race),
         Asian=as.factor(Asian),
         Black=as.factor(Black),
         `Middle Eastern`=as.factor(`Middle Eastern`),
         Mixed=as.factor(Mixed),
         `Native American`=as.factor(`Native American`),
         Other=as.factor(Other),
         White=as.factor(White)
  )


# Look at structure again
#str(steve)

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

# check number of NAs
kable(colSums(is.na(steve)))

# significant number of NAs
# prediction continuous will be on famincr (income) -> 6521 missing
# prediction binary will be on votetrump -> 19668 missing
# TO DO: need to decide how to handle missing data

# data in column(s) 'state', 'race' are not numeric so need to remove
# correlation coefficients based on Pearson’s method
races <- c('Asian','Black','Hispanic','Middle Eastern', 'Mixed', 'Native American', 'Other', 'White')
ggcorr(select(steve,-c(all_of(races)),White, Black), 
       label = TRUE, label_size = 2,            # label changes
       hjust = 0.8, size = 3, color = "grey50", # text changes
       layout.exp = 3)                          # whitespace on left


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


## ------------------------------------------------------------
##                        Prediction
## ------------------------------------------------------------



# cross validation! 

# comment on: 
# - all 1s classifier gives 
trump_votes/total_votes
# - all 0s classifier gives
other_votes/total_votes

### Classification ###
# - cross entropy loss
# - (possible extension: equal weight to each type of error)
# * logistic regression

fm = glm(formula = votetrump ~ . - race, family = 'binomial', data=steve)
summary(fm)
  # White co-eff = NA??

# * kNN
# * decision trees



#### Regression ### --------------------------------------------------------
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
  geom_bar(stat="identity", position=position_dodge()) + coord_cartesian(ylim=c(2.5,3.25))

steve <- steve_org
steve$votetrump <- as.factor(steve$votetrump)

ggplot(steve, aes(x=famincr)) +     geom_histogram(color="black", fill="white", binwidth=1) + scale_x_continuous(breaks=seq(1,12,1)) + labs(title="Family Income: 1(<$10k)-12(>$150k)")

ggplot(steve, aes(x=pid7na)) +    geom_histogram(color="black", fill="white", binwidth=1) + scale_x_continuous(breaks=seq(1,7,1)) + labs(title="Political Party (higher = more Republican)")

ggplot(steve, aes(x=ideo)) +    geom_histogram(color="black", fill="white", binwidth=1) + scale_x_continuous(breaks=seq(1,5,1)) + labs(title="Political Spectrum (higher = more conservative)")

ggplot(data = steve) +    geom_bar(mapping = aes(x = pid7na, fill=votetrump), position = "dodge") + labs(title="Political Party by Trump Vote", x="Political Party (higher = more Republican)") + scale_x_continuous(breaks=seq(1,7,1)) +  scale_fill_manual(name="Voted for Trump", labels = c("No", "Yes"), values = c("blue", "red"))

ggplot(data = steve) + 
  geom_bar(mapping = aes(x = famincr, fill=collegeed), position = "dodge") + labs(title="Family Income by College Education", x="Income: 1(<$10k)-12(>$150k)") + scale_x_continuous(breaks=seq(1,12,1)) +  scale_fill_manual(name="College Educated", labels = c("No", "Yes"), values = c("grey", "orange"))

ggplot(data=coefs) + geom_bar(mapping = aes(x = names, y=val), stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ labs(title="Scaled OLS Coefficients", x = "Factor", y="Value")
