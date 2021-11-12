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

# Remove race and white column (because otherwise with have rank-deficiency)
steve <- steve %>% select(-race, -Other)

# OLS model and CV for estimate
OLS <- lm(famincr~.,steve)
OLS.cv = cvFit(OLS, data=steve, y=steve$famincr, K=100, seed=161, cost=rmspe)


steve_removed <- steve %>% select(famincr, age)
OLS_removed <- lm(famincr~.,steve_removed)
OLS_removed.cv = cvFit(OLS, data=steve_removed, y=steve$famincr, K=100, seed=161, cost=rmspe)
#Baseline 
mean_inc <- mean(steve_removed$famincr)
baseline_RMSE = sqrt(sum((steve_removed$famincr - mean_inc)^2)/(length(steve_removed$famincr)))

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
OLS_plus <- lm(famincr~.,steve)
OLS_plus.cv = cvFit(OLS_plus, data=steve, y=steve$famincr, K=10, R=10, seed=161, cost=rmspe)

print(summary(OLS))
print(OLS.cv)
print(summary(OLS_plus))
print(OLS_plus.cv)


print("Ridge Regularization ---------------------------------------")
#Perform ridge regularization 


lam = exp(seq(-2,5,.5))

ridge <- glmnet(select(steve,-famincr), steve$famincr, alpha=0, lambda = lam)
ridge_cv <- cv.glmnet(x=data.matrix(select(steve,-famincr)), y=steve$famincr, nfolds = 10, type.measure="mse", alpha=0)


print("Random Forest ---------------------------------------")

#Implement random forest
steve <- rename(steve, "MiddleEastern" = "Middle Eastern")
steve <- rename(steve, "NativeAmerican" = "Native American")

#steve_sample = sample(steve, 2)
#rf <- randomForest(famincr~., steve, type = "regression", ntree=100, nodesize = 1000)
#rf.cv <- rfcv(trainx = data.matrix(select(steve,-famincr)), trainy = steve$famincr, cv.fold =10, type = "regression", ntree=100, maxnodes = 10)



print("Plots ---------------------------------------")

# Residual plot of OLS

plot_subset <- data.frame(x = steve$famincr, y = OLS$residuals)
plot_subset <- plot_subset[sample(nrow(plot_subset),300),]

resid_plot <- ggplot(plot_subset) +
  geom_point(mapping = aes(x, y), position="jitter") +
  labs(x = "Income", y = "Residuals", 
       title = paste("Residuals of", format(OLS$call)))


# Error as a function of lambda

mean_cv_error = sqrt(ridge_cv$cvm)
upper_cv_error = sqrt(ridge_cv$cvup)
lower_cv_error = sqrt(ridge_cv$cvlo)
lambdas = log(ridge_cv$lambda)

lambda_data = data.frame(x=lambdas, y = mean_cv_error, upper = upper_cv_error, lower = lower_cv_error)

lambda_plot = ggplot(lambda_data, aes(x=x, y=y)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1) +
  geom_point()


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

#Plot both hyper parameters and also no. of features as well 

trees = c(10,50,100, 500, 1000)
nodes = c(10000,5000,1000,500,100)
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
print(RMSE)
#rf.cv <- rfcv(trainx = data.matrix(select(steve,-famincr)), trainy = steve$famincr, cv.fold =10, type = "regression", ntree=100, maxnodes = 10)
