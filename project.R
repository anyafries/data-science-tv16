library(stevedata)
library(tidyverse)
library(knitr)
library(GGally)
data(TV16)

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
str(steve)

steve <- steve %>% 
  mutate(value = 1)  %>% spread(racef, value, fill = 0) %>%  # one-hot encodings of races
  select(-uid,-lrelig,-lcograc,-lemprac)                     # do not need individual ID + remove latent variables

# Look at structure again
str(steve)

# this isn't useful yet I thought I was doing something else lol
col_names <- c(
                'votetrump' = "Voted Trump",
                'age' = "Age",
                'female' = "Female",
                'collegeed' = "College Degree",
                'racef' = "Race",
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
                'racerare' = "Believes racism is rare",
                )

# check number of NAs
kable(colSums(is.na(steve)))

# significant number of NAs
# prediction continuous will be on famincr (income) -> 6521 missing
# prediction binary will be on votetrump -> 19668 missing
# TO DO: need to decide how to handle missing data

# data in column(s) 'state', 'racef' are not numeric so need to remove
# correlation coefficients based on Pearson’s method
ggcorr(select(steve,-c(racef,state)), 
       label = TRUE, label_size = 2,            # label changes
       hjust = 0.9, size = 3, color = "grey50", # text changes
       layout.exp = 3)                          # whitespace on left

### trumpvote
# most strongly corr w trumpvote:
  # - ideo, pid7na, whiteadv, lcograc
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
ggplot(data=steve, aes(x=lcograc)) +
  geom_density() + facet_wrap(vars(votetrump),ncol=1)
ggplot(data=steve, aes(x=racef)) +
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

ggplot(data=steve, aes(x=collegeed,y=famincr)) +
  geom_jitter(height=0.5, width=0.3, alpha=0.1)

ggplot(data=steve, aes(x=racef,y=famincr)) +
  geom_jitter(height=0.5, width=0.3, alpha=0.1)

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
# * kNN
# * decision trees

#### Regression ###
# - SSE/L2 norm
# * OLS
# * OLS with interaction/higher order terms/regularisation

# Last step: see if we can do it on 2020 data 
# -> differing population model?









