#Gregory Bruich, Ph.D.
#Economics 50, Spring 2022
#Harvard University
#Send suggestions and corrections to gbruich@fas.harvard.edu

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

#Set seed for cross validation and random forests
HUID <- 50505050 #Replace with your HUID
set.seed(HUID)

# Install packages (if necessary) and load required libraries
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(randomForest)) install.packages("randomForest"); library(randomForest)
if (!require(rpart)) install.packages("rpart"); library(rpart)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

#-------------------------------------------------------------------------------
# Primer on loops in the context of steady states
#-------------------------------------------------------------------------------

# Chetty et al. (2020) report the following rank-rank regression pooling all 
# races and genders: Rank_{kids} =  33.31 + 0.351 *  Rank_{parents}
# 
# The average income rank for white parents today is the $57.9$th percentile. 
# We predict that the average white child will reach the following percentile 
# when they are adults:

parents_rank <- 57.9
kids_rank <- 33.31 + 0.351 * parents_rank
kids_rank

# For the next generation, the predicted rank for the (average) white grandchild
# is:

parents_rank = kids_rank
kids_rank = 33.31 + 0.351 * parents_rank
kids_rank

# Now we may want to study the predictions from this model across many 
# generations: the grandchilden, the great grandchildren, and the 
# great great grand children, and so on.  We can do this using a loop

generations <- seq(1,7,1) #This is for generations 1 through 7

parents_rank = 57.9 #This is the starting value for the parent's generation

# use a for loop to run the experiment
for(j in generations){ 
  #Calculate kid's predicted rank
  kids_rank <- 33.31 + 0.351 * parents_rank
  
  #Print the output to the console
  print(paste0("In generation ", j, ", parent_rank = ", parents_rank, ", child_rank = ", kids_rank))
  
  #Set parent's rank equal to kids' rank so we are ready for the next iteration
  parents_rank <- kids_rank
}

# Notice that the model predicts that the average white child will reach the
# 51.3rd percentile in generation 7, which is lower than where the first 
# generation started from (which you will recall was the $57.9$th percentile).  
# This is a reduction of around 6 percentiles.
# 
# Meanwhile, the average income rank for Black parents is the 32.7th percentile.
# If we apply the same rank-rank relationship between Black parents and Black 
# children that we did above, our model predicts that the average Black child 
# will reach the 44.9th percentile when they grow up, with further gains for 
# future generations.

generations <- seq(1,7,1) #This is for generations 1 through 7
parents_rank = 32.7 #This is the starting value for the parent's generation
for(j in generations){ 
  kids_rank <- 33.31 + 0.351 * parents_rank
  print(paste0("In generation ", j, ", parent_rank = ", parents_rank, ", child_rank = ", kids_rank))
  parents_rank <- kids_rank
}

# As you can see, this model predicts substantial improvements in outcomes for
# Black children across generations, with average gains of 12 percentiles in a 
# single generation.  The model makes the unrealistic prediction of convergence
# in outcomes across racial groups: By generation 7, both white and Black children
# are at the 51.3 percentile.  This result is unrealistic because racial disparities have persisted for many generations in the United States.
# 
# However, the model is incorrect: we know from Lab 2 and Lecture that Black 
# children and white children experience very different rates of upward mobility 
# across generations. In particular, Chetty et al. (2020) report the following 
# rank-rank regression for Black children:
# 
# Rank_{kids} =  25.4 + 0.28 *  Rank_{parents}
# 
# The prediction implied by this rank-rank graph is very different than the 
# previous calculations suggested.  To see this, in the first generation this 
# model predicts:

parents_rank = 32.7
kids_rank = 25.4 + 0.28 * parents_rank
kids_rank

# In stark contrast to the predictions from the calculations earlier, here 
# there is hardly any predicted improvement in the income rank for Black children
# in generation 1.  To see what happens across generations, we can update our 
# loop:

generations <- seq(1,7,1) #This is for generations 1 through 7
parents_rank = 32.7 #This is the starting value for the parent's generation
for(j in generations){ 
  kids_rank = 25.4 + 0.28 * parents_rank
  print(paste0("In generation ", j, ", parent_rank = ", parents_rank, ", child_rank = ", kids_rank))
  parents_rank <- kids_rank
}

# As you can see, the income rank hardly improves at all for the second and 
# third generation, and eventually stabilizes after four generations at the 
# 35.3rd percentile. The 35.3rd percentile is the steady state (or fixed point)
# prediction for Black children, because the model predicts no further 
# improvement once average income reaches this level. 
# 
# In contrast, similar calculations would show that the 54.2nd percentile 
# is the steady state (or fixed point) of the model for white children. 
# These stark steady state gaps show that addressing racial disparities in 
# upward mobility is crucial for lessening racial disparities in the United States. 
# 
# ## Trying it on your own
# 
# Chetty, Hendren, Jones, and Porter (2020) report the following estimates 
# for Hispanic children: 
# 
# Rank_{kids} =  36.14 + 0.26 *  Rank_{parents}
# 
# The average income rank for Hispanic parents is the 36.17th percentile.  
#
# Write your own for loop to study the predictions from the model for Hispanic 
# children over the next 7 generations.
#
# Using the output from your for loop, what is the steady state prediction for 
# Hispanic children?


#-------------------------------------------------------------------------------
# Data set up
#-------------------------------------------------------------------------------

#Open stata data set
atlas_training <- read_dta("atlas_training.dta")
head(atlas_training)

#Store predictor variables which all start with P_*
vars <- colnames(atlas_training[,grep("^[P_]", names(atlas_training))])

vars

#Data frame with just predictors P_* and kfr_pooled_pooled_p25
training <- subset(atlas_training, training==1, vars)
training$kfr_pooled_pooled_p25 <- atlas_training[atlas_training$training==1,]$kfr_pooled_pooled_p25


#-------------------------------------------------------------------------------
# Part 1: Decision tree with cross validation and two hand picked predictors
#-------------------------------------------------------------------------------

# Implement five-fold cross validation to choose the depth of a decision tree 
# that uses two of the predictors (your choice which predictors). Plot the 
# cross-validation out of sample root mean squared prediction error (CV RMSE) 
# versus the depth of the tree.

# Modify the code to change from 10 fold cross validation to 5
# and change the predictors that I use to two others of your choice

##Illustrate cross validation##
#K-fold Cross validation to select tree depth
# setup the experiment
n <- nrow(training) # the number of observations
K <- 10 # the number of `folds'

# create a copy of the training data
cv <- training

# create a vector of fold memberships (random order)
cv$foldid <- rep(1:K,each=ceiling(n/K))[sample(1:n)]

# create an empty data frame of results
B <- seq(1,20,1) #This is for depths for 1 to 20
OOS <- data.frame(fold=rep(NA,K*length(B) ), squarederror=rep(NA,K*length(B) ), maxdepth=rep(NA,K*length(B) )) 

# use a for loop to run the experiment
row <- 0
for(j in B){ 
  for(k in 1:K){ 
    row <- row + 1
    cvtrain <- subset(cv, foldid != k) # train on all but fold `k'
    cvfold <- subset(cv, foldid == k) # fold `k'
    
    # Fit tree on all but fold `k'
    cvtree <- rpart(kfr_pooled_pooled_p25 ~ P_37 + P_56,
                    data=cvtrain, 
                    maxdepth = c(j), 
                    cp=0) 
    
    
    #Get predictions for fold `k'
    predfull <- predict(cvtree, newdata=cvfold) # Get predictions for fold `k'
    
    #Calculate sum of squred errors for obs in fold `k'
    OOS$squarederror[row] <- sum((cvfold$kfr_pooled_pooled_p25 - predfull)^2) # Calculate prediction errors for fold `k'
    
    OOS$maxdepth[row] <- j # store the maxdepth
    
    OOS$fold[row] <- k # store the fold
    
  }
}

#Summarize the results
OOS
summary(OOS)

#Calculate the combined error across folds
ssr <- tapply(OOS$squarederror, OOS$maxdepth, sum)
ssr <- as.data.frame(ssr)
ssr$maxdepth <- seq(1,20,1)

#Calculate the CV RMSE
ssr$rmse <- sqrt(ssr$ssr / nrow(training))

#Display sum across folds for each bandwidth
ggplot(ssr, aes(x=maxdepth,y=rmse)) +
  geom_point() +
  geom_line() +
  labs(y = "Cross Validation RMSE",
       x = "Tree Depth")

ggsave("cv_tree.png")


### What is the optimal tree depth based on this graph?
cv_optimal_depth = YOUR ANSWER GOES HERE

### Modify this example code to estimate tree using the depth you chose above 
### using the full training data
tree <- rpart(kfr_pooled_pooled_p25 ~ P_37 + P_56, 
                      data=training, 
                      maxdepth = cv_optimal_depth, 
                      cp=0) 


#Visualize the fitted decision tree
plot(tree, margin = 0.2)
text(tree, cex = 0.5)

#Calculate predictions for all rows in training sample
y_train_predictions_tree <- predict(tree, newdata=training)


#-------------------------------------------------------------------------------
# Part 2. Random Forest using same two predictors as the tree
#-------------------------------------------------------------------------------

#Two commands to implement random forests: randomForest() and ranger()
#ranger() is quicker

#Random Forest from 1000 Bootstrapped Samples (ntree=100)
#Random Forests may take a while to run!  Be patient!
smallforest <- randomForest(kfr_pooled_pooled_p25 ~ P_37 + P_56, 
                               ntree=1000, 
                               mtry=2,
                               data=training)

#Tuning parameters are ntree and mtry
#ntree is number of trees in your forest
#mtry is the number of predictors considered at each split (default is number of predictors divided by 3)

smallforest #Review the Random Forest Results

#Generate predictions for training data
y_train_predictions_smallforest <- predict(smallforest, newdata=training, type="response")

#-------------------------------------------------------------------------------
# Part 4. Random Forest with the full set of predictors
#-------------------------------------------------------------------------------

#Two commands to implement random forests: randomForest() and ranger()
#ranger() is quicker

#Random Forest from 1000 Bootstrapped Samples (ntree=100)
#Random Forests may take a while to run!  Be patient!
mobilityforest <- randomForest(kfr_pooled_pooled_p25 ~ ., 
                               ntree=1000, 
                               mtry=40,
                               importance=TRUE, ## add importance=TRUE so that we store the variable importance information
                               data=training)

#Tuning parameters are ntree and mtry
#ntree is number of trees in your forest
#mtry is the number of predictors considered at each split (default is number of predictors divided by 3)
#Setting mtry<121 can help in small samples like this one.

mobilityforest #Review the Random Forest Results

#Generate predictions for training data
y_train_predictions_forest  <- predict(mobilityforest, newdata=training, type="response")

### Try changing mtry to 20 (16.5% of predictors), 60 (50% of predictors), or 121 (all predictors)

#-------------------------------------------------------------------------------
# Part 4 What variables are the most important predictors?
#-------------------------------------------------------------------------------

importance(mobilityforest)
varImpPlot(mobilityforest, type=1) #Plot the Random Forest Results

#type	is either 1 or 2, specifying the type of importance measure 
#(1=mean decrease in accuracy, 2=mean decrease in node impurity)


#-------------------------------------------------------------------------------
# Part 5 Calculate and compare the mean squared error in the training sample. 
#-------------------------------------------------------------------------------

## Root mean squared prediction error in  the training sample.
p <- 3
RMSPE <- matrix(0, p, 1)
RMSPE[1] <- sqrt(mean((training$kfr_pooled_pooled_p25 - y_train_predictions_tree)^2, na.rm=TRUE))
RMSPE[2] <- sqrt(mean((training$kfr_pooled_pooled_p25 - y_train_predictions_smallforest)^2, na.rm=TRUE))
RMSPE[3] <- sqrt(mean((training$kfr_pooled_pooled_p25 - y_train_predictions_forest)^2, na.rm=TRUE))

#Display a table of the results
data.frame(RMSPE, method = c("Tree", "Small RF", "Large RF"))  


#-------------------------------------------------------------------------------
# Part 6 Calculate and compare the mean squared error in  the lock box data 
#-------------------------------------------------------------------------------

#Read in data with truth. Truth is kfr_actual in these data
atlas_test <- read_dta("atlas_lockbox.dta")

#Merge with truth to evaluate predictions. 
atlas <- left_join(atlas_test, atlas_training , by="geoid")

#Separate test data set as a separate data frame
test <- subset(atlas, training==0)

#Get predictions for test data
y_test_predictions_tree <- predict(tree, newdata=test)
y_test_predictions_smallforest <- predict(smallforest, newdata=test, type="response")
y_test_predictions_forest  <- predict(mobilityforest, newdata=test, type="response")

#Calculate RMSPE for test data
p <- 3
OOS_RMSPE <- matrix(0, p, 1)
OOS_RMSPE[1] <- sqrt(mean((test$kfr_actual - y_test_predictions_tree)^2, na.rm=TRUE))
OOS_RMSPE[2] <- sqrt(mean((test$kfr_actual - y_test_predictions_smallforest)^2, na.rm=TRUE))
OOS_RMSPE[3] <- sqrt(mean((test$kfr_actual - y_test_predictions_forest)^2, na.rm=TRUE))

# Display table of results
data.frame(OOS_RMSPE, method = c("Tree", "Small RF", "Large RF"))  
