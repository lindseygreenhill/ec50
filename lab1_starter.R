# Name: Lindsye Greenhill
# Economics 50, Harvard University


# This starter code might have some typos.  Please help us find them!
# Send corrections and suggestions to gbruich@fas.harvard.edu


########### R BASICS
# This is a starter script, AKA a template, for your R code for this lab
# assignment In R, comments are lines of code that start with a #. When
# executing the R-code, R will ignore anything starting with #

# For questions that require code, please delete the 'YOUR CODE' comment and
# write the appropriate lines in R The tables at the bottom of the Lab
# Assignment PDF contain most of the relevant lines of code for this assignment,
# so don't get too overwhelmed if you're stuck

# For questions that require a numerical or qualitative response, write your
# answer under the 'ANSWER' comment

# To run a line of code, highlight and click 'Run' on the top of your dataset


############ LAB 1 CODING EXERCISE

library(tidyverse)
library(haven)
library(ggplot2)
library(statar)

## Read data 
nlsy <- read_dta("nlsy97.dta")


#Q1: Create Histogram of kid_income (hint: sample code is found in table 3 of the assignment page)

ggplot(nlsy) +
  geom_histogram(aes(x = kid_income, y = ..density..),
                  bins = 50) +
  theme_classic() +
  labs(title = "Distribution of kid_income")


#Q2: Mean of kid_income

  # YOUR CODE:
mean <- mean(nlsy$kid_income, na.rm = T)
  
  # ANSWER: $70,499.94

#Q3: Fraction below mean

  # YOUR CODE:
frac <- nlsy %>%
  mutate(below_mean = if_else(kid_income < mean, 1, 0)) %>%
  summarise(frac = mean(below_mean))
  
  # ANSWER: 59.6% of the observations have kid_income below the mean of
  # kid_income. This is the case because the data is skewed and not perfectly
  # symmetrical. 


#Q4: Median of kid income reported by summary()

  # YOUR CODE:

median <- median(nlsy$kid_income, na.rm = T)
  
  # ANSWER: The sample median is $58,750. 


#Q5: Standard deviation

  # YOUR CODE:

sd <- sd(nlsy$kid_income, na.rm = T)
  
  # ANSWER: The standard deviation = $59552.02


#Q6: Fraction within 1 and 2 standard deviations of mean

  # YOUR CODE:

data <- nlsy %>%
  mutate(one_sd = if_else(kid_income <= (mean + sd) & kid_income >= (mean - sd), 
                         1, 0),
         two_sd = if_else(kid_income <= (mean + 2*sd) & kid_income >= (mean - 2*sd),
                         1, 0))
frac_one <- mean(data$one_sd)
frac_two <- mean(data$two_sd)

  # ANSWER: 78.67% of the data falls within 1 standard deviation of the mean and
  # 94.89% of the data falls within 2 standard deviations of the mean.


#Q7: Generate percentile ranks

  # YOUR CODE: 

percentile_rank<-function(variable){ 
  
  #Convert to ranks, taking care of potential missing values 
  r <- ifelse(is.na(variable), NA, rank(variable, ties.method = "average")) 
  
  #Return percentile rank = rank normalized so max is 100 
  100*r/max(r, na.rm = T) 
} 

nlsy$kid_inc_rank <-with(nlsy, percentile_rank(kid_income)) 

ggplot(nlsy) +
  geom_histogram(aes(x = kid_inc_rank, y = ..density..)) +
  theme_classic() +
  labs(title = "Distribution of kid_inc_rank")

#Compare mean and median in percentile ranks

  # YOUR CODE:

mean_rank <- mean(nlsy$kid_inc_rank, na.rm = T)
median_rank <- median(nlsy$kid_inc_rank, na.rm = T)

# The mean = 50.08 and the median = 50.11. 


#Q8: Bin scatters to find variables related to kid_income

  # YOUR CODE:

# Which of the variables you chose seem to be non-linearly related to kid_income?

  # ANSWER:


#Q9: Random assignment simulation

  #a) Set seed so that simulations are replicable
  set.seed(505050505)

  #Generate uniformly distributed random number between 0 and 1
  
  nlsy$random_number <- runif(length(nlsy$kid_income))
  
  
  #b) (i) Generate new variable: treatment_group

  # YOUR CODE:
  

  #b) (ii) How many observations in treatment group? How many in control group?
  
  
  
  #c) Compute sample mean and sample sd for all variables listed in table 1 
  
    # YOUR CODE for treatment group:
  
    # YOUR CODE for control group:
  
    # ANSWER:
  

