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
library(skimr)
library(statar)

## Read data 

nlsy <- read_dta("nlsy97.dta")


#Q1: Create Histogram of kid_income (hint: sample code is found in table 3 of
#the assignment page)

ggplot(nlsy) +
  geom_histogram(aes(x = kid_income, y = ..density..),
                  bins = 50) +
  theme_classic() +
  labs(title = "Distribution of kid_income")


#Q2: Mean of kid_income

mean <- mean(nlsy$kid_income, na.rm = T)
  
  # ANSWER: $70,499.94

#Q3: Fraction below mean

frac <- nlsy %>%
  mutate(below_mean = if_else(kid_income < mean, 1, 0)) %>%
  summarise(frac = mean(below_mean))
  
  # ANSWER: 59.6% of the observations have kid_income below the mean of
  # kid_income. This is the case because the data is skewed and not perfectly
  # symmetrical. 


#Q4: Median of kid income reported by summary()


median <- median(nlsy$kid_income, na.rm = T)
  
  # ANSWER: The sample median is $58,750. 


#Q5: Standard deviation

sd <- sd(nlsy$kid_income, na.rm = T)
  
  # ANSWER: The standard deviation = $59552.02


#Q6: Fraction within 1 and 2 standard deviations of mean


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

percentile_rank <- function(variable){ 
  
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


mean_rank <- mean(nlsy$kid_inc_rank, na.rm = T)

median_rank <- median(nlsy$kid_inc_rank, na.rm = T)

# The mean = 50.08 and the median = 50.11. 

#Q8: Bin scatters to find variables related to kid_income

# this looks linear

ggplot(nlsy, aes(x = child_education, y = kid_income)) +
  stat_smooth(method = "lm", se = FALSE) +
  stat_binmean(n = 20, geom = "point")

# this looks more exponential than linear

ggplot(nlsy, aes(x = parent_inc, y = kid_income)) +
  stat_smooth(method = "lm", se = FALSE) +
  stat_binmean(n = 20, geom = "point")

# this looks relatively linear but not as good

ggplot(nlsy, aes(x = mother_education, y = kid_income)) +
  stat_smooth(method = "lm", se = FALSE) +
  stat_binmean(n = 20, geom = "point")

# this looks relatively linear

ggplot(nlsy, aes(x = child_sat, y = kid_income)) +
  stat_smooth(method = "lm", se = FALSE) +
  stat_binmean(n = 20, geom = "point")

# Which of the variables you chose seem to be non-linearly related to kid_income?

  # ANSWER: child_education seems to be nonlinear and parent_income seems to be
  # nonlinear (I think it looks more exponential). 


#Q9: Random assignment simulation

  #a) Set seed so that simulations are replicable
  set.seed(31305187)

  #Generate uniformly distributed random number between 0 and 1
  
  nlsy$random_number <- runif(length(nlsy$kid_income))
  
  
  #b) (i) Generate new variable: treatment_group

  # YOUR CODE:
nlsy <- nlsy %>%
  mutate(treatment_group = if_else(random_number >= .5, 1, 0))

# finding numbers for treatment and control groups

nlsy %>%
  group_by(treatment_group) %>%
  count()



  #b) (ii) How many observations in treatment group? How many in control group?
  # there are 2711 observations in the treatment group and 2775 in the control. 
  
  
  
  #c) Compute sample mean and sample sd for all variables listed in table 1 

options(dplyr.width = Inf)

# parent_inc mean for control = $46848 and treatment = $45967

nlsy %>%
  group_by(treatment_group) %>%
  summarise_all("mean")

# parent_inc sd for control = $46824 and treatment = $45329

nlsy %>%
  group_by(treatment_group) %>%
  summarise_all("sd")
  
    # ANSWER:The mean of the parent_inc variable in the control group = $46848
    # and in the treatment group = $45967. The sd of the parent_inc variable in
    # the control group = $46824 and in the treatment group = $45329. 

# what is the pirpose of random assignment in an experiment? The purpose of
# random assignment in this experiment is to eliminate potential confounding
# variables and create comparable groups. I would 100% prefer to use random
# assignment as opposed to assigning groups myself because doing it myself would
# probably result in bias of some sort and because it was not random I could not
# make any causal claims. Additionally, even if I assigned groups according to a
# few specified control variables, I might not create comparable groups with
# regards to other factors that I have not specifically acounted for in my
# sorting. As such, the best way to  create comparable treatment groups is
# random assignment.
  

