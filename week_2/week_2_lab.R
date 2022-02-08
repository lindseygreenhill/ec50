# Name: Lindsey Greenhill
# Date: Feb 8, 20211

library(tidyverse)
library(haven)
library(statar)

# reading in data

nlsy <- read_dta("week_2/nlsy97.dta")

########## Question 1 ###########
# Part a

# creating new indicator variable for mother going to college

nlsy <- nlsy %>%
  mutate(mother_less_college = if_else(mother_education <= 12, 1, 0))

# calculating fraction for both groups

nlsy %>%
  group_by(mother_less_college) %>%
  summarise(frac_kid_college = mean(child_college))

# ANSWER: .182 fraction of the sample whose parents did not go to college
# (measured by mother's education) went on to receive a college degree or
# higher.

# Part b 

nlsy %>%
  group_by(mother_less_college) %>%
  summarise(frac_kid_college = sd(child_college))

# ANSWER: I calculated that 18.2% of the children whose parents had a high
# school education or less went on to receive a college degree or higher. The
# standard deviation of this statistic = .386. With this in mind,.182 seems
# close to .209 because .209  is well within one standard deviation of .182.

########## Question 2 ###########

# code taken from lab 1

percentile_rank <- function(variable){ 
  
  #Convert to ranks, taking care of potential missing values 
  r <- ifelse(is.na(variable), NA, rank(variable, ties.method = "average")) 
  
  #Return percentile rank = rank normalized so max is 100 
  100*r/max(r, na.rm = T) 
} 

# part a -- taken from lab 1

nlsy$kid_inc_rank <-with(nlsy, percentile_rank(kid_income)) 
nlsy$parent_inc_rank <-with(nlsy, percentile_rank(parent_inc)) 


########## Question 3 ###########

# part a

nlsy %>%
  ggplot(aes(x = parent_inc_rank, y = kid_inc_rank)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Scatter plot of kid_inc_rank vs. parent_inc_rank") +
  theme_classic()


# part b

ggplot(nlsy, aes(x = parent_inc_rank, y = kid_inc_rank)) +
  stat_binmean(n = 20, geom = "point") +
  stat_smooth(method = "lm", se = FALSE) +
  labs(title = "binned scatter plot of kid_inc_rank vs. parent_inc_rank") +
  theme_classic()

# ANSWER: I think that the binned scatter plot is more useful in this instance
# because it is much easier to see the relationship between the two variables.
# In the basic scatter plot, many of the points overlap and it is difficult to
# discern any clear pattern. With that being said, the scatterplot also probably
# gives a more clear picture of the variability in tthe data. 

########## Question 4 ###########

mod <- lm(kid_inc_rank ~ parent_inc_rank, data = nlsy)
summary(mod)

# ANSWER: The intercept of the regression = 31.418. The estimated slope = .372.
# The interpretation of this model is: for every 1 unit increases in the
# parent_inc_rank variable, there is a .372 unit increase in the kid_inc_rank
# variable on average. In simpler words, a 1 point increase in parent income rank
# results in a .372 point increase in the kid income rank on average.
# Additionally, when the parent_income_rank = 0, the kid_income_rank is equal to
# 31.418 on average. 

########## Question 5 ###########
