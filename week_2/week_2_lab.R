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

stat_5a <- 31.4183 + (.3728*25)

# part a: 40.74

stat_5b <- (31.4183 + (.3728*100)) - (31.4183 + (.3728*0))

# part b: 37.28. So the sample suggests that the world is more unequal than the
# tax data does

# check this

stat_5c <- nlsy %>%
  filter(parent_inc_rank <= 20) %>%
  mutate(move_up = if_else(kid_inc_rank >=80, 1, 0)) %>%
  summarise(prob = mean(move_up))

#ANSWER: 7.37%

# do I need to adjust for inflation here?

stat_5d <- nlsy %>%
  mutate(earned_more_parents = if_else((kid_income/1.4767) > parent_inc, 1, 0)) %>%
  summarize(frac = mean(earned_more_parents))

# part d: 50.9% of children born in the 1980s earned more than their parents
# after adjusting for inflation

########## Question 6 ###########

# black men stats #

black_men <- nlsy %>%
  filter(female == 0, black == 1)

black_men_mod <- lm(kid_inc_rank ~ parent_inc_rank, data = black_men)
summary(black_men_mod)

stat_6a_b <- 25.70348 + (.29432*25)

# ANSWER: 33.03

stat_6b_b <- (25.70348 + (.29432*100)) - (25.70348 + (.29432*0))

# ANSWER: 29.43

stat_5c_b <- black_men %>%
  filter(parent_inc_rank <= 20) %>%
  mutate(move_up = if_else(kid_inc_rank >=80, 1, 0)) %>%
  summarise(prob = mean(move_up))

# ANSWER: 5.79%

stat_6d_b <- black_men %>%
  mutate(earned_more_parents = if_else((kid_income/1.4767) > parent_inc, 1, 0)) %>%
  summarize(frac = mean(earned_more_parents))

# ANSWER: 48.9

# white men stats #

white_men <- nlsy %>%
  filter(female == 0, white == 1)

white_men_mod <- lm(kid_inc_rank ~ parent_inc_rank, data = white_men)
summary(white_men_mod)

stat_6a_w <- 40.11022 + (.26692*25)

# ANSWER: 46.783

stat_6b_w <- (40.11022 + (.26692*100)) - (40.11022 + (.26692*0))

# ANSWER: 26.692

stat_5c_w <- white_men %>%
  filter(parent_inc_rank <= 20) %>%
  mutate(move_up = if_else(kid_inc_rank >=80, 1, 0)) %>%
  summarise(prob = mean(move_up))

# ANWER: 10.3%  

stat_6d_w <- white_men %>%
  mutate(earned_more_parents = if_else((kid_income/1.4767) > parent_inc, 1, 0)) %>%
  summarize(frac = mean(earned_more_parents))

# ANSWER: 48.3
