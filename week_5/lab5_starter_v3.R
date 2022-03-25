#Gregory Bruich, Ph.D.
#Economics 50, Spring 2022
#Harvard University
#Send suggestions and corrections to gbruich@fas.harvard.edu


rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(rdrobust)) install.packages("rdrobust"); library(rdrobust)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)


#set working directory
dat<- read_dta("probation.dta")

#Generate centered version of GPA
dat$dist_from_cut <- dat$GPA - 1.6

##Subset data to [-1.2,1.2]
dat_narrow <- subset(dat,dist_from_cut<=1.2 & dist_from_cut>=-1.2)

####*Binscatter plot for outcome: graduating in 4 years
rdplot(dat_narrow$gradin4, #outcome variable
       dat_narrow$dist_from_cut, #running variable
        p = 1, #p = 1 is linear best fit line. p >= 2 is nonlinear
        nbins = c(20, 20), #number of bins on each side of threshold
        binselect = "es", #option to use "equal spaced" binning
        y.lim = c(0, 0.6), #Set y-axis scale
        x.label = "Grade Point Average minus 1.6", 
        y.label = "Fraction Graduating in 4 years"
       )

ggsave("gradin4.png")



#To estimate the discontinuity requires generating some additional variables
#generate indicator for being above probation threshold
dat$T <- 0
dat$T[which(dat$dist_from_cut >= 0)] <- 1

#Generate interaction term for linear
dat$interaction <- dat$dist_from_cut*dat$T

##Subset data to [-1.2,1.2] with new variables added
dat_narrow <- subset(dat,dist_from_cut<=1.2 & dist_from_cut>=-1.2)

#Estimate regression
linear <- lm(gradin4 ~ T + dist_from_cut + interaction , data = dat_narrow)
coeftest(linear, vcov = vcovHC(linear, type="HC1"))

#Coefficient of interest is coefficient on T = indicator for being above probation threshold





