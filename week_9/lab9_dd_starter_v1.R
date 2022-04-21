# Gregory Bruich, Ph.D.
# Economics 50, Harvard University
# Send corrections and suggestions to gbruich@fas.harvard.edu

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(statar)) install.packages("statar"); library(statar)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)

# Now all packages should be installed and loaded!

#Set working directory to be location of data
#Session -> set working directory -> choose working directory

#read in data
#File import data set -> from stata -> browse 
cig <- read_dta("ec50_smoking.dta")


#-------------------------------------------------------------------------------
# Data set up
#-------------------------------------------------------------------------------

#Data prep

#Define Arizona indicator
cig$az <- 0
cig$az[which(cig$state=="AZ")] <- 1
summary(cig$az)



#-------------------------------------------------------------------------------
# Graphical analyses
#-------------------------------------------------------------------------------


#I'll start with a new data frame that includes all 51 states in years 1987 to 2000
cig_narrow <- subset(cig, year>=1987 & year <= 2000)

#Replicate figure 1
ggplot(cig_narrow, aes(x=year,y=pack_sales,
                       shape= factor(az, labels = c("Rest of U.S.", "Arizona")))) +
  geom_vline(xintercept=1993.5) +
  stat_summary(fun = "mean",geom="point") +
  stat_summary(fun = "mean",geom="line")  +
  labs(x = "Year", y = "Cigarette Consumption", shape = "") +
  theme(legend.position="bottom")
ggsave("plot1.png")


#Compare Arizona to only neighboring states
#California, Nevada, Utah, New Mexico, and Colorado
cig_narrow <- subset(cig_narrow, state == "AZ" 
                     | state == "CA" 
                     | state == "NV" 
                     | state == "UT" 
                     | state == "NM" 
                     | state == "CO")


#Replicate figure 2
ggplot(cig_narrow, 
       aes(x=year,y=pack_sales, 
           shape = factor(az, labels = c("Surrounding States", "Arizona")))) +
  geom_vline(xintercept=1993.5) +
  stat_summary(fun = "mean",geom="point") +
  stat_summary(fun = "mean",geom="line")  +
  labs(x = "Year", y = "Cigarette Consumption", shape = "") +
  theme(legend.position="bottom")
ggsave("plot2.png")


#-------------------------------------------------------------------------------
# Regression analysis to quantify what we see in the graphs
#-------------------------------------------------------------------------------


#Regression example to modify

#These regressions are run over the neighboring states
#California, Nevada, Utah, New Mexico, and Colorado

#Generate post 1994 indicator variable
cig_narrow$post <- 0
cig_narrow$post[which(cig_narrow$year >= 1994)] <- 1

#Generate interaction term
cig_narrow$dd <- cig_narrow$az*cig_narrow$post

#Subset to years you want
#cig_narrow2 only includes the years right around the policy change
cig_narrow2 <- subset(cig_narrow, year>=1993 & year <= 1995)

#Estimate simple differences in differences
reg1 <- lm(pack_sales ~ post + az + dd, data=cig_narrow2)

#Report heteroskedasticity robust standard errors
coeftest(reg1, vcov = vcovHC(reg1, type="HC1"))

#What if we use more years? cig_narrow includes the 1987-2000
#Estimate difference in difference regression extended to multiple control states and many years
#factor(year) generates a fixed effect for each year, excluding one as the reference level
#factor(state) generates a fixed effect for each state, excluding one as the reference level
reg2 <- lm(pack_sales ~ dd + factor(year) + factor(state_fips), 
           data=cig_narrow)
coeftest(reg2, vcov = vcovHC(reg2, type="HC1"))

