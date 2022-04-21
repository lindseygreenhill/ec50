# Gregory Bruich, Ph.D.
# Economics 50, Harvard University
# Send corrections and suggestions to gbruich@fas.harvard.edu

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

#Set working directory to be location of data
#Session -> set working directory -> choose working directory

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(statar)) install.packages("statar"); library(statar)
if (!require(Synth)) install.packages("Synth"); library(Synth)
if (!require(SCtools)) install.packages("SCtools"); library(SCtools)

# Now all packages should be installed and loaded!

#read in data
#File import data set -> from stata -> browse 
cig <- read_dta("ec50_smoking.dta")


#-------------------------------------------------------------------------------
# Data set up
#-------------------------------------------------------------------------------

#Data prep
#Natural log of state income per capita
cig$lnincome <- log(cig$personal_income)

#Indicator for California
cig$ca <- 0
cig$ca[which(cig$state=="CA")] <- 1
summary(cig$ca)

#Restrict to CA and 38 states that did not have other policy changes
prop99 <- subset(cig, sample == 1)



#-------------------------------------------------------------------------------
# Graphical analyses
#-------------------------------------------------------------------------------


#Subset data to particular time range
cig_narrow <- subset(prop99, year <= 2000)

#Replicate figure 1
ggplot(cig_narrow, aes(x=year,y=pack_sales, 
                       shape= factor(ca, labels = c("Rest of U.S.", "California")))) +
  geom_vline(xintercept=1988) +
  stat_summary(fun.y = "mean",geom="point") +
  stat_summary(fun.y = "mean",geom="line")  +
  labs(x = "Year", y = "Cigarette Consumption", shape = "") +
  theme(legend.position="bottom")
ggsave("plot1.png")


#-------------------------------------------------------------------------------
# Synthetic control: Estimation
#-------------------------------------------------------------------------------

#This example follows: 

#https://www.rdocumentation.org/packages/Synth/versions/1.1-5/topics/synth
#https://www.rdocumentation.org/packages/Synth/versions/1.1-5/topics/dataprep


#https://rpubs.com/danilofreire/synth


#Store FIPS code for treated state
treated_state <- 6 #This is California in the example

# Make a list of control states' fips codes, excluding treated state

#Start with a list of all unique FIPS codes
list_state <- unique(prop99$state_fips)

#Now remove the treated state's FIPS code from the list
list_state <- list_state[list_state != treated_state]


# create matrices from panel data that provide inputs for synth()
prop99 <- as.data.frame(prop99)

dataprep.out<-
  dataprep(
    foo = prop99,
    predictors = c("beer", "lnincome", "cost_per_pack"),
    predictors.op = "mean",
    dependent = "pack_sales",
    unit.variable = "state_fips",
    time.variable = "year",
    special.predictors = list(
      list("pack_sales", 1975, "mean"),
      list("pack_sales", 1980, "mean"),
      list("pack_sales", 1988, "mean")
    ),
    treatment.identifier = treated_state,
    controls.identifier = list_state,
    time.predictors.prior = c(1970:1988),
    time.optimize.ssr = c(1970:1988),
    unit.names.variable = "state",
    time.plot = 1970:2000
  )

## run the synth command to identify the weights
## that create the best possible synthetic 
## control unit for the treated.
synth.out <- synth(dataprep.out)

## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out$solution.w,2)
# contains the unit weights  

## the output from synth opt 
## can be flexibly combined with 
## the output from dataprep to 
## compute other quantities of interest
## for example, the period by period 
## discrepancies between the 
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps

## also there are three convenience functions to summarize results.
## to get summary tables for all information 
## (V and W weights plus balance btw. 
## treated and synthetic control) use the 
## synth.tab() command
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

## to get summary plots for outcome trajectories 
## of the treated and the synthetic control unit use the 
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out, tr.intake = 1989)
dev.copy(png,'prop99_graph1.png')
dev.off()

## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out, tr.intake = 1989)
dev.copy(png,'prop99_graph2.png')
dev.off()

#-------------------------------------------------------------------------------
# Synthetic control: Permutation test
#-------------------------------------------------------------------------------

## This uses the SCtools library which we installed above
## https://cran.r-project.org/web/packages/SCtools/SCtools.pdf


## Generating the placebos takes a long time to run

## run the generate.placebos command to reassign treatment status
## to each unit listed as control, one at a time, and generate their
## synthetic versions. Sigf.ipop = 2 for faster computing time. 
## Increase to the default of 5 for better estimates. 
tdf <- generate.placebos(dataprep.out,synth.out, Sigf.ipop = 2)

## Plot the gaps in outcome values over time of each unit --
## treated and placebos -- to their synthetic controls
p <- plot_placebos(tdf,discard.extreme=TRUE, mspe.limit=10, xlab='Year')
p
dev.copy(png,'prop99_graph3.png')
dev.off()

## Test how extreme was the observed treatment effect given the placebos:
ratio <- mspe.test(tdf)
ratio$p.val
mspe.plot(tdf, discard.extreme = FALSE)
dev.copy(png,'prop99_graph4.png')
dev.off()





