#Gregory Bruich, Ph.D.
#Economics 50
#Harvard University
#Send suggestions and corrections to gbruich@fas.harvard.edu

rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

## load packages
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(tidyvese)) install.packages("tidyverse"); library(tidyverse)

#Opportunity Insights style bar graphs

#Example Bar graph with two bars: height 0.01 and 0.4
data_for_graph <- data.frame(c(0.01, .4), c("Control group", "Treatment group"))  
data_for_graph

# Change name of 1st column of data_for_graph to "Moved"
names(data_for_graph)[1] <- "Moved"

# Change name of 2nd column of data_for_graph to "Group"
names(data_for_graph)[2] <- "Group"

#Look at the data frame we have created
data_for_graph

# Bar graph displaying results
ggplot(data=data_for_graph, aes(x=Group, y=Moved)) +
  geom_bar(stat="identity", fill="navy") +
  labs(y = "Moved Using Experimental Voucher")

ggsave("fig1_test.png")

