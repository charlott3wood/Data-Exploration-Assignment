CleanData<- read.csv("CleanData")
library(purrr)
library(tidyverse)
library(fs)
library(jtools)
library(readr)
library(dplyr)
library(ggplot2)

#Difference in Difference kinda
#Control group that wasn't there 
#Here is the difference in effect between high and low income 
Year <- CleanData %>% 
  mutate(after = month >= as.Date('2015-09-01'),
       treated = HighEarnings > 0)

Year2 <- Year %>% 
  group_by(after, treated) %>%
  summarize(changeofindex = mean(index))

#Regression 
guess <- lm(index ~ after*treated, data = Year)
export_summs(guess, digits = 3)


#Graph
graph2 <- graph %>% group_by(month, HighEarnings)  %>% summarise(mean(index))
grpah2 <- graph2 %>% mutate(HighEarningsWord = ifelse(HighEarnings == 1, "High", "Low"))
grpah2 <- rename(grpah2, Index = 'mean(index)')
grpah2 <- rename(grpah2, Earnings = HighEarningsWord)
grpah2 <- rename(grpah2, Month = month)

ggplot(data=subset(grpah2, Month > as.Date("2013-08-01")), 
       aes(x=Month, y=Index, group = Earnings, color = Earnings))+
  geom_line()+ 
  geom_vline(xintercept = "2015-09-01")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ggtitle("Indexs Over Time Based on Income")
  

