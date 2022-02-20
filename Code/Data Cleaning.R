library(purrr)
library(tidyverse)
library(fs)
library(readr)
library(dplyr)
library(lubridate)

#Loading in Data 
CollegeScoreCardData <- read.csv("Data/CollegeScorecardDataDictionary-09-08-2015.csv")
Id_Names <- read.csv('Data/id_name_link.csv')
MostRecent <- read.csv('Data/Most+Recent+Cohorts+(Scorecard+Elements).csv')
list.files("Data", pattern = 'trends_up_to_')
data_dir <- "Data/Trends"
fs::dir_ls(data_dir)
csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")
csv_files
readr::read_csv(csv_files[1])
Trends <- csv_files %>% 
  map_dfr(read_csv)
bind_rows(Trends)

#Data Cleaning, all choices explained in the Analysis

#Remove Duplicates 
trends2 <- distinct(Trends, schname, monthorweek, keyword, .keep_all = TRUE)

#Join Trends and ID 
TrendsID <- inner_join(trends2, Id_Names, by = 'schname')

#Bachelors Degrees Only
filter(MostRecent, PREDDEG == 3)
Bachelors <- MostRecent %>%  select(UNITID, OPEID, PREDDEG) %>% 
  filter(PREDDEG == 3)

#Combine with Trends 
Bachelors <- rename(Bachelors, opeid = OPEID)
Bachelors <- rename(Bachelors, unitid = UNITID)
TrendsIDBachelors <- inner_join(TrendsID, Bachelors, by = 'opeid')
TrendsIDBachelors <- subset(TrendsIDBachelors, select = -unitid.y)
TrendsIDBachelors <- rename(TrendsIDBachelors, unitid= unitid.x)

#Combine with ID Names 
IDBachelorsOnly <- MostRecent %>%  select(UNITID, OPEID, PREDDEG) %>% 
  filter(PREDDEG == 3)

#Combine Bachelors and ID
IDBachelorsOnly <- rename(IDBachelorsOnly, opeid = OPEID)
IdBachelors <-inner_join(Id_Names, IDBachelorsOnly, by = 'opeid')

#Add in earnings 
filter(MostRecent, PREDDEG == 3)
colnames(MostRecent)
Earnings <- MostRecent %>%  select(md_earn_wne_p10.REPORTED.EARNINGS, UNITID, OPEID) 

#Combine with Trends and Bachelors
Earnings <- rename(Earnings, opeid = OPEID)
Earnings <- rename(Earnings, unitid = UNITID)
TrendsIdBachelorsEarnings<- inner_join(TrendsIDBachelors, Earnings, by = 'opeid')
TrendsIdBachelorsEarnings <- subset(TrendsIdBachelorsEarnings, select = -unitid.y)
TrendsIdBachelorsEarnings<- rename(TrendsIdBachelorsEarnings, unitid= unitid.x)

#Combine with ID 
IDBachelorsEarning <- inner_join(IdBachelors, Earnings, by = 'opeid')
IDBachelorsEarning <- subset(IDBachelorsEarning, select = -unitid.y)

#Make Binary Variable for high earnings a median income of over $100,000 for all 
TrendsIdBachelorsEarnings$HighEarnings <- ifelse(TrendsIdBachelorsEarnings$md_earn_wne_p10.REPORTED.EARNINGS > 62000, "1", "0")

#Make Binary Variable for high earnings a median income of over $100,000 for ID 
IDBachelorsEarning$HighEarnings <- ifelse(IDBachelorsEarning$md_earn_wne_p10.REPORTED.EARNINGS > 62000, "1", "0")

#Group By One Month Per College and Indexes 
TrendsIdBachelorsEarningsGrouped <- TrendsIdBachelorsEarnings %>%
  mutate(date = as.Date(str_sub(monthorweek, 1, 10))) %>%
  group_by(schname, keyword)%>%
  mutate(index = (index - mean(index,na.rm = TRUE)/sd(index, na.rm = TRUE))) %>%
  group_by(month=floor_date(date, "month"), opeid) %>%
  summarize(index = mean(index, na.rm= TRUE))

CleanData <- left_join(TrendsIdBachelorsEarningsGrouped, IDBachelorsEarning, by = 'opeid',)
CleanData <- CleanData %>% drop_na(month)

#Clean Data 
write.csv(CleanData, "CleanData")

