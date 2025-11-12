##clean raw pilot data and data manipulation check 
## CCF 12.7.23 updated KLS 11.11.25 

##install packages
library(readr)
library(dplyr)
library(tidyverse)
library(here)

# Load source functions
source(here::here('src', 'cleaning_functions.R'))

#import Qualtrics datasets from 
  ## younger adults: import YA data, add column with age group, remove first two rows
  ya <- read_csv(here("raw_data", "agePE_YA_March 4, 2024_13.00.csv"))
  ya['age_group']='ya'
  ya<-ya[-c(1,2) , ]
  
  ## middle-aged adults: import MA data, add column with age group, remove first two rows
  ma<-read_csv(here("raw_data","agePE_MA_March 4, 2024_13.05.csv"))
  ma['age_group']='ma'
  ma<-ma[-c(1,2) , ]
  
  ## older adults: import OA data, add column with age group, remove first two rows
  oa<-read_csv(here("raw_data","agePE_OA_March 4, 2024_13.55.csv"))
  oa['age_group']='oa'
  oa<-oa[-c(1,2) , ]
  
#merge dataframes
dt<-bind_rows(ya, ma, oa)
rm(ya, ma, oa)

##remove unnecessary columns at beginning
d1 <- dt[18:length(dt)]
names(d1)[1] <- "id"
rm(dt)

##cleaned data for each task subtype and recombine
##ultimatum game
ult<-clean_ultimatum(d1)

##demographics
demo<-clean_demographics(d1)

##emotion classification
emoclass<-clean_emoclass(d1)

##depression 
cesd<-clean_and_score_cesd(d1)

## combine
d2 <- left_join(demo, emoclass, by = 'id')
d3 <- left_join(demo, ult, by = 'id')
d4 <- left_join(demo, cesd, by = 'id')
d5 <- bind_rows(d2, d3)
d6 <- bind_rows(d5, d4)

## order by id
d6 <- arrange(d6, id)

# save new file with data in long format
write.csv(d6, here('data', 'agePE_data.csv'), row.names = FALSE)