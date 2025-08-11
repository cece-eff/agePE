##clean raw pilot data and data manipulation check 
## CCF 12.7.23

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

##remove unnecessary columns at beginning
d1 <- dt[18:length(dt)]
names(d1)[1] <- "id"

##save cleaned data for each task subtype 
    ##ultimatum game
    ult<-clean_ultimatum(d1)
    write.csv(ult, here::here('cleaned_data', 'cleaned_data_ultimatum.csv'), row.names=FALSE)
    
    ##demographics
    demo<-clean_demographics(d1)
    write.csv(demo, here::here('cleaned_data', 'cleaned_data_demo.csv'), row.names=FALSE)
    
    ##emotion classification
    emoclass<-clean_emoclass(d1)
    write.csv(emoclass, here::here('cleaned_data', 'cleaned_data_emoclass.csv'), row.names=FALSE)
