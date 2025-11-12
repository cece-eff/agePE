# create a data dictionary
# KLS 11.11.25

# load required packages
library(here)
library(tidyverse)

# load source functions

# set hard-coded variables

# read data in 
dt <- read.csv(here::here('data', 'agePE_data.csv'))

# create data dictionary 
dd <- t(dt[1,])
dd <- as.data.frame(rownames(dd))
colnames(dd) <- c('Variable')

# make factors
dt <- dt %>% mutate(across(c(id, sex, gender, gender_other, ethnicity, race, education, 
                             health, household_n, household_income, occupation, ladder_US,
                             ladder_community, use_data, age_group, emotion, offer, choice, CESd_item, Category_CESd ), as.factor))

# create and populate measurement units ####
units <- sapply(dt, class)
units[2] <- "MM"
units[3] <- "DD"
units[4] <- "YY"
units[5] <- "years"

# Create and populate allowed_values field ####
emotions <- levels(dt$emotion) %>% str_c(collapse = ", ")
allowed_values = c("alpha-numeric values", "1-12", "1-31", "", "positive integer", "1 = Male or 2 = Female", #Sex
                   "1 = Male, 2 = Female, 3 = Transgender, 4 = None of the above (self identify below", " ", #gender
                   "1 = Hispanic or Latino or 2 = not Hispanic or Latino", #ethnicity
                   "1 = Caucasian/White, 2 = Black or African American, 3 = Asian, 4 = American Indian/Alaska Native, 5 = Native Hawaiian or other Pacific Islander, 6 = Multiracial, 7 = Other", #race
                   "1 = less than high school, 2 = GED, 3 = high school graduate, 4 = some college, 5 = associates, 6 = bachelors, 7 = masters, 8 = professional, 9 = doctorate", #education
                   "1 = very healthy, 2 = quite healthy, 3 = moderately healthy, 4 = slightly healthy, 5 = not at all healthy", "positive integer", 
                   "1 = <$15,000, 2 = $15,000-$24,999, 3 = $25,000-$34,999, 4 = $35,000-$44,999, 5 = $45,000-$59,999, 6 = $60,000-$79,999, 7 = $80,000-$99,999, 8 = $100,000-$149,999, 9 = $150,000-$199,999, 10 = $200,000-$249,999, 11 = $250,000-$499,999, 12 = >$500,000", 
                   "", "1-10", "1-10", "1 = Yes or 2 = No ", "ya, ma or oa", #age group
                   levels(dt$emotion) %>% str_c(collapse = ", "), "positive integer", "positive integer", "integer", "integer", "offer1-offer20", #offer
                   "positive decimals", "-0.5-0.5", "-500-500", "-500-500", "0 = reject or 1 = accept ", "CEDs1-CEDs20", 
                   "0 = Rarely or None of the Time (< 1 Day), 1 = Some or a Little of the Time (1-2 Days), 2 = Occasionally or a Moderate Amount of the Time (3-4 days), 3 = Most or All of the Time (5-7 Days)", 
                   "0-60", "Healthy or Depressed"
                   )

#create and populate description field ####
description <- c("ID number assigned to participant by Qualtrics", "Participant's Date of Birth - Month", "Participant's Date of Birth - Day", 
                 "Participant's Date of Birth - Year", "Age of participants in years", "Participant's Sex assigned at Birth", "Participant's Gender", " ", 
                 "Participant's Ethnicity", "Participant's Race", "Participant's Education", "Self-reported health", "Number of people in participant's household", 
                 "Participant's household income", "Participant's occupation", "Participant's self-reported SES relative to US", "Participant's self-reported SES relative to community", 
                 "Should we use your data?", "Participant's age group", "Emotion rated on that trial", 
                 "x coordinate on dARM", "y coordinate on dARM", "valence rating on dARM", "arousal rating on dARM", 
                 "Ultimatium game offer number", "How much money is offered on this trial of Ultimatium game", 
                 "reward prediction error", "valence prediction error", "arousal prediction error", "accept or reject the offer", 
                 "Center for Epidemologic Studies Depression Scale (CES-D) item number", 
                 "CED-S Rating", "CED-Score, higher score indicate more symptoms", "Depression Diagnosis based on CED-S")

dd <- dd %>% mutate(
  units,
  allowed_values, 
  description
)

# save file ####
write.csv(dd, here::here('data', 'agePE_data_dictionary.csv'), row.names = FALSE)
