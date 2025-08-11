##### DATA AND MANIPULATION CHECK ######
## update 8.8.25 CCF

##import cleaned dataframe
ult <- read.csv(here::here('cleaned_data', 'cleaned_data_ultimatum.csv'))

## how many times did each participant see each offer
table(ult$id, ult$offer)

## were there ten total offers and was each offer displayed exactly twice for each participant
table(ult$id, ult$offer_value)
