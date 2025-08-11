##identify outliers/people to exclude
## CCF 12.7.23, updated 8/8/25 CCF

##import cleaned dataframe
ult <- read.csv(here::here('cleaned_data', 'cleaned_data_ultimatum.csv'))
demo <- read.csv(here::here('cleaned_data', 'cleaned_data_demo.csv'))
emoclass <- read.csv(here::here('cleaned_data', 'cleaned_data_emoclass.csv'))

###identify individuals to exclude
  ##identify those who had same response 95% of time or more (from ultimatum df)
  ntrials = length(unique(na.omit(ult$offer)))
  same <- ult %>% group_by(id, choice) %>% summarize(
    n = n(),
    percent_response = n/ntrials,
    exclude = ifelse(percent_response > .95, 1, 0), 
    label = 'same response'
  )

  ##identify those who said not to use their data (from demo df)
  use <- demo %>% group_by(id, use_data) %>% summarize(
    exclude = ifelse(use_data == 2, 1, 0), 
    label = 'said dont use data'
  )

  ##identify those who didn't place neutral correctly on the dARM (from emoclass df)
  neutral <- emoclass %>% group_by(id, emotion, valence, arousal) %>% filter(emotion == "Neutral") %>% summarize(exclude = if_else(between(valence, -50, 50) & between(arousal, -50, 50), 0, 1),
                                                                                                            label = "did not place neutral correctly")
##create list of participants we are excluding and why
exclude = rbind(same[c(1,5,6)], use[c(1,3,4)], neutral[c(1,5,6)])

# get list of participants to remove
exclude<-exclude %>% filter(exclude == 1)

#save excluded participant table
write.csv(exclude, here::here('cleaned_data', 'excluded_participants.csv'), row.names=FALSE)






##MORE INFO
#get total number of participants excluded
excluded_ps<-unique(exclude_age$id)
length(excluded_ps)
##132 participants removed

##remove excluded participants from cleaned data set
dt_final<-subset(dt, !(dt$id %in% excluded_ps))
length(unique(dt_final$id))
##272 participants in analytic sample