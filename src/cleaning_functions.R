##CCF 7.29.25

####### ULTIMATUM GAME CLEANING ######### 
clean_ultimatum <- function(dt) {
  ##create df for just ultimatum game data 
  ult<-d1[c(1, 68:187,210:229)]
  ult<-ult %>% mutate_at(2:141, as.numeric)
  ult[is.na(ult)]<-0 ##replace NAs with 0, Qualtrics didn't register values that were exactly 0,0 on the darm so need to insert them
  
  ##calculate prediction error of all types, arousal needs to be multiplied by -1 because on the DARM used, low arousal reflects high location values so needs to be reversed
  offers<-d1[c(1,210:229)] #get list of offers
  offers<- offers %>% pivot_longer(cols='offer1b':'offer20b', names_to = "offer", values_to ="offer_value")
  offerlist<- unique(offers$offer)
  for (x in 1:length(offerlist)) {
    rpe <- ult[,paste0(offerlist[x])] - ult[,paste0(offerlist[x], '_pr_1')]
    ult[ , paste0("rpe_offer", x)]<-rpe
    vpe<- ult[,paste0(offerlist[x], '_ee_1_x')] - ult[,paste0(offerlist[x], '_pe_1_x')]
    ult[ , paste0("vpe_offer", x)]<-vpe
    ape<- (ult[,paste0(offerlist[x], '_ee_1_y')] - ult[,paste0(offerlist[x], '_pe_1_y')])*-1
    ult[ , paste0("ape_offer", x)]<-ape
  }
  
  ##wide to long format
  ##fix offers 
  offers$offer<- str_replace_all(offers$offer, "b", "")
  ##reward
  rpe<-ult %>% select(1, starts_with("rpe"))
  rpe<- rpe %>% pivot_longer(cols='rpe_offer1':'rpe_offer20', names_prefix = 'rpe_', names_to = 'offer',values_to ='rpe')
  ##valence
  vpe<-ult %>% select(1, starts_with("vpe"))
  vpe<- vpe %>% pivot_longer(cols='vpe_offer1':'vpe_offer20', names_prefix = 'vpe_',names_to = 'offer', values_to ='vpe')
  ##arousal
  ape<-ult %>% select(1, starts_with("ape"))
  ape<- ape %>% pivot_longer(cols='ape_offer1':'ape_offer20', names_prefix = 'ape_', names_to = 'offer', values_to ='ape')
  ##choice
  choicevars<-ult %>% select(1, ends_with("choice"))
  choice<- choicevars %>% pivot_longer(cols='offer1b_choice':'offer20b_choice', names_to = "offer", values_to ="choice")
  choice$offer <- str_replace_all(choice$offer, "b_choice", "")
  choice$choice[choice$choice == 2]<- 'accept'
  choice$choice[choice$choice == 1]<- 'punish'
  choice$choice[choice$choice == 'punish']<- 1
  choice$choice[choice$choice == 'accept']<- 0
  choice$choice<-as.factor(choice$choice)
  
  ##combine data frames into one
  df_list<-list(offers, rpe, vpe, ape, choice)
  full<- df_list %>% reduce(full_join, by=c('id', 'offer'))
  
  return(full)
}

######### DEMOGRAPHICS CLEANING ############
  clean_demographics <- function(demo){
    demo<-d1[c(1, 6:21, 208, 230)]

  names(demo)[1] <- "id"
  names(demo)[2] <- "dob_m"
  names(demo)[3] <- "dob_d"
  names(demo)[4] <- "dob_y"
  names(demo)[5] <- "age"
  names(demo)[6] <- "sex"
  names(demo)[7] <- "gender"
  names(demo)[8] <- "gender_other"
  names(demo)[9] <- "ethnicity"
  names(demo)[10] <- "race"
  names(demo)[11] <- "education"
  names(demo)[12] <- "health"
  names(demo)[13] <- "household_n"
  names(demo)[14] <- "household_income"
  names(demo)[15] <- "occupation"
  names(demo)[16] <- "ladder_US"
  names(demo)[17] <- "ladder_community"
  names(demo)[18] <- "use_data"
  names(demo)[19] <- "age_group"
  
  ##ordinal grouping for education
  demo$education[demo$education <= 14] <- 1 #"less than high school"
  demo$education[demo$education == 16] <- 2 #"GED"
  demo$education[demo$education == 15] <- 3 #"high school graduate"
  demo$education[demo$education == 17 | demo$education == 23 |demo$education == 24 |demo$education == 25 | demo$education == 26] <- 4 #"some college"
  demo$education[demo$education == 18] <- 5 # "associates"
  demo$education[demo$education == 19] <- 6 #"bachelor's"
  demo$education[demo$education == 20] <- 7 #"master's"
  demo$education[demo$education == 21] <- 8 #"professional"
  demo$education[demo$education == 22] <- 9 #"doctorate"
  
  ##data dictionary/add labels 
  #demo$sex[demo$sex == 1]<- "male"
  #demo$sex[demo$sex == 2]<- "female"
  #demo$gender[demo$gender == 1]<- "male"
  #demo$gender[demo$gender == 2]<- "female"
  #demo$gender[demo$gender == 3]<- "transgender"
  #demo$ethnicity[demo$ethnicity == 1]<- "Hispanic/Latino"
  #demo$ethnicity[demo$ethnicity == 2]<- "Not Hispanic/Latino"
  #demo$race[demo$race == 1]<- "Caucasian/White"
  #demo$race[demo$race == 2]<- "Black/AA"
  #demo$race[demo$race == 3]<- "Asian"
  #demo$race[demo$race == 4]<- "American Indian/Alaska Native"
  #demo$race[demo$race == 5]<- "Native Hawaiian/Pacific Islander"
  #demo$race[demo$race == 6]<- "Multiracial"
  #demo$race[demo$race == 7]<- "Other"
  #demo$education[demo$education <= 14] <- "less than high school"
  #demo$education[demo$education == 15] <- "high school graduate"
  #demo$education[demo$education == 16] <- "GED"
  #demo$education[demo$education == 17] <- "some college credit, less than 1 year"
  #demo$education[demo$education == 18] <- "associates"
  #demo$education[demo$education == 19] <- "bachelor's"
  #demo$education[demo$education == 20] <- "master's"
  #demo$education[demo$education == 21] <- "professional"
  #demo$education[demo$education == 22] <- "doctorate"
  #demo$education[demo$education == 23] <- "one or more yaers at a 2-year program, no degree"
  #demo$education[demo$education == 24] <- "one year of college at 4-year program, no degree"
  #demo$education[demo$education == 25] <- "two years of college at 4-year program, no degree"
  #demo$education[demo$education == 26] <- "three years of college at 4-year program, no degree"
  #demo$health[demo$health == 1] <- "very healthy"
  #demo$health[demo$health == 2] <- "quite healthy"
  #demo$health[demo$health == 3] <- "moderately healthy"
  #demo$health[demo$health == 4] <- "slightly healthy"
  #demo$health[demo$health == 5] <- "not at all healthy"
  #demo$household_income[demo$household_income == 1] <- "<$15,000"
  #demo$household_income[demo$household_income == 2] <- "$15,000-$24,999"
  #demo$household_income[demo$household_income == 3] <- "$25,000-$34,999"
  #demo$household_income[demo$household_income == 4] <- "$35,000-$44,999"
  #demo$household_income[demo$household_income == 5] <- "$45,000-$59,999"
  #demo$household_income[demo$household_income == 6] <- "$60,000-$79,999"
  #demo$household_income[demo$household_income == 7] <- "$80,000-$99,999"
  #demo$household_income[demo$household_income == 8] <- "$100,000-$149,999"
  #demo$household_income[demo$household_income == 9] <- "$150,000-$199,999"
  #demo$household_income[demo$household_income == 10] <- "$200,000-$249,999"
  #demo$household_income[demo$household_income == 11] <- "$250,000-$499,999"
  #demo$household_income[demo$household_income == 12] <- ">$500,000"
  
  return(demo)
}

######EMO CLASSIFICATION CLEANING #############
  clean_emoclass <- function(emo_class){
    emoclass<-d1[c(1,22:61)]
    emo<- emoclass %>% pivot_longer(cols= !id, names_to = "emotion", values_to ="location")
    emo<-emo %>% separate(emotion, c("emotion", "dimension"), "_1_")
    emo<- emo %>% pivot_wider(names_from = "dimension", values_from = "location")
    emo$x<-as.numeric(emo$x)
    emo$y<-as.numeric(emo$y)
    emo$valence<-emo$x - 250
    emo$arousal<-emo$y - 250
    
    return(emo)
  }
  
  ### CLEAN AND SCORE CES-d ####
  clean_and_score_cesd <- function(data) {
    dt<-data[c(1, 188:207)]
    # rename columns
    dt <- dt %>%
      rename_with(~ gsub("Q386_", "CESd_", .x))
    # convert values to numeric
    dt<-dt %>% mutate(across(c(CESd_1:CESd_20), as.numeric))
    
    #recode
    dt[dt =="1"]<-0
    dt[dt =="2"]<-1
    dt[dt =="3"]<-2
    dt[dt =="4"]<-3
    
    #reverse code items 4, 8, 12, and 16
    dtnew<-dt %>% mutate(q4rev = 3 - CESd_4,
                         q8rev = 3 - CESd_8,
                         q12rev = 3 - CESd_12,
                         q16rev = 3 - CESd_16)
    
    
    ##sum score w/o old items
    sum<-dtnew %>% mutate(Score_CESd = rowSums(select(., -c(id, CESd_4, CESd_8, CESd_12, CESd_16)), na.rm=TRUE))
    
    CESd <- sum %>%  mutate(Category_CESd = ifelse(Score_CESd > 16, "Depressed", "Healthy"))
    
    CESd <- CESd %>% pivot_longer(
      starts_with("CESd_"),
      names_to = "CESd_item",
      values_to = "CESd_rating"
    )
    CESd <- CESd %>% select(-q4rev, -q8rev, -q12rev, -q16rev)
    CESd <- CESd %>% relocate(c(id, CESd_item:CESd_rating))

    return(CESd)
  }
  