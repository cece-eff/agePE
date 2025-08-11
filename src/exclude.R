exclude_participants <- function(data) {
  # Read the CSV file
  dt_all <- data
  
  #read the exclude file name
  exclude <- read.csv(here::here('cleaned_data', 'excluded_participants.csv'))
  
  # Filter out excluded participants
  dt <- dt_all[!(dt_all$id %in% exclude$id), ]
  
  return(dt)
}
