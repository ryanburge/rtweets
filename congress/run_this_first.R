library(building)
library(tidytext)


load('D://rtweets/all_tw.Rdata')

# 
# ## This is how I built the full data file, for reference. 
# # 
#  load('D://rtweets/congress/rep_tweets.RData')
#  load('D://rtweets/congress/sen_tweets.RData')
#  
#  
# # saveRDS(merged, "all_tw.rds")
#  
#  
#  all <- bind_rows(rep_tweets, sen_tweets)
#  
#  rm(rep_tweets)
#  rm(sen_tweets)
#  
#  demo <- read_csv("congress/party_gender.csv") %>% 
#    select(-X1, -X1_1)
#  
#  merged <- left_join(all, demo)
#  rm(all)
#  
#  merged <- merged %>% 
#    filter(gender != "REMOVE")
# 
# save(merged, file = "all_tw.Rdata")






