# library(tidyverse)
# library(extrafont)
# 
# all <- bind_rows(rep_tweets, sen_tweets)
# 
# demo <- read_csv("congress/party.csv")
# 
# demo <- demo %>% 
#   select(gender, type, birthday, twitter) %>% 
#   rename(screen_name = twitter)
# 
# merged <- left_join(all, demo)
# 
# party <- read_csv("kamrul_small.csv") 
# 
# merged <- left_join(all, party)


thoughts <- subset(merged, grepl("thoughts and prayers", ignore.case = TRUE, text)) 
# 
# at <- 'thoughts and prayers'
# thoughts <- subset(merged, grepl(at, text))


thoughts$date <- format(as.Date(strptime(thoughts$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
thoughts$date <- as.Date(thoughts$date, "%m/%d/%Y")

week <- thoughts %>% 
  group_by(party, week = cut(date, "month"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d"))

cols <- c("Independent" = "grey", "Republican" = "firebrick3", "Democrat" = "dodgerblue3", "Other" = "forestgreen")


week %>% 
  filter(party != "Other") %>% 
  filter(week < "2018-03-01") %>% 
  ggplot(., aes(x=week, y=n)) + geom_col(aes(fill = party), color = "black") +
  theme_minimal() +
  scale_fill_manual(values = cols) +
  theme(legend.position = c(0.7, 0.9)) +
  theme(text=element_text(size=44, family="font")) +
  theme(legend.title = element_blank()) +
  labs(x = "Date Tweeted", y = "Tweets per Month", title = "Which Party Tweets Thoughts and Prayers?", subtitle = "", caption = "Scraped from Twitter's API (6/2008 - 4/2018)") +
  ggsave("D://rtweets/congress/thoughts_party_final.png", dpi = 300, width = 10, height =6, type = "cairo")
  

week$year <- format(as.Date(week$week, format="%Y/%m/%d"),"%Y")

week %>% 
  group_by(party, year) %>% 
  summarise(total = sum(n)) %>% 
  as.data.frame()
  

