library(tidyverse)
library(extrafont)

all <- bind_rows(rep_tweets, sen_tweets)

demo <- read_csv("congress/party.csv")

demo <- demo %>% 
  select(gender, type, birthday, twitter) %>% 
  rename(screen_name = twitter)

merged <- left_join(all, demo)

party <- read_csv("kamrul_small.csv") 

merged <- left_join(all, party)


at <- 'thoughts and prayers'
thoughts <- subset(merged, grepl(at, text))


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
  ggplot(., aes(x=week, y=n)) + geom_col(aes(fill = party), color = "black") +
  labs(x = "Date Tweeted", y = "Tweets per week", title = "Thoughts and Prayers?", subtitle = "Assesing How Often Members of Congress Tweet Thoughts and Prayers", caption = "Scraped from Twitter's API (6/2008 - 4/2018)") +
  bar_rb() + scale_fill_manual(values = cols) 

ggsave(file="D://rtweets/congress/thoughts_party_month.png", type = "cairo-png", width = 21, height = 15)

  