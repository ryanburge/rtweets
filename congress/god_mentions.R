
god <- subset(all, grepl("god|jesus|christ", ignore.case = TRUE, text)) 

god <- god %>% 
  filter(screen_name != "PrayerCaucus")


god$date <- format(as.Date(strptime(god$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
god$date <- as.Date(god$date, "%m/%d/%Y")

week <- god %>% 
  group_by(week = cut(date, "week"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d"))


all$date <- format(as.Date(strptime(all$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
all$date <- as.Date(all$date, "%m/%d/%Y")

allweek <- all %>% 
  group_by(week = cut(date, "week"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d")) %>% 
  rename(total = n)

graph <- left_join(allweek, week) %>% mutate(pct = n/total) %>% na.omit()


graph %>% 
  ggplot(., aes(x=week, y=pct)) + geom_col(color = "black", fill = "firebrick3") +
  labs(x = "Date Tweeted", y = "Percent of Weekly Tweets", title = "When Congress Gets Religious on Twitter", subtitle = "Counting Mentions of God, Jesus, or Christ", caption = "Scraped from Twitter's API (6/2008 - 4/2018)") +
  bar_rb() + scale_y_continuous(labels=function(x) paste0(x,"%"))

ggsave(file="D://rtweets/congress/god_mentions.png", type = "cairo-png", width = 21, height = 15)




