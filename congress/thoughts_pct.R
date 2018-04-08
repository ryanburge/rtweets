all$date <- format(as.Date(strptime(all$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
all$date <- as.Date(all$date, "%m/%d/%Y")

week_all <- all %>% 
  group_by(week = cut(date, "week"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d")) %>% 
  rename(total = n)


new <- left_join(week, week_all) %>% 
  mutate(pct = n/total)

new %>% 
  ggplot(., aes(x=week, y=pct)) + geom_col(color = "black", fill = "firebrick3") +
  labs(x = "Date Tweeted", y = "Tweets per week", title = "Thoughts and Prayers?", subtitle = "Assesing How Often Members of Congress Tweet Thoughts and Prayers", caption = "Scraped from Twitter's API (6/2008 - 4/2018)") +
  bar_rb() +
  annotate("text", x=as.Date("2017-10-01"), y = 95, label = "Las Vegas", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2018-02-14"), y = 106, label = "Stoneman Douglas", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2013-04-15"), y = 106, label = "Boston Bombing", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2016-06-12"), y = 60, label = "Pulse Nightclub", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2013-09-15"), y = 57, label = "Paris", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2017-06-14"), y = 97, label = "Scalise", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2015-06-17"), y = 61, label = "Charleston", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2017-11-05"), y = 52, label = "Sutherland Springs", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2009-11-05"), y = 7, label = "Fort Hood", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2016-07-14"), y = 50, label = "Nice, France", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2016-03-12"), y = 77, label = "Brussels", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2015-11-01"), y = 80, label = "San Bernardino", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2014-03-08"), y = 60, label = "Malaysia Flight 370", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2013-08-01"), y = 29, label = "DC Navy Yard", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2013-05-20"), y = 76, label = "OK Tornadoes", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2012-07-16"), y = 36, label = "Aurora", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2012-12-10"), y = 44, label = "Sandy Hook", size = 6, family = "Product Sans")