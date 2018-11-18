library(tidyverse)
library(extrafont)

all <- bind_rows(rep_tweets, sen_tweets)

# 
# at <- 'thoughts and prayers'
# thoughts <- subset(all, grepl(at, text))

thoughts <- subset(merged, grepl("thoughts and prayers", ignore.case = TRUE, text)) 


## This is the quick and dirty way ####
ggplot(thoughts, aes(created_at)) + geom_histogram(color = "black", fill = "firebrick3", bins = 360) + 
  labs(x = "Date Tweeted", y = "Tweets per week", title = "Thoughts and Prayers?", subtitle = "How Often Do Members of Congress Tweet Thoughts and Prayers", caption = "Scraped from Twitter's API") +
  bar_rb() +
  annotate("text", x = as.Date(2017-10-01 14:02:48), y = 70, label = "Las Vegas", size = 10, family = "font")

ggsave(file="D://rtweets/congress/thoughts.png", type = "cairo-png", width = 21, height = 15)

## This is the more involved way ####
thoughts$date <- format(as.Date(strptime(thoughts$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
thoughts$date <- as.Date(thoughts$date, "%m/%d/%Y")

week <- thoughts %>% 
  group_by(week = cut(date, "week"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d"))


font_add_google("Lato", "font")
showtext_auto()

week %>% 
  ggplot(., aes(x=week, y=n)) + geom_col(color = "black", fill = "firebrick3") +
  labs(x = "Date Tweeted", y = "Tweets per week", title = "Assesing How Often Members of Congress Tweet 'Thoughts and Prayers'", subtitle = "", caption = "Scraped from Twitter's API (6/2008 - 4/2018)") +
  theme_minimal() +
  theme(text=element_text(size=44, family="font")) +
  annotate("text", x=as.Date("2017-10-01"), y = 103, label = "Las Vegas", size = 6, family = "font") +
  annotate("text", x=as.Date("2018-02-14"), y = 121, label = "Stoneman Douglas", size = 6, family = "font") +
  annotate("text", x=as.Date("2013-04-15"), y = 121, label = "Boston Bombing", size = 6, family = "font") +
  annotate("text", x=as.Date("2016-06-12"), y = 60, label = "Pulse Nightclub", size = 6, family = "font") +
  annotate("text", x=as.Date("2013-09-15"), y = 74, label = "Paris", size = 6, family = "font") +
  annotate("text", x=as.Date("2017-06-14"), y = 112, label = "Scalise", size = 6, family = "font") +
  annotate("text", x=as.Date("2015-06-22"), y = 67, label = "Charleston", size = 6, family = "font") +
  annotate("text", x=as.Date("2017-11-05"), y = 55, label = "Sutherland Springs", size = 6, family = "font") +
  annotate("text", x=as.Date("2009-11-05"), y = 7, label = "Fort Hood", size = 6, family = "font") +
  annotate("text", x=as.Date("2016-07-10"), y = 52, label = "Nice, France", size = 6, family = "font") +
  annotate("text", x=as.Date("2016-03-14"), y = 82, label = "Brussels", size = 6, family = "font") +
  annotate("text", x=as.Date("2015-11-01"), y = 87, label = "San Bernardino", size = 6, family = "font") +
  annotate("text", x=as.Date("2014-03-11"), y = 70, label = "Malaysia Flight 370", size = 6, family = "font") +
  annotate("text", x=as.Date("2013-07-25"), y = 32, label = "DC Navy Yard", size = 6, family = "font") +
  annotate("text", x=as.Date("2013-05-20"), y = 85, label = "OK Tornadoes", size = 6, family = "font") +
  annotate("text", x=as.Date("2012-07-16"), y = 41, label = "Aurora", size = 6, family = "font") +
  annotate("text", x=as.Date("2012-12-10"), y = 50, label = "Sandy Hook", size = 6, family = "font") +
  ggsave("D://rtweets/congress/thoughts_final.png", dpi = 300, width = 10, height =6, type = "cairo")
  
  
  
  
thoughts %>% mutate(year = format(as.Date(created_at, format="%Y/%m/%d"),"%Y")) %>% 
  mutate(month = format(as.Date(created_at, format="%Y/%m/%d"),"%m")) %>% 
                      filter(year == 2013) %>%  filter(month == "05") %>% select(text) %>% as.data.frame()

 
thoughts %>% mutate(year = format(as.Date(created_at, format="%Y/%m/%d"),"%Y")) %>% group_by(year) %>% count()
all %>% mutate(year = format(as.Date(created_at, format="%Y/%m/%d"),"%Y")) %>% group_by(year) %>% count()

