ab <- 'abortion'
abort <- subset(all, grepl(ab, text))

abort$date <- format(as.Date(strptime(abort$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
abort$date <- as.Date(abort$date, "%m/%d/%Y")


abweek <- abort %>% 
  group_by(week = cut(date, "week"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d")) 
 
all$date <- format(as.Date(strptime(all$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
all$date <- as.Date(all$date, "%m/%d/%Y")

all_week <- all %>% 
  group_by(week = cut(date, "week"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d")) %>% 
  rename(total =n)

ab_pct <- left_join(all_week, abweek) %>% 
  mutate(pct = n/total)

ab_pct %>% 
  ggplot(., aes(x=week, y=pct)) + geom_col(color = "black", fill = "firebrick3") +
  bar_rb() +
  labs(x = "Date Tweeted", y = "Percent of All Tweets", title = "Discussion of Abortion by Congress") +
  annotate("text", x=as.Date("2017-01-23"), y = .020, label = "Global Gag Rule", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2017-10-02"), y = .010, label = "House Passes 20 Week Ban", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2009-11-02"), y = .016, label = "Stupak Amendment", size = 6, family = "Product Sans")

ggsave(file="D://rtweets/congress/abortion.png", type = "cairo-png", width = 21, height = 15)


ab_pct %>% 
  ggplot(., aes(x=week, y=pct)) + geom_col(color = "black", fill = "firebrick3") +
  bar_rb() +
  labs(x = "Date Tweeted", y = "Percent of All Tweets", title = "Discussion of Abortion by Congress") +
  annotate("text", x=as.Date("2017-01-23"), y = .020, label = "Global Gag Rule", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2017-10-02"), y = .010, label = "House Passes 20 Week Ban", size = 6, family = "Product Sans") +
  annotate("text", x=as.Date("2009-11-02"), y = .016, label = "Stupak Amendment", size = 6, family = "Product Sans")

ggsave(file="D://rtweets/congress/abortion.png", type = "cairo-png", width = 21, height = 15)

abort %>% mutate(year = format(as.Date(created_at, format="%Y/%m/%d"),"%Y")) %>% 
  mutate(month = format(as.Date(created_at, format="%Y/%m/%d"),"%m")) %>% 
  filter(year == 2010) %>%  filter(month == "07") %>% select(text) %>% as.data.frame()

##Trying it by month
abmonth <- abort %>% 
  group_by(week = cut(date, "month"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d")) 

all_month <- all %>% 
  group_by(week = cut(date, "month"))  %>% 
  count() %>% 
  ungroup(week) %>% 
  mutate(week = as.Date(week, "%Y-%m-%d")) %>% 
  rename(total =n)

ab_pct <- left_join(all_week, abweek) %>% 
  mutate(pct = n/total)

ab_pct %>% ggplot(., aes(x=week, y=pct)) + geom_col() + bar_rb()
