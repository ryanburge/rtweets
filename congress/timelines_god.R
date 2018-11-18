### Just Party ####


god <- subset(merged, grepl("god|God", ignore.case = TRUE, text)) 

god <- god %>% 
  filter(screen_name != "PrayerCaucus")


god$date <- format(as.Date(strptime(god$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
god$date <- as.Date(god$date, "%m/%d/%Y")

week <- god %>% 
  group_by(month = cut(date, "month"), party)  %>% 
  count() %>% 
  ungroup(month) %>% 
  mutate(month = as.Date(month, "%Y-%m-%d"))

merged$date <- format(as.Date(strptime(merged$created_at, '%Y-%m-%d %H:%M:%S')), "%m/%d/%Y")
merged$date <- as.Date(merged$date, "%m/%d/%Y")

total <- merged %>% 
  group_by(month = cut(date, "month"))  %>% 
  count() %>% 
  ungroup(month) %>% 
  mutate(month = as.Date(month, "%Y-%m-%d")) %>% 
  rename(tot = n)

graph <- left_join(week, total)
graph <- graph %>% 
  mutate(pct = n/tot)


font_add_google("Lato", "font")
showtext_auto()

graph %>% 
  filter(party  == "Republican" | party == "Democrat") %>% 
  filter(month < "2018-03-01") %>% 
  ggplot(., aes(x= month, y= pct, group = party, color = party)) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = "1 year", labels = date_format("%Y")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = percent) + 
  scale_color_manual(values = c("dodgerblue3", "firebrick3"))  +
  theme(legend.position = c(0.8, 0.6)) +
  theme(text=element_text(size=44, family="font")) +
  labs(x = "", y = "Percent of All Tweets", title = "Mentions of God by Members of Congress", caption = "Data: Scraped from Twitter's API (6/2008 - 4/2018)") +
  ggsave("D://rtweets/congress/god_timeline.png", dpi = 300, width = 10, height =6, type = "cairo")

##### Party and Gender #####


week <- god %>% 
  group_by(month = cut(date, "month"), party, gender)  %>% 
  count() %>% 
  ungroup(month) %>% 
  mutate(month = as.Date(month, "%Y-%m-%d"))

graph <- left_join(week, total)
graph <- graph %>% 
  mutate(pct = n/tot)

graph %>% 
  filter(gender != "NA") %>% 
  filter(party  == "Republican" | party == "Democrat") %>%
  mutate(gender = str_replace(gender, "M", "Male")) %>% 
  mutate(gender = str_replace(gender, "F", "Female")) %>% 
  mutate(new = paste(party, gender, sep = " - ")) %>% 
  filter(month < "2018-03-01") %>% 
  ggplot(., aes(x= month, y= pct, group = new, color = new)) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = "1 year", labels = date_format("%Y")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_y_continuous(labels = percent) + 
  scale_color_manual(values = c("#BFEFFF", "#236B8E", "#ff9999", "#660000"))  +
  theme(legend.position = c(0.8, 0.75)) +
  theme(text=element_text(size=44, family="font")) +
  labs(x = "", y = "Percent of All Tweets", title = "Mentions of God by Members of Congress", caption = "Data: Scraped from Twitter's API (6/2008 - 4/2018)") +
  ggsave("D://rtweets/congress/god_timeline_gender.png", dpi = 300, width = 10, height =6, type = "cairo")


