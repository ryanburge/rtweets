
number <- merged %>% 
  mutate(year = format(as.Date(created_at, format="%Y/%m/%d"),"%Y")) %>% 
  mutate(month = format(as.Date(created_at, format="%Y/%m/%d"),"%m")) %>% 
  mutate(mnth = as.numeric(month)) %>% 
  filter(year == 2017) %>%  
  filter(mnth >=7) %>% 
  group_by(party, gender) %>% 
  count() %>% 
  filter(party == "Democrat" | party  == "Republican")

number2 <- merged %>% 
  mutate(year = format(as.Date(created_at, format="%Y/%m/%d"),"%Y")) %>% 
  mutate(month = format(as.Date(created_at, format="%Y/%m/%d"),"%m")) %>% 
  mutate(mnth = as.numeric(month)) %>% 
  filter(year == 2017) %>%  
  filter(mnth >=7) %>%  
  select(screen_name, party, gender) %>% 
  distinct(screen_name, party, gender) %>% 
  group_by(party) %>% 
  count(gender) %>%  
  filter(party == "Democrat" | party  == "Republican")

bars <- bind_cols(number2, number) %>% 
  select(party, gender, n, n1) %>% 
  rename(tweet_n = n1) %>% 
  mutate(avg = tweet_n/n)

bars %>% 
  mutate(avg = round(avg, 0)) %>% 
  filter(gender != "NA") %>% 
  filter(party  == "Republican" | party == "Democrat") %>%
  mutate(gender = str_replace(gender, "M", "Male")) %>% 
  mutate(gender = str_replace(gender, "F", "Female")) %>% 
  mutate(new = paste(party, gender, sep = " - ")) %>% 
  ggplot(., aes(x = new, y = avg, fill = new)) +
  geom_col(color = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#BFEFFF", "#236B8E", "#ff9999", "#660000")) +
  theme(text=element_text(size=44, family="font")) +
  geom_text(aes(y = avg + 14, label = paste0(avg, ' Tweets')), position = position_dodge(width = .9), size = 13, family = "font") +
  labs(x = "", y = "Average Number of Tweets per Account", title = "Average Number of Tweets per Party and Gender", subtitle = "Between July 2017 and December 2017", caption = "Data: Scraped from Twitter's API (6/2008 - 4/2018)") +
  ggsave("D://rtweets/congress/avg_tweets_gender_party.png", dpi = 300, width = 7, height =6, type = "cairo")



