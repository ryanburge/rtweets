
monthly <- merged %>% 
  group_by(month = cut(date, "month"), party, gender) %>% 
  count() %>% 
  ungroup(month) %>% 
  mutate(month = as.Date(month, "%Y-%m-%d")) 


  
monthly %>% 
  filter(gender != "NA") %>% 
  filter(party  == "Republican" | party == "Democrat") %>%
  mutate(gender = str_replace(gender, "M", "Male")) %>% 
  mutate(gender = str_replace(gender, "F", "Female")) %>% 
  mutate(new = paste(party, gender, sep = " - ")) %>% 
  filter(month < "2018-03-01") %>% 
  ggplot(., aes(x= month, y= n, group = new, color = new)) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks = "1 year", labels = date_format("%Y")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("#BFEFFF", "#236B8E", "#ff9999", "#660000"))  +
  theme(legend.position = c(0.5, 0.75)) +
  theme(text=element_text(size=44, family="font")) +
  labs(x = "", y = "Total Number of Tweets", title = "Total Tweet Volume by Month", caption = "Data: Scraped from Twitter's API (6/2008 - 4/2018)") +
  ggsave("D://rtweets/congress/total_vol_tweets_party_gender.png", dpi = 300, width = 10, height =6, type = "cairo")
  