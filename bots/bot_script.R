library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(httr)
library(wordcloud2)

tweets <- read_csv("tweets.csv")

tweets$date<-ymd_hms(tweets$created_str)
tweets$date <- floor_date(tweets$date, "day")


reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


faith <- subset(tidy_tweets, grepl("faith", word)) %>% mutate(term = c("Faith"))
god <- subset(tidy_tweets, grepl("god", word)) %>% mutate(term = c("God"))
jesus <- subset(tidy_tweets, grepl("jesus", word)) %>% mutate(term = c("Jesus"))
christ <- subset(tidy_tweets, grepl("christ", word)) %>% mutate(term = c("Christ"))
church <- subset(tidy_tweets, grepl("church", word)) %>% mutate(term = c("Church"))
bible <- subset(tidy_tweets, grepl("bible", word)) %>% mutate(term = c("Bible"))
church <- subset(tidy_tweets, grepl("church", word)) %>% mutate(term = c("Church"))
islam <- subset(tidy_tweets, grepl("islam", word)) %>% mutate(term = c("Islam"))
muslim <- subset(tidy_tweets, grepl("muslim", word)) %>% mutate(term = c("Muslim"))
religion <- subset(tidy_tweets, grepl("religion", word)) %>% mutate(term = c("Religion"))
gay <- subset(tidy_tweets, grepl("gay", word)) %>% mutate(term = c("Gay"))
abortion <- subset(tidy_tweets, grepl("abortion", word)) %>% mutate(term = c("Abortion"))
pray <- subset(tidy_tweets, grepl("pray", word)) %>% mutate(term = c("Pray"))



sml <- bind_rows(faith, god, jesus, christ, bible, church, islam, muslim, religion, gay, abortion, pray) 

sml <- sml %>% 
  filter(word != "christmas") %>% 
  filter(word != "christina")

# trump <- subset(tidy_tweets, grepl("trump", word)) %>% mutate(term = c("Trump"))

sml <- bind_rows(faith, god, jesus, christ, bible, church, islam, muslim, religion, gay, abortion, pray) 

count <- sml %>% group_by(term) %>% count()

count %>% 
  ggplot(., aes(x=reorder(term, -n), y=n)) + geom_col(fill = "blue4", color = "black") + bar_rb() +
  labs(x= "Word", y= "Number of Occurrences", title ="Which Religious Terms Do Twitter Trolls Use?", caption= "Data: NBC News", subtitle = "For Comparision: 'Trump' Occurs 13,139 times")

ggsave(file="bot_counts.png", type = "cairo-png", width = 18, height = 15)

ts <- sml %>% 
  filter(term != "Trump") %>% group_by(date) %>% count() 

ts$date <- as.Date(ts$date)

ts %>% 
  ggplot(., aes(x=date, y=n)) + geom_line() +
  scale_x_date(breaks = date_breaks("months"), labels = date_format("%b - %y")) + long_rb() + theme(axis.text.x = element_text(family = "Product Sans", size =24, angle = 45, hjust = 1)) +
  labs(x= "Date", y = "Number of Tweets with Religious Words", title = "When Did Trolls Use Religious Words?", caption= "Data: NBC News")

ggsave(file="long_bots.png", type = "cairo-png", width = 18, height = 15)


user <- sml %>% 
  group_by(user_key) %>% 
  count() %>% arrange(-n) 

user2 <- tidy_tweets %>% 
  group_by(user_key) %>% 
  count() %>% arrange(-n) 

us <- left_join(user, user2, by = c("user_key")) %>% mutate(pct = n.x/n.y) 

use <- read_csv("use.csv")

use$user_key <- tolower(use$user_key)

cuse <- read_csv("bots/duh.csv")

cuse <- left_join(cuse, us)


cuse <- cuse %>% 
  select(-n, -n.x, -n.y, -X1) %>% 
  mutate(follow = followers_count/1000) %>% 
  mutate(pct = pct*100) %>% 
  select(-followers_count) %>% 
  melt()

cuse <- cuse %>% 
  mutate(variable = recode(variable, "'follow'= 'Followers (in Thousands)'; 'pct'= '% Religious Tweets'"))
  

cuse %>% 
  ggplot(., aes(x= reorder(user_key, value), y= value, group= variable, label = variable, fill=variable)) + 
  geom_col(position = "dodge", color = "black") + 
  flip_bar_rb() + coord_flip() +
  labs(x= "User Name", y = "Count", title = "Which Trolls Were the Most Successful?", caption= "Data: NBC News") +
  scale_fill_manual(values=c("darkgrey", "dodgerblue3")) + theme(plot.title = element_text(family = "Product Sans", size = 54, vjust =2, face = "bold"))

ggsave(file="users_bots.png", type = "cairo-png", width = 18, height = 15)

scatter <- left_join(pct, use)



scatter %>% 
  filter(n.y >100) %>% 
  ggplot(., aes(x=pct, y=followers_count/100)) + geom_point(size =4) + scatter_rb() + geom_smooth() +
  labs(x= "Percent of Tweets that Were Religious", y = "Follower Count (in Thousands)", title = "Relationship Between Followers and Religious Tweets", caption= "Data: NBC News") + 
  theme(plot.title = element_text(family = "Product Sans", size = 44, vjust =2, face = "bold")) + 
  scale_x_continuous(labels = scales::percent)

ggsave(file="scatter_bots.png", type = "cairo-png", width = 18, height = 12)
