

hh1 <- read_csv("merry_xmas/hh_tweets.csv")
mc1 <- read_csv("merry_xmas/xmas_tweets.csv")

hh2 <- read_csv("merry_xmas/hh_tweets_1221.csv")
mc2 <- read_csv("merry_xmas/xmas_tweets_1221.csv")




reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_mc1 <- mc1 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_hh1 <- hh1 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_mc2 <- mc2 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_hh2 <- hh2 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


count_mc1 <- tidy_tweets_mc1 %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))


count_hh1 <- tidy_tweets_hh1 %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))



count_mc2 <- tidy_tweets_mc2 %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))


count_hh2 <- tidy_tweets_hh2 %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))


hh_1 <- hh1 %>% 
  mutate(date = ymd_hms(created_at)) %>% mutate(hour = hour(date)) %>% mutate(day = as.Date(date))

hour_hh1 <- hh_1 %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Happy Holidays"))

mc_1 <- mc1 %>% 
  mutate(date = ymd_hms(created_at)) %>% mutate(hour = hour(date)) %>% mutate(day = as.Date(date))

hour_mc1 <- mc_1 %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Merry Christmas"))


hh_2 <- hh2 %>% 
  mutate(date = ymd_hms(created_at)) %>% mutate(hour = hour(date)) %>% mutate(day = as.Date(date))

hour_hh2 <- hh_2 %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Happy Holidays"))

mc_2 <- mc2 %>% 
  mutate(date = ymd_hms(created_at)) %>% mutate(hour = hour(date)) %>% mutate(day = as.Date(date))

hour_mc2 <- mc_2 %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Merry Christmas"))


c_graph <- bind_rows(hour_hh1, hour_mc1, hour_hh2, hour_mc2)

g20 <- c_graph %>% 
   filter(day == "2017-12-20") %>% 
  ggplot(., aes(x=factor(hour), y=n, group = term, fill = term, label = term)) + geom_col(position = "dodge", color = "black") + bar_rb() +
  labs(x= "Hour (GMT)", y = "Number of Tweets", title = "Volume of Tweets w/Merry Christmas vs. Happy Holidays", subtitle = "Twitter's API Limited Both Samples", caption = "Tweets Collected on 12/20/2017")


g21 <- c_graph %>% 
  filter(day == "2017-12-21") %>% 
  ggplot(., aes(x=factor(hour), y=n, group = term, fill = term, label = term)) + geom_col(position = "dodge", color = "black") + bar_rb() +
  labs(x= "Hour (GMT)", y = "Number of Tweets", title = "", subtitle = "Twitter's API Limited Both Samples", caption = "Tweets Collected on 12/21/2017")

g20 + g21

ggsave(file="D://rtweets/merry_xmas/compare_rate.png", type = "cairo-png", width = 18, height = 15)




top_mc1 <- count_mc1 %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_mc1 <- top_mc1 %>% 
  mutate(pct = n/17963) %>% 
  mutate(term = c("Merry Christmas")) %>% 
  mutate(date = c("12/20"))

top_hh1 <- count_hh1 %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_hh1 <- top_hh1 %>% 
  mutate(pct = n/17999) %>% 
  mutate(term = c("Happy Holidays")) %>% 
  mutate(date = c("12/20"))


top_mc2 <- count_mc2 %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_mc2 <- top_mc2 %>% 
  mutate(pct = n/17963) %>% 
  mutate(term = c("Merry Christmas")) %>% 
  mutate(date = c("12/21"))

top_hh2 <- count_hh2 %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_hh2 <- top_hh2 %>% 
  mutate(pct = n/17999) %>% 
  mutate(term = c("Happy Holidays")) %>% 
  mutate(date = c("12/21"))


compare <- bind_cols(top_mc1, top_mc2)  %>% mutate(tot = n + n1) %>% mutate(final = tot/35962) %>% select(word, final, term)
compare2 <- bind_cols(top_hh1, top_hh2) %>% mutate(tot = n + n1) %>% mutate(final = tot/35962) %>% select(word, final, term)

cc <- bind_rows(compare, compare2)

cc$term_f = factor(cc$term, levels=c('Merry Christmas','Happy Holidays'))


cc %>% 
  ggplot(., aes(x=reorder(word, final), y=final, group = term, fill = term, label = term)) + geom_col(position = "dodge", color = "black") + 
  coord_flip() + 
  scale_y_continuous(labels = scales::percent) +
  flip_bar_rb() +
  labs(x= "Word", y = "Percent of Tweets", title = "Are Merry Christmas Tweets More Political or Religious?", caption = "Tweets Collected on 12/20/2017") + 
  guides(fill = guide_legend(reverse=FALSE))

ggsave(file="D://rtweets/merry_xmas/words_combined.png", type = "cairo-png", width = 18, height = 15)



a1 <- tidy_tweets_hh1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(term = c("Happy Holidays"))


a2 <- tidy_tweets_mc1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(term = c("Merry Christmas"))


sent1 <- bind_rows(a1, a2)

p2 <- sent %>% 
  # filter(hour >15) %>% 
  ggplot(., aes(x=factor(term), y=sentiment, group = term, fill = term, label = term)) + geom_col(color = "black") + bar_rb() +
  labs(x= "", y = "Sentiment Score", title = "Total Sentiment", subtitle = "Sentiment = Positive Words - Negative Words",caption = "Tweets Collected on 12/21/2017")


hh_v2 <- read_csv("merry_xmas/hh_tweets.csv")
mc_v2 <- read_csv("merry_xmas/xmas_tweets.csv")



reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_mc2 <- mc_v2 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_hh2 <- hh_v2 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))




a3 <- tidy_tweets_hh2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(term = c("Happy Holidays"))


a4 <- tidy_tweets_mc2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(term = c("Merry Christmas"))


sent2 <- bind_rows(a3, a4)


p1 <- sent1 %>% 
  # filter(hour >15) %>% 
  ggplot(., aes(x=factor(term), y=sentiment, group = term, fill = term, label = term)) + geom_col(color = "black") + bar_rb() +
  labs(x= "", y = "Sentiment Score", title = "Total Sentiment of Merry Christmas and Happy Holidays", subtitle = "Sentiment = Positive Words - Negative Words", caption = "Tweets Collected on 12/20/2017") + theme(legend.position="none")


p2 <- sent2 %>% 
  # filter(hour >15) %>% 
  ggplot(., aes(x=factor(term), y=sentiment, group = term, fill = term, label = term)) + geom_col(color = "black") + bar_rb() +
  labs(x= "", y = "Sentiment Score", title = "", subtitle = "Sentiment = Positive Words - Negative Words", caption = "Tweets Collected on 12/21/2017") + theme(legend.position="none")

p1+p2

ggsave(file="D://rtweets/merry_xmas/sent_patch_new.png", type = "cairo-png", width = 18, height = 15)


count_mc1 <- count_mc1[-c(3,4,5,7,8),]
count_mc1 <- count_mc1[-c(4,7,8),]
count_mc1 <- count_mc1[-c(8,9),]
count_mc1 <- count_mc1[-c(1,2),]

count_hh1 <- count_hh1[-c(1,2,3,4,5,7),]
count_hh1 <- count_hh1[-c(7),]



