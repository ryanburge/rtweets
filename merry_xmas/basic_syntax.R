library(rtweet)
library(tidyverse)
library(tidytext)
library(lubridate)
library(extrafont)


# ## whatever name you assigned to your created app
# appname <- "rtweet_burge"
# 
# ## api key (example below is not a real key)
# key <- "gy1nOnuQDFiSnajrgFzxNM5gC"
# 
# ## api secret (example below is not a real key)
# secret <- "VY6AajsjLr8wVfHdUMiQcapWZ6rJDwdULdMbYBGuEJHTCM63l3"
# 
# ## create token named "twitter_token"
# twitter_token <- create_token(
#   app = appname,
#   consumer_key = key,
#   consumer_secret = secret, set_renv = TRUE)


mc <- search_tweets(
    "Merry Christmas", n = 50000, include_rts = FALSE
  )



hh <- search_tweets(
  "Happy Holidays", n = 50000, include_rts = FALSE
)


mc2 <- mc %>% select(status_id, created_at, user_id, screen_name, text, source, reply_to_status_id, reply_to_user_id, reply_to_screen_name, favorite_count, retweet_count)
write.csv(mc2, "xmas_tweets_1221.csv")

hh2 <- hh %>% select(status_id, created_at, user_id, screen_name, text, source, reply_to_status_id, reply_to_user_id, reply_to_screen_name, favorite_count, retweet_count)
write.csv(hh2, "hh_tweets_1221.csv")



reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_mc <- mc2 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

reg_words <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets_hh <- hh2 %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg_words) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


count_mc <- tidy_tweets_mc %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))


count_hh <- tidy_tweets_hh %>%
  count(word, sort=TRUE) %>%
  filter(substr(word, 1, 1) != '#', # omit hashtags
         substr(word, 1, 1) != '@') %>% # omit Twitter handles
  mutate(word = reorder(word, n))


hh <- hh %>% 
  mutate(date = ymd_hms(created_at)) %>% mutate(hour = hour(date)) %>% mutate(day = as.Date(date))

hour_hh <- hh %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Happy Holidays"))

mc <- mc2 %>% 
  mutate(date = ymd_hms(created_at)) %>% mutate(hour = hour(date)) %>% mutate(day = as.Date(date))

hour_mc <- mc %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Merry Christmas"))

c_graph <- bind_rows(hour_hh, hour_mc)

c_graph %>% 
# filter(hour >15) %>% 
  ggplot(., aes(x=factor(hour), y=n, group = term, fill = term, label = term)) + geom_col(position = "dodge", color = "black") + bar_rb() +
  labs(x= "Hour (GMT)", y = "Number of Tweets", title = "Volume of Tweets w/Merry Christmas vs. Happy Holidays", subtitle = "Twitter's API Limited Both Samples", caption = "Tweets Collected on 12/20/2017")

ggsave(file="D://rtweets/merry_xmas/rate_hour_v2.png", type = "cairo-png", width = 18, height = 15)




top_mc <- count_mc %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_mc <- top_mc %>% 
  mutate(pct = n/17963) %>% 
  mutate(term = c("Merry Christmas"))

top_hh <- count_hh %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_hh <- top_hh %>% 
  mutate(pct = n/17999) %>% 
  mutate(term = c("Happy Holidays"))

compare <- bind_rows(top_mc, top_hh)

compare$term_f = factor(compare$term, levels=c('Merry Christmas','Happy Holidays'))


compare %>% 
  ggplot(., aes(x=reorder(word, pct), y=pct, group = term, fill = term, label = term)) + geom_col(position = "dodge", color = "black") + 
  coord_flip() + 
  scale_y_continuous(labels = scales::percent) +
  flip_bar_rb() +
  labs(x= "Word", y = "Percent of Tweets", title = "Are Merry Christmas Tweets More Political or Religious?", caption = "Tweets Collected on 12/20/2017") + 
  guides(fill = guide_legend(reverse=FALSE))

ggsave(file="D://rtweets/merry_xmas/compare_v2.png", type = "cairo-png", width = 18, height = 15)



a1 <- tidy_tweets_hh %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(term = c("Happy Holidays"))


a2 <- tidy_tweets_mc %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>% 
  mutate(term = c("Merry Christmas"))


sent <- bind_rows(a1, a2)

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


p1 <- sent2 %>% 
  # filter(hour >15) %>% 
  ggplot(., aes(x=factor(term), y=sentiment, group = term, fill = term, label = term)) + geom_col(color = "black") + bar_rb() +
  labs(x= "", y = "Sentiment Score", title = "Total Sentiment", subtitle = "Sentiment = Positive Words - Negative Words", caption = "Tweets Collected on 12/20/2017")

p1+p2

ggsave(file="D://rtweets/merry_xmas/sent_patch.png", type = "cairo-png", width = 22, height = 15)

