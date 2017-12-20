library(rtweet)
library(tidyverse)
library(tidytext)
library(lubridate)


## whatever name you assigned to your created app
appname <- "rtweet_burge"

## api key (example below is not a real key)
key <- "gy1nOnuQDFiSnajrgFzxNM5gC"

## api secret (example below is not a real key)
secret <- "VY6AajsjLr8wVfHdUMiQcapWZ6rJDwdULdMbYBGuEJHTCM63l3"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret, set_renv = TRUE)


mc <- search_tweets(
    "Merry Christmas", n = 50000, include_rts = FALSE
  )

hh <- search_tweets(
  "Happy Holidays", n = 18000, include_rts = FALSE
)


mc2 <- mc %>% select(status_id, created_at, user_id, screen_name, text, source, reply_to_status_id, reply_to_user_id, reply_to_screen_name, favorite_count, retweet_count)

hh2 <- hh %>% select(status_id, created_at, user_id, screen_name, text, source, reply_to_status_id, reply_to_user_id, reply_to_screen_name, favorite_count, retweet_count)

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

count_hh <- hh %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Happy Holidays"))

mc <- mc %>% 
  mutate(date = ymd_hms(created_at)) %>% mutate(hour = hour(date)) %>% mutate(day = as.Date(date))

count_mc <- mc %>% group_by(day, hour) %>% count() %>% 
  mutate(term = c("Merry Christmas"))

c_graph <- bind_rows(count_hh, count_mc)

c_graph %>% 
# filter(hour >15) %>% 
  ggplot(., aes(x=factor(hour), y=n, group = term, fill = term, label = term)) + geom_col(position = "dodge", color = "black") + bar_rb() +
  labs(x= "Hour (GMT)", y = "Number of Tweets", title = "Volume of Tweets w/Merry Christmas vs. Happy Holidays", subtitle = "Twitter's API Limited Both Samples", caption = "Tweets Collected on 12/20/2017")

ggsave(file="D://rt/rate_hour.png", type = "cairo-png", width = 18, height = 15)




top_mc <- count_mc %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_mc <- top_mc %>% 
  mutate(pct = n/12395) %>% 
  mutate(term = c("Merry Christmas"))

top_hh <- count_hh %>% 
  filter(word == "god" | word == "president" | word == "america" | word == "tax" | word == "trump" | word == "donald" | word == "vladimir" | word == "gop" | word == "maga" | word == "christian")

top_hh <- top_hh %>% 
  mutate(pct = n/17959) %>% 
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

ggsave(file="D://rt/compare.png", type = "cairo-png", width = 18, height = 15)




