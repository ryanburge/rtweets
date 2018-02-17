library(tidyverse)

library(readr)
league <- read_csv("sports/league.csv")

league <- league %>% 
  mutate(pct = count/total)

league %>% 
  ggplot(., aes(x=reorder(league, -pct), y=pct)) + geom_col(fill = "navyblue", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "League", y = "Percent of Accounts on Twitter List", title = "Percent of Twitter Accounts with Bible Verse(s) in Bio", caption = "Data: Twitter Scrape (2/8/2018)")

ggsave(file="league_tweet_rate.png", type = "cairo-png", width = 18, height = 15)

verses <- read_csv("sports/verses.csv")

count <- verses %>% 
  group_by(verse) %>%
  summarise(sum = sum(count)) %>% 
  ungroup(verse)


count %>% 
  ggplot(., aes(x=reorder(verse, sum), y=sum)) + geom_col(fill = "red4", color = "black") + coord_flip() +
  flip_bar_rb() +
  labs(x= "Bible Verse", y = "Total Number of Occurrences", title = "Which Bible Verses Are Favorites of Athletes? ", caption = "Data: Twitter Scrape (2/8/2018)")

ggsave(file="verse_count.png", type = "cairo-png", width = 18, height = 15)
