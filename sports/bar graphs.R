library(tidyverse)

library(readr)
league <- read_csv("sports/league.csv")

league <- league %>% 
  mutate(pct = count/total)

img <- readPNG(system.file("img", "D://rtweets/sports/mlb.png", package="png"))
g <- rasterGrob(img, interpolate=TRUE)


league %>% 
  ggplot(., aes(x=reorder(league, -pct), y=pct)) + geom_col(fill = "seagreen4", color = "black") + 
  scale_y_continuous(labels = scales::percent) +
  bar_rb() +
  labs(x= "League", y = "Percent of Accounts on Twitter List", title = "Percent of Twitter Accounts with Bible Verse(s) in Bio", caption = "Data: Twitter Scrape (2/8/2018)")

ggsave(file="league_tweet_rate.png", type = "cairo-png", width = 18, height = 15)
