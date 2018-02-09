

nba <- lists_members(slug = "nba-players", owner_user = "NBAplayers")
nfl <- lists_members(slug = "nfl-players", owner_user = "NFL")
nhl <- lists_members(slug = "nhl-players", owner_user = "NHL")
mlb <- lists_members(slug = "players1", owner_user = "MLB")



nba1 <- subset(nba, grepl(":[[:digit:]]+", description))
nba1 <- nba1 %>% 
  select(name, description)
write.csv(nba1, "nba.csv")

nfl1 <- subset(nfl, grepl(":[[:digit:]]+", description))
nfl1 <- nfl1 %>% 
  select(name, description)
write.csv(nfl1, "nfl.csv")

nhl1 <- subset(nhl, grepl(":[[:digit:]]+", description))
nhl1 <- nhl1 %>% 
  select(name, description)
write.csv(nhl1, "nhl.csv")

mlb1 <- subset(mlb, grepl(":[[:digit:]]+", description))
mlb1 <- mlb1 %>% 
  select(name, description)
write.csv(chr, "mlb.csv")

