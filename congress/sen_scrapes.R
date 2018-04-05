##This is using CSPANs list of Senators on Twitter to find the usernames
sen <- lists_members(slug = "senators", owner_user = "CSpan")


## This is getting rid of those accounts that don't belong to actual senators
sen <- sen %>% 
  filter(screen_name != "SenateGOP" & screen_name != "SenateDoctors" & screen_name != "SenateDems" & screen_name != "SenateBudget" & screen_name != "SenateBanking") %>% 
  filter(screen_name != "SenateAgDems" & screen_name != "SASCMajority" & screen_name != "IndianCommittee" & screen_name != "dscc" & screen_name != "EnergyGOP") %>% 
  filter(screen_name != "EnergyDems")

## Now I am passing the userid to a users variable
users <- sen$user_id

## This is grabbing their timelines
tweets2 <- get_timelines(users, n = 3200)

## Except we have a problem. I got rated limited and not all the senators got the full 3000 tweets
## Here's what I did, found the accounts that got more than 3k tweets scraped and made a dataset called threek 
threek <- tweets2 %>% group_by(screen_name) %>% count() %>% arrange(-n) %>% head(80)

## Then I anti joined the set with the accounts that already grabbed enough tweets, this should leave me with only the accounts that don't have enough
small <- anti_join(sen, threek)

## Again, pass their userid and scrape those 23 accounts
users <- small$user_id
tweets3 <- get_timelines(users, n = 3200)

## This is binding the two datasets together and then removing duplicates 
sen_tweets <- bind_rows(tweets2, tweets3)
sen_tweets <- sen_tweets[!duplicated(sen_tweets), ]

## Need to dump the list variables so I can get a csv file
write_sen <- sen_tweets %>% select(-hashtags, -symbols, -urls_url, -urls_t.co, -urls_expanded_url, -media_url, -media_t.co, -media_expanded_url, -media_type, -ext_media_url, -ext_media_t.co, -ext_media_expanded_url, -mentions_user_id, -mentions_screen_name, -geo_coords, -coords_coords, -bbox_coords)
write.csv(write_sen, "sen_tweets.csv")

rm(small, threek, tweets, tweets2, tweets3)

