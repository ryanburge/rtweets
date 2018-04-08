
## I am gonna try something. It's going to pull all tweets that have a digit, a colon, then another digit. That should get bible verses
bible <- subset(all, grepl(":[[:digit:]]+", text))

## Now I need to keep only the tweets that contain names of books of the Bible
bible2 <- subset(bible, grepl("Genesis|Exodus|Leviticus|Numbers|Deuteronomy|Joshua|Judges|Ruth|Samuel|Kings|Chronicles|Ezra|Nehemiah|Esther|Job|Psalms|Proverbs|Ecclesiastes|Solomon|Isaiah|Jeremiah|Lamentations|Ezekiel|Daniel|Hosea|Joel|Amos|Obadiah|Jonah|Micah|Nahum|Habakkuk|Zephaniah|Haggai|Zechariah|Malachi|Matthew|Mark|Luke|John|Acts|Romans|Corinthians|Galatians|Ephesians|Philippians|Thesssalonians|Timothy|Titus|Philemon|Hebrews|James|Peter|Jude|Revelation", text))

## This is leaving a lot of extra junk. I am going to try to filter a lot of that out. 
bible2 <- subset(bible2, !grepl("pm", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("TORNADO", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("Hurricane", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("FLASH FLOOD", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("@foxandfriends", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("@CNNnewsroom", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("CNN", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("Fox", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("FOX", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("FoxNews", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("Radio", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("Watch", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("radio", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("Watch", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("watch", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("tune", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("Tune", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("News", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("news", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("TV", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("EarMarks", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("earmarks", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("meeting", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("Meeting", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("agenda", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("town hall", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("farmers", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("congress", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("@MarkDavis", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("jobs", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("tomorrow", ignore.case = TRUE, text))
bible2 <- subset(bible2, !grepl("January|February|April|June|July|August|September|October|November|December", text))
bible2 <- subset(bible2, !grepl("Monday|Tuesday|Wednesday|Thursday|Friday|Saturday", text))

## It's still not as clean as I would like. I am going to write this out, then do some hand cleaning
write_bib <- bible2 %>% select(screen_name,status_id,text)
write.csv(write_bib, "bible_cong.csv")

## This is my cleaned tweets dataset, it's 1165 total tweets
clean <- read_csv("clean_bible_cong.csv")

count <- clean %>% group_by(screen_name) %>% count() %>% arrange(-n) %>% head(10)

count$party <- c("Republican", "Republican", "Republican", "Republican", "Republican", "Republican", "Republican", "Republican", "Republican", "Republican")

count %>% 
  ggplot(., aes(x= reorder(screen_name, n), y=n, fill = party, label = party, group= party)) + geom_col(fill = "firebrick3", color = "black") + 
  coord_flip() + 
  flip_bar_rb() +
  geom_text(aes(y = n + 20, label = paste0(n)), position = position_dodge(width = .9), size = 10, family = "Product Sans") +
  labs(x = "Screen Name", y = "Number of Tweets", title = "Which Members Tweet Bible Verses?", caption = "Scraped from Twitter's API (6/2008 - 4/2018)", subtitle = "The Top 10 Accounts Were All Republicans")

ggsave(file="D://rtweets/congress/bible_verse_count.png", type = "cairo-png", width = 21, height = 15)



