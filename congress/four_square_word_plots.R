# 
# tidy <- merged %>% 
#   unnest_tokens(word, text, token = "tweets")



all_count <- merged %>% 
  group_by(party) %>% 
  count(gender) %>% 
  filter(party != "Independent") %>% 
  filter(party != "Other")

word <- 'god|God'
god <- subset(merged, grepl(word, text))

god_count <- god %>% 
  group_by(party) %>% 
  count(gender) %>% 
  filter(party != "Independent") %>% 
  bind_cols(all_count) %>% 
  mutate(pct = n/n1) %>% 
  mutate(gender = recode(gender, "'F' = 'Female'; 'M' = 'Male'")) %>% 
  mutate(group = paste(party, gender, sep = " - ")) %>% 
  mutate(term = "God")

word <- 'jesus|Jesus'
jesus <- subset(merged, grepl(word, text))

jesus_count <- jesus %>% 
  group_by(party) %>% 
  count(gender) %>% 
  filter(party != "Independent") %>% 
  bind_cols(all_count) %>% 
  mutate(pct = n/n1) %>% 
  mutate(gender = recode(gender, "'F' = 'Female'; 'M' = 'Male'")) %>% 
  mutate(group = paste(party, gender, sep = " - ")) %>% 
  mutate(term = "Jesus")

word <- 'faith|Faith'
faith <- subset(merged, grepl(word, text))

faith_count <- faith %>% 
  group_by(party) %>% 
  count(gender) %>% 
  filter(party != "Independent") %>%   
  filter(party != "Other") %>% 
  bind_cols(all_count) %>% 
  mutate(pct = n/n1) %>% 
  mutate(gender = recode(gender, "'F' = 'Female'; 'M' = 'Male'")) %>% 
  mutate(group = paste(party, gender, sep = " - ")) %>% 
  mutate(term = "Faith")

word <- 'bible|Bible'
bible <- subset(merged, grepl(word, text))

bible_count <- bible %>% 
  group_by(party) %>% 
  count(gender) %>% 
  filter(party != "Independent") %>%   
  filter(party != "Other") %>% 
  bind_cols(all_count) %>% 
  mutate(pct = n/n1) %>% 
  mutate(gender = recode(gender, "'F' = 'Female'; 'M' = 'Male'")) %>% 
  mutate(group = paste(party, gender, sep = " - ")) %>% 
  mutate(term = "Bible")

plot <- bind_rows(bible_count, faith_count, god_count, jesus_count) %>% 
  select(group, term, pct)

plot$group <- factor(plot$group , levels = c("Democrat - Male", "Republican - Male", "Democrat - Female", "Republican - Female"))

# 
# bible_count$group <- factor(bible_count$group , levels = c("Democrat Male", "Republican Male", "Democrat Female", "Republican Female"))
# faith_count$group <- factor(faith_count$group , levels = c("Democrat Male", "Republican Male", "Democrat Female", "Republican Female"))
# jesus_count$group <- factor(jesus_count$group , levels = c("Democrat Male", "Republican Male", "Democrat Female", "Republican Female"))
# god_count$group <- factor(god_count$group , levels = c("Democrat Male", "Republican Male", "Democrat Female", "Republican Female"))



font_add_google("Lato", "font")
showtext_auto()

plot %>% 
  mutate(pct= round(pct,5)) %>%
  ggplot(., aes(x = group, y = pct, fill = group)) +
  geom_col(color = "black") +  
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "none") +  
  facet_wrap(~ term) +
  scale_fill_manual(values = c("#236B8E", "#660000" ,"#BFEFFF", "#ff9999")) +
  theme(text=element_text(size=64, family="font")) +
  geom_text(aes(y = pct + .0005, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
  labs(x = "", y = "Percent of Total Tweets", title = "Mentions of Specific Religious Terms",  caption = "Data: Scraped from Twitter's API (6/2008 - 4/2018)") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 32)) +
  ggsave("D://rtweets/congress/four_square_terms.png", dpi = 300, width = 10, height =6, type = "cairo")
  
# 
# 
# a1 <- bible_count %>% 
#   mutate(pct= round(pct,5)) %>% 
#   ggplot(., aes(x = group, y = pct, fill = group)) + 
#   geom_col(color = "black") +  
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal() +
#   theme(legend.position = "none") +  
#   scale_fill_manual(values = c("dodgerblue3", "firebrick3", "dodgerblue3", "firebrick3")) +
#   theme(text=element_text(size=64, family="font")) +
#   geom_text(aes(y = pct + .000013, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
#   labs(x = "", y = "Percent of Total Tweets", title = "Mentions of Bible") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 32))
# 
# a2 <- faith_count %>% 
#   mutate(pct= round(pct,5)) %>% 
#   ggplot(., aes(x = group, y = pct, fill = group)) + 
#   geom_col(color = "black") +  
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal() +
#   theme(legend.position = "none") +  
#   scale_fill_manual(values = c("dodgerblue3", "firebrick3", "dodgerblue3", "firebrick3")) +
#   theme(text=element_text(size=64, family="font")) +
#   geom_text(aes(y = pct + .00015, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
#   labs(x = "", y = "", title = "Mentions of Faith") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 32))
# 
# 
# 
# a3 <- jesus_count %>% 
#   mutate(pct= round(pct,5)) %>% 
#   ggplot(., aes(x = group, y = pct, fill = group)) + 
#   geom_col(color = "black") +  
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal() +
#   theme(legend.position = "none") +  
#   scale_fill_manual(values = c("dodgerblue3", "firebrick3", "dodgerblue3", "firebrick3")) +
#   theme(text=element_text(size=64, family="font")) +
#   geom_text(aes(y = pct + .00003, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
#   labs(x = "", y = "Percent of Total Tweets", title = "Mentions of Jesus") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 32))
# 
# 
# a4 <- god_count %>% 
#   mutate(pct= round(pct,5)) %>% 
#   ggplot(., aes(x = group, y = pct, fill = group)) + 
#   geom_col(color = "black") +  
#   scale_y_continuous(labels = scales::percent) +
#   theme_minimal() +
#   theme(legend.position = "none") +  
#   scale_fill_manual(values = c("dodgerblue3", "firebrick3", "dodgerblue3", "firebrick3")) +
#   theme(text=element_text(size=64, family="font")) +
#   geom_text(aes(y = pct + .0003, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 16, family = "font") +
#   labs(x = "", y = "", title = "Mentions of God")  + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 32))
# 
# a <- a1 + a2 + a3 + a4
# 
# ggsave("four_terms.png", type = "cairo-png", width = 7, height = 8, a)
