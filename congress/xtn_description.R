reps <- lists_members(slug = "u-s-representatives", owner_user = "CSpan")
sen <- lists_members(slug = "senators", owner_user = "CSpan")

profiles <- bind_rows(reps, sen)

demo <- read_csv("congress/party_gender.csv") %>% 
      select(-X1, -X1_1)

xtn <- profiles %>% 
  select(screen_name, description)

xtn <-  left_join(xtn, demo)

terms <- 'Christian|christian|christ|Christ|god|God|Jesus|jesus|faith|Faith|catholic|Catholic|Muslim|muslim'
sub <- subset(xtn, grepl(terms, description))

## Only 3 MoCs use one of these terms in their description: one Democrat and two Republicans
## RepValDemings U.S. Representative Florida's 10th Congressional District. Public Servant. Former Orlando Police Chief. Christian. Wife. Mother. Grandmother. Harley Enthusiast. Democrat   F 
## RepCurbelo    Representing Florida's 26th Congressional District, Husband, Dad, Catholic, Son of Cuban exiles, GOP, UM gradx2, Belen Jesuit                                    Republican M    
## RepJeffDuncan Christian, husband, father, small business owner. Lover of football & the outdoors. Employee for the people of SC's Third Congressional District. Call me Jeff.  Republican M     

## Looking for Specific Words ####

df1 <- subset(xtn, grepl("proudly serving|proudly representing", ignore.case = TRUE, description)) 

proud <- df1 %>% 
  mutate(term = "Proudly Serving") %>% 
  filter(party != "NA")

df2 <- subset(xtn, grepl("official twitter", ignore.case = TRUE, description)) 
official <- df2 %>% 
  mutate(term = "Official Twitter") %>% 
  filter(party != "NA")


proud <- df1 %>% 
  mutate(term = "Proudly Serving") %>% 
  filter(party != "NA")

cons <- subset(xtn, grepl("conservative", ignore.case = TRUE, description)) 
cons <- cons %>% 
  mutate(term = "Conservative") %>% 
  filter(party != "NA")

lib <- subset(xtn, grepl("liberal", ignore.case = TRUE, description)) 

rep <- subset(xtn, grepl("Republican| GOP", ignore.case = TRUE, description)) 
rep <- rep %>% 
  mutate(term = "Republican or GOP") %>% 
  filter(party != "NA")


df3 <- subset(xtn, grepl("Democrat", ignore.case = TRUE, description)) 
dem <- df3 %>% 
  mutate(term = "Democrat")

df3 <- subset(xtn, grepl("Female | Woman | Women", ignore.case = TRUE, description)) 
women <- df3 %>% 
  mutate(term = "Female or Woman")

df3 <- subset(xtn, grepl("Gun", ignore.case = TRUE, description)) 
guns <- df3 %>% 
  mutate(term = "Gun")

df3 <- subset(xtn, grepl("progressive", ignore.case = TRUE, description)) 
prog <- df3 %>% 
  mutate(term = "Progressive")

graph <- bind_rows(cons, lib, rep, dem, proud, official, women, guns, prog)

count <- graph %>% 
  group_by(party) %>% 
  count(term)

count %>% 
  filter(party == "Democrat" | party == "Republican") %>% 
  ggplot(., aes(x = reorder(term, n), y = n, fill = party)) +
  geom_col(color = "black", position = "dodge") +
  theme_gg("Lato") +
  coord_flip() +
  scale_fill_manual(values = c("dodgerblue3", "firebrick3"))

tidy <- xtn %>% 
  unnest_tokens(word, description) %>% 
  filter(party != "NA") %>% 
  count(party, word, sort = TRUE) 

tidy <- tidy %>% 
  bind_tf_idf(word, party, n)

tidy %>% 
  filter(party == "Democrat" | party == "Republican") %>% 
  arrange(-tf_idf) %>% 
  as.data.frame()

+scale_y_continuous(labels = percent)



