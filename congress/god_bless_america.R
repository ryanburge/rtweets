
all_count <- merged %>% 
  group_by(party, gender) %>% 
  count()

sub <- subset(merged, grepl("god bless america|God Bless America|god bless America|God bless America", ignore.case = TRUE, text))

sub %>% 
  group_by(gender, party) %>% 
  count() %>% 
  rename(total = n) %>% 
  left_join(all_count) %>% 
  mutate(pct = total/n)

terms <- 'God|god'
sub <- subset(complete, grepl(terms, text))

sub %>% 
  group_by(gender, party) %>% 
  count() %>% 
  rename(total = n) %>% 
  left_join(all_count) %>% 
  mutate(pct = total/n)

terms <- 'Religion|religion'
sub <- subset(complete, grepl(terms, text))

sub %>% 
  group_by(gender, party) %>% 
  count() %>% 
  rename(total = n) %>% 
  left_join(all_count) %>% 
  mutate(pct = total/n)

terms <- 'evangelical|Evangelical'
sub <- subset(complete, grepl(terms, text))

sub %>% 
  group_by(gender, party) %>% 
  count() %>% 
  rename(total = n) %>% 
  left_join(all_count) %>% 
  mutate(pct = total/n)