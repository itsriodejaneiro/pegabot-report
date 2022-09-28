# Read data ---------------------------------------------------------------

base_termos <- c('Rússia', 'Ucrânia', 'Putin', 'Vladimir Putin',
                 'Zelensky', 'Zelenski', 'Volodymyr Zelensky',
                 'Volodymyr Zelenski', 'Volodimir Zelensky',
                 'Volodimir Zelenski', 'Vlodimir Zelensky',
                 'Vlodimir Zelenski', 'Moscou', 'Kiev')

base_twint <-
    list.files(path = paste0('data/raw'), 
               pattern = "^all", full.names = T) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c")))
  
length(unique(base_twint$id))

# Remove duplicates - keep the tweet that has the highest # of rts

base_twint <- base_twint %>% 
  group_by(id) %>%
  arrange(desc(nretweets)) %>%
  slice(1) %>%
  filter(!is.na(id)) %>%
  filter(language == 'und' | language == 'pt') %>%
  mutate(tweet_lower = tolower(tweet)) 

base_twint_cleaned <- base_twint[grepl(paste(tolower(base_termos), collapse="|"), base_twint$tweet_lower),]

write_csv(base_twint, 'data/processed/base_twint.csv')
