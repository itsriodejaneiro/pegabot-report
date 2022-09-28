# Initial config -  Always execute
source('~/initialConfig.R')

# Leitura da base twint
base_twint <- read_csv("Dados/alltweets_2021-06-28 00:00:00_1625005801.csv",
                       col_types = cols(.default = "c"))

# Agrupar qntd de tweets por usuário
total_tweets_user <- base_twint %>% 
  group_by(user_id) %>% 
  summarise(n = n()) 

# Estatísticas
sd_tweets <- round(sd(total_tweets_user$n),1); sd_tweets
mean_tweets <- round(mean(total_tweets_user$n),1); mean_tweets
median_tweets <- round(median(total_tweets_user$n),1); median_tweets

# Qtd de usuários por número de publicações
users_per_ntweets <- base_twint %>% 
  group_by(username) %>% 
  summarise(n = n())  %>%
  count(total_tweets = n, sort = TRUE) %>%
  mutate(percent = percent(n/length(unique(base_twint$username))))

# Verificar usuários com alta frequência de tweets
# Agrupar pelo usuário e contar o total de tweets realizados
# filtrar os usuários que tem uma frequência de tweets maior que um valor x (desvio padrão)
total_tweets_altafreq <- base_twint %>% 
  group_by(username) %>% 
  summarise(n = n()) %>% 
  filter(n >= mean_tweets) %>%
  arrange(desc(n))

handles_analisar <- paste0('@', total_tweets_altafreq$username)
handles_analisar <- as.data.frame(handles_analisar)
colnames(handles_analisar) <- 'Perfil'
write_csv(handles_analisar, 'Dados/handles_mean.csv')

