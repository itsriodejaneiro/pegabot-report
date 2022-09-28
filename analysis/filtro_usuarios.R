library(readr)

## Leitura da base de dados do twint 
base_twint <- read_csv("data/processed/base_twint.csv", # Ajustar o caminho para arquivo aqui
                       col_types = cols(.default = "c"))

## Verificar o total de postages por usuário:
total_tweets_user <- base_twint %>% 
  group_by(user_id) %>% # Agrupar por usuário
  summarise(n = n()) # Contar a qntd. de registros (publicações) por usuário
sum(total_tweets_user$n)

# Estatísticas
sd_tweets <- round(sd(total_tweets_user$n)); sd_tweets
mean_tweets <- round(mean(total_tweets_user$n)); mean_tweets
median_tweets <- round(median(total_tweets_user$n)); median_tweets

## Filtro: verificar usuários com alta frequência de tweets
# Agrupar pelo usuário e contar o total de tweets realizados
# filtrar os usuários que tem uma frequência de tweets maior que um valor x (exemplo aqui: desvio padrão, calculado na linha 14)
total_users_altafreq <- base_twint %>% 
  group_by(username) %>% 
  summarise(n = n()) %>% 
  filter(n >= sd_tweets) %>%
  arrange(desc(n)) %>%
  select(username)

colnames(total_tweets_altafreq) <- 'Perfil'

# Salvar o arquivo com os nomes dos uauários
write_csv(total_users_altafreq, 'usuarios_altafreq.csv')
