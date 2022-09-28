# Leitura das bases ----------------------------------------------------

## 1. Leitura dos resultados do pegabot #####
base_pegabot <- read_csv("data/processed/base_pegabot.csv", 
                         col_types = cols(.default = "c"))

# Ajustes no username to lower e com @
base_pegabot$`Perfil Twitter` <- gsub('@', '', tolower(base_pegabot$`Perfil Twitter`))
base_pegabot$id_pegabot <- seq(1:nrow(base_pegabot))

## 2. Leitura da base twint #####
base_twint <- read_csv("data/processed/base_twint.csv", 
                      col_types = cols(.default = "c"))

length(unique(base_twint$id))
length(unique(base_twint$user_id_str))

# Ajustes no username de base_twint to lower e com @
base_twint$username_original <- base_twint$username
base_twint$username <- gsub('@', '', tolower(base_twint$username))

write_csv(base_twint_cleaned, 'data/processed/base_twint.csv')
base_twint <- read_csv('data/base_twint.csv', col_types = cols(.default = "c"))

## 3. Juntar tudo #####
full_data <- merge(base_twint, base_pegabot, by.x = 'username', by.y = 'Perfil Twitter', all.x = T); #rm(bp); rm(twint)
length(unique(full_data$user_id))

# Analises ----------------------------------------------------------------

## 0. Filtrar bots e erros #####

# Porcentagem de 70%+ na lista da hashtag
full_data <- full_data %>% # Separando a base 
  mutate(Resultado = cut(x = as.numeric(`Análise Total`), 
                         breaks = c(0,70, Inf),
                         labels = c("Baixa",
                                    "Alta")))
# Para agilizar:
write_csv(full_data, 'data/processed/full_data.csv')
full_data <- read_csv('data/processed/full_data.csv', col_types = cols(.default = "c"))

bots <- full_data %>%
  filter(Resultado == "Alta") %>% 
  distinct(user_id, `username`, `Análise Total`)

# Filtrar tweets de bots
bots_twint <- full_data %>%
  filter(user_id %in% bots$user_id)

# Perfis que não puderam ser analisados
erro <- full_data %>% 
  filter(is.na(`Análise Total`)) %>%
  distinct(user_id, username)

erros_twint <- full_data %>%
  filter(user_id %in% erro$user_id)

## 1. Stats #####

# Números gerais
ntweets_analisados <- length(unique(full_data$id)); ntweets_analisados
nperfis_total <- length(unique(full_data$user_id)); nperfis_total
nperfis_analisados <- full_data %>%
  select('username') %>%
  drop_na() %>%
  distinct() %>% count(); nperfis_analisados
nerros <- nrow(erro); nerros
percent_erros <- percent(nerros/nperfis_analisados$n[1], 0.1); percent_erros
nposts_erro <- nrow(erros_twint); nposts_erro
percent_posts_erro <- percent(nposts_erro/ntweets_analisados); percent_posts_erro
primeiro_dia <- min(full_data$date); primeiro_dia
ultimo_dia <- max(full_data$date); ultimo_dia

# qtd. de bots:
nbots <- length(unique(bots$user_id)); nbots
nbots_twint <- length(unique(bots_twint$user_id)); nbots_twint # aqui só para confirmar valores
# % de bots em relação aos perfis analisados:
percent_bots <- percent(nbots/nperfis_analisados$n, 0.1); percent_bots 
# qtd de publicações feitos por bots:
nposts_bots <- nrow(bots_twint); nposts_bots
# qtd de tweets feitos por bots:
ntweets_bots_twint <- bots_twint %>% filter(retweet == 'False') %>% nrow(); ntweets_bots_twint
# qtd de rts feitos por bots:
nretweets_bots_twint <- bots_twint %>% filter(retweet == 'True') %>% nrow(); nretweets_bots_twint
# % do volume de publicações feitas por bots em relação ao total de tweets coletados:
percent_posts_bots <- percent(nposts_bots/ntweets_analisados, 0.1); percent_posts_bots
# % do que é tweet (de bots) considerando todas as publicações de bots
percent_tweets_bots <- percent(ntweets_bots_twint/nposts_bots); percent_tweets_bots
# % do que é rt (de bots) considerando todas as publicações de bots
percent_retweets_bots <- percent(nretweets_bots_twint/nposts_bots); percent_retweets_bots

total_horas <- round(as.numeric(difftime(ultimo_dia, primeiro_dia, units = "h")),); total_horas
total_dias <- round(as.numeric(difftime(ultimo_dia, primeiro_dia, units = "d")),); total_dias
total_semanas <- round(as.numeric(difftime(ultimo_dia, primeiro_dia, units = "w")),); total_semanas

stats_df <- t(data.frame('Total de tweets' = ntweets_analisados,
                         'Total_perfis analisados' = nperfis_analisados,
                         'Total perfis não analisados' = nerros,
                         #'Porc. perfis com erro' = percent_erros,
                         'Inicio' = primeiro_dia,
                         'Fim' = ultimo_dia,
                         'Total de horas' = total_horas,
                         'Total de dias' = total_dias,
                         'Total de semanas' = total_semanas,
                         'Total perfis automatizados' = nbots,
                         'Porc. Total perfis automatizados' = percent_bots,
                         'Posts de perfis automatizados' = nposts_bots,
                         'Porc. Posts de perfis automatizados' = percent_posts_bots))

# Salvar arquivo com valores gerais
output_file <- "resumo.xlsx"

write.xlsx2(stats_df, output_file, sheetName = "Resumo",
            col.names = F, row.names = TRUE, append = TRUE)

## 2. Analise Temporal #####
full_data_bckp <- full_data

# Adicionar intervalo de 7 dias
full_data$interval_7day <- cut(as.POSIXct(full_data$date), breaks = "7 days",) 
# Adicionar intervalo de 1 dia
full_data$interval_1day <- cut(as.POSIXct(full_data$date), breaks = "1 day",) 
# Adicionar intervalo de 1 h
full_data$interval_1hour <- cut(as.POSIXct(full_data$date), breaks = "1 hour",) 
# Adicionar intervalo de 2 h
full_data$interval_2hour <- cut(as.POSIXct(full_data$date), breaks = "2 hour",) 
# Adicionar intervalo de 6 h
full_data$interval_6hour <- cut(as.POSIXct(full_data$date), breaks = "6 hour",) 
# Adicionar intervalo de 3 h
full_data$interval_3hour <- cut(as.POSIXct(full_data$date), breaks = "3 hour",) 
# Adicionar intervalo de 10 min
full_data$interval_10min <- cut(as.POSIXct(full_data$date), breaks = "10 min",) 
# Adicionar intervalo de 5 min
full_data$interval_5min <- cut(as.POSIXct(full_data$date), breaks = "5 min") 

# Para agilizar 2:
write_csv(full_data, 'data/processed/full_data_details.csv')
full_data <- read_csv('data/processed/full_data_details.csv', col_types = cols(.default = "c"))

# Ajustar o parâmetro da função a seguir para o intervalo
# que fizer mais sentido. Esses intervalor correspondem aos calculados
# nas linhas 160 a 175
ft_temporal <- plyr::count(full_data, 'interval_3hour')
# Ajustar parâmetro a seguir com o mesmo intervalo usado na linha anterior
ft_temporal$interval <- format(strptime(ft_temporal$interval_3hour,
                                        "%Y-%m-%d %H:%M"),
                               format = "%d/%m (%Hh)") 
ft_temporal_outline <- boxplot.stats(ft_temporal$freq)$out

# Verificar picos de compartilhamento
peaks <- ft_temporal %>%
  filter(freq %in% ft_temporal_outline); peaks; str(peaks) #%>% droplevels()

# Plot temporal
plot_temporal <- ggplot(ft_temporal, aes(x = fct_reorder(interval, as.Date(interval_3hour)), y = freq)) +
  geom_line(group = 1, color = paleta_its[2], size = 1) +
    labs(title = "Evolução do compartilhamento dos tweets",
       subtitle = "publicados entre os dias 23/08 e 01/09 de 2022", # Ajustar subtítulo 
       x = "",
       y = "") +
  # Ajustar valores fixo de 'by' que está em breaks (do eixo x) a seguir
  # para arrumar a visualização do gráfico
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   breaks = ft_temporal$interval[seq(1, nrow(ft_temporal), by = 3)]) +
  theme_minimal() +
  # Ajustar valores fixos que estão em breaks (do eixo y) a seguir
  # para arrumar a visualização do gráfico
  scale_y_continuous(breaks = seq(0, max(ft_temporal$freq) + 10000, 2000),
                     name = "Nº de tweets") +
  theme(text = element_text(family = "sans"),
        plot.title = element_text(size = 13, 
                                  colour = paleta_its[8]),
        plot.subtitle = element_text(size = 12, 
                                     colour = paleta_its[8]
                                     ),
        axis.text.x = element_text(size = 12,
                                   colour = paleta_its[7]),
        axis.text.y = element_text(size = 12,
                                   colour = paleta_its[7]),
        axis.title.y = element_text(size = 13, 
                                    colour = paleta_its[7],
                                    vjust = 2, 
                                    hjust = 0.98),
        plot.margin = unit(c(.5,2,.5,1),"cm"),
        panel.grid.minor.y = element_blank()); plot_temporal

ggsave('output/plot1_temporal.png', width = 18, height = 10, units = 'in', dpi = 100)

write.xlsx2(ft_temporal, output_file, sheetName = "EvolucaoTemporal",
            col.names = F, row.names = TRUE, append = TRUE)

## 3. O que tem na base (tweets e RTs) #####

nretweets <- nrow(full_data[full_data$retweet == 'True',]); nretweets
ntweets <- nrow(full_data[full_data$retweet == 'False',]); ntweets
percent_rts <- percent(nretweets/ntweets_analisados, 0.1); percent_rts
percent_tweets <- percent(ntweets/ntweets_analisados, 0.1); percent_tweets

# Plot
tweets_rts <- c(`RT` = nretweets, `Tweets` = ntweets)
waffle(title = 'Proporção de RTs e Tweets compartilhados pelos usuários',
       tweets_rts / 1000, rows = 6,
       colors = c(paleta_its[2], paleta_its[6]),
       xlab = 'cada unidade = 10000 registros')

plot_tweets_rts <- waffle(
  tweets_rts / 1000, rows = 6, # Ajustar esses dois valores manualmente
  colors = c(paleta_its[2], paleta_its[6]),
  legend_pos="bottom-left") + 
  labs(title = "Proporção de <b style='color:#8a8a8a'>tweets</b> e <b style='color:#54318C'>RTs</b> ", 
       subtitle = "<span style='color:#6c6c6c'>cada unidade = 1.000 registros</span>") +
  theme(text = element_text(family = "sans"),
        plot.title = element_markdown(lineheight = 1.1, size = 20),
        plot.subtitle = element_markdown(lineheight = 1.1, size = 16),
        axis.title.x = element_text(size = 13, vjust = 1, 
                                    hjust = 0, colour = paleta_its[7]),
        plot.margin = unit(c(0,0,0,0),"mm")
  ); plot_tweets_rts

ggsave('output/plot0_tweets_rts.png', width = 7, height = 5, units = 'in', dpi = 100)

## 4. Análise da distribuição dos volume de tweets publicados por usuário ####

# Verificar o desvio padrão, média e mediana dos registros 
# Agrupar qntd de tweets por usuário
total_tweets_user <- base_twint %>% 
  group_by(user_id) %>% 
  summarise(n = n()) 
sum(total_tweets_user$n)

# Estatísticas
sd_tweets <- round(sd(total_tweets_user$n)); sd_tweets
mean_tweets <- round(mean(total_tweets_user$n)); mean_tweets
median_tweets <- round(median(total_tweets_user$n)); median_tweets

# Qtd de usuários por número de publicações
users_per_ntweets <- full_data %>% 
  group_by(user_id) %>% 
  summarise(n = n())  %>%
  count(total_tweets = n, sort = TRUE) %>%
  mutate(percent = percent(n/ntweets_analisados))

# Usuários que publicaram até a média de tweets
user_ntweets_mean <- users_per_ntweets %>% 
  filter(total_tweets <= mean_tweets) %>% 
  summarise(total = sum(n)) %>%
  mutate(percent = percent(total/nperfis_total)); user_ntweets_mean

write.xlsx2(users_per_ntweets, output_file, sheetName = "QtdTweets-QtdUsuarios",
            col.names = F, row.names = TRUE, append = TRUE)

# Verificar usuários com alta frequência de tweets
# Agrupar pelo usuário e contar o total de tweets realizados
# filtrar os usuários que tem uma frequência de tweets maior que um valor x (desvio padrão)
total_tweets_altafreq <- base_twint %>% 
  group_by(username) %>% 
  summarise(n = n()) %>% 
  filter(n >= sd_tweets) %>%
  arrange(desc(n))

# Porcentagem dos usuários com alta frequência de tweets em relação ao total de usuários únicos 
# presentes na base do twint
nusuarios_alta_freq <- nrow(total_tweets_altafreq); nusuarios_alta_freq
usuarios_alta_freq <- scales::percent(nusuarios_alta_freq/nperfis_total, accuracy = 0.1); usuarios_alta_freq

# Quantidade total de tweets postados por usuários com alta frequência -- sum(total_tweets_freq80$n)
# em relação à quantidade total de tweets coletados -- ntweets_analisados
# Isso vai dar uma ideia do volume de tweets que poucos usuarios publicam em relação ao total de tweets.
ntotal_tweets_altafreq <- sum(total_tweets_altafreq$n); ntotal_tweets_altafreq
alta_atividade <- scales::percent(ntotal_tweets_altafreq/ntweets_analisados, accuracy = 0.1); alta_atividade

users_per_ntweets$type <- ifelse(round(users_per_ntweets$total_tweets) >= sd_tweets, 
                                 'alta', 'normal')
# Plot todos os usuários
plot_dist <- ggplot(users_per_ntweets, aes(x = total_tweets, y = n, colour = type)) + 
  geom_point(alpha = .7, size = 4) +
  #geom_bar(stat = "identity", fill = paleta_its[1]) +
  labs(title = "Distribuição da quantidade de tweets por número de usuários",
       subtitle = expression(
         paste("considerando ", bold("todos os usuários"))),
       y = "Nº de usuários (log)",
       x = "Nº de tweets") + scale_y_log10() +
  theme_minimal() + 
  scale_color_manual(values = paleta_its[c(11,6)]) +
  theme(text = element_text(family = "sans"),
        legend.position = "none",
        plot.title = element_text(size = 20, 
                                  colour = paleta_its[8]),
        plot.subtitle = element_text(size = 18,
                                     colour = paleta_its[8]),
        axis.text.x = element_text(size = 18,
                                   colour = paleta_its[7]),
        axis.title.x = element_text(size = 18, 
                                    colour = paleta_its[7],
                                    vjust = -1, 
                                    hjust = 0),
        axis.text.y = element_text(size = 18,
                                   colour = paleta_its[7]),
        axis.title.y = element_text(size = 18, 
                                    colour = paleta_its[7],
                                    vjust = 2, 
                                    hjust = 0.98),
        plot.margin = unit(c(.5,2,.5,1),"cm"),
        panel.grid.minor.y = element_blank()); plot_dist #+

ggsave('output/plot3_dist.png', width = 16, height = 8, units = 'in', dpi = 100)

# O que é tweet e retweet desses usuários com alta frequência
# Filtrar a base para pegar apenas regsitros desses usuários 
subset_alta_freq <- full_data[full_data$username %in% total_tweets_altafreq$username,]
retweets_altafreq <- nrow(subset_alta_freq[subset_alta_freq$retweet == 'True',])
tweets_altafreq <- nrow(subset_alta_freq[subset_alta_freq$retweet == 'False',])

# Porcentagem em relação ao total de registros de subset_alta_freq
percent_rts_alta <- scales::percent(retweets_altafreq/nrow(subset_alta_freq)); percent_rts_alta
percent_tweets_alta <- scales::percent(tweets_altafreq/nrow(subset_alta_freq)); percent_tweets_alta

# Porcentagem em relação ao total de registros da full_data
percent_rts_alta <- scales::percent(retweets_altafreq/ntweets_analisados); percent_rts_alta
percent_tweets_alta <- scales::percent(tweets_altafreq/ntweets_analisados); percent_tweets_alta

# Dentre os perfis com mais de 70%+ quais estão na listagem 
# de alta frequência de posts
usuarios_altafreq_bots <- bots[bots$username %in% total_tweets_altafreq$username,]
nrow(usuarios_altafreq_bots)
(nrow(usuarios_altafreq_bots)/nrow(total_tweets_altafreq))*100

## 5. Análises sobre data de criação dos usuários #####

# Quantidade de usuários que foram criados nos últimos anos e que são bots 

full_data$account_created <- as.Date(full_data$`Criação da conta`)
full_data$interval_created_account <- format(full_data$`Criação da conta`,
                                             format = "%d/%m/%Y")
created_account_peryear <- full_data %>%
  select(user_id, username, interval_created_account, Resultado) %>%
  distinct() %>%
  mutate(year = format(as.Date(interval_created_account, format="%d/%m/%Y"),"%Y")) %>%
  group_by(year, Resultado) %>%
  count() %>%
  arrange(year) %>%
  filter(year != '1970') %>% filter(year != '2006')

plot_creation <- ggplot(created_account_peryear, aes(x = year, y = n)) + 
  geom_line(aes(color = Resultado, group = Resultado), size = 1.5) +
  geom_point(aes(color = Resultado, group = Resultado)) +
  scale_color_manual(values = paleta_its[c(9,2)]) +
  labs(title = "Evolução da criação de contas por ano",
       x = "",
       y = "") +
  theme_minimal() + 
  scale_y_continuous(breaks = seq(0, max(created_account_peryear$n) + 1000, 300),
                     name = "Nº de novas contas") +
  theme(text = element_text(family = "sans"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.position = 'top', 
        legend.justification = c(0,0),
        legend.direction = 'horizontal',
        plot.title = element_text(size = 20, 
                                  colour = paleta_its[8],
                                  hjust = -0.08),
        plot.subtitle = element_text(size = 18),
        axis.text.x = element_text(size = 18,
                                   colour = paleta_its[7]),
        axis.text.y = element_text(size = 18,
                                   colour = paleta_its[7]),
        axis.title.y = element_text(size = 18, 
                                    colour = paleta_its[7],
                                    vjust = 2, 
                                    hjust = 0.98),
        plot.margin = unit(c(.5,2,.5,1),"cm"),
        panel.grid.minor.y = element_blank()); plot_creation

ggsave('output/plot2_created_acc.png', width = 12, height = 6, units = 'in', dpi = 100)

created_bots_year <- created_account_peryear %>%
  filter(Resultado == 'Alta')
created_recent <- sum(created_bots_year$n[12:16])/nbots # Ajustar para intervalo de anos que é relevante calcular

## 6. Tweets originais que mais receberam RTs de bots ####

poprts_bots <- bots_twint %>%
  filter(retweet == 'True') %>%
  group_by(tweet, retweet_id) %>%
  summarise(n = n())  %>%
  arrange(desc(n)) %>%
  head(5)

poprts_bots_font <- full_data %>%
  filter(id %in% poprts_bots$retweet_id) %>%
  select(username, tweet, link, id) %>%
  left_join(y = poprts_bots, by = c('id' = 'retweet_id')) %>%
  arrange(desc(n))

## 7. Outros temas #####

# 7.1 Listagem de hashtags recentes
base_twint <- base_twint %>% ungroup
user_data <- full_data %>% select(user_id, Resultado, `Hashtags Recentes`) %>% distinct()

hashtags <- unnest_tokens(base_twint, h, 'hashtags') %>%
  count(h, sort = T) 
hashtags_filtered <- hashtags[4:60,]
hashtags_filtered <- hashtags_filtered[-3,]

plot_tags <- ggplot(hashtags[3:50,], aes(label = h, size = n,  color = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE, shape = 'circle') +
  scale_size_area(max_size = 12) +
  theme_minimal() +
  scale_color_gradient(low = paleta_its[3], high = paleta_its[1]); plot_tags

ggsave('output/plot_tags.png', width = 16, height = 10, units = 'in', dpi = 100)

# 7.2 Listagem de termos comuns nos tweets (excluindo RTs) por dia

clean_text <- full_data %>%
  filter(retweet == 'False') %>%
  mutate(tweet_clean = tweet_lower) %>%
  select(tweet_clean, interval_1day) %>%
  mutate_at(vars(tweet_clean), textprocessing) # essa função textprocessing está no arquivo initialSetup.R

all_words_period <- unnest_tokens(clean_text, w, 'tweet_clean') %>%
  group_by(interval_1day, w) %>%
  count(w, sort = T) %>%
  filter(!w %in% c(stopwords('pt'), 
                   'q', 'é', 'tá', 'ser', 'vai', 
                   'pra', 'para', 'que', 'ou', 'sobre', 'vou'))

# Ajustar para outros intervalos se necessário
days <- unique(all_words_period$interval_1day)

paste0("output/wc_", days[i], ".html")

for (i in 1:length(days)){
  
  top_words_day <- all_words_period %>%
    filter(interval_1day == days[i]) %>%
    arrange(desc(n)) %>%
    head(200)
  
  wc <- wordcloud2(top_words_day[-1], color = paleta_its, shape = 'circle')
  saveWidget(wc, paste0("output/wc_", days[i], ".html"), selfcontained = F)
  webshot(
    paste0("output/wc_", days[i], ".html"), 
    paste0("output/wc_", days[i], ".png"), 
    vwidth = 1300, vheight = 1200, delay = 10)

}


