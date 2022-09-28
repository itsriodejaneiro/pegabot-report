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
base_twint <- read_csv('Dados/base_twint.csv', col_types = cols(.default = "c"))

## 3. Juntar tudo #####
#bp <- base_pegabot; bp$id_usuario_pb <- bp$`ID do Usuário`#bp$`ID do Usuário` <- as.numeric(bp$`ID do Usuário`)
#twint <- base_twint; #twint$user_id <- as.numeric(twint$user_id)
full_data <- merge(base_twint, base_pegabot, by.x = 'username', by.y = 'Perfil Twitter', all.x = T); #rm(bp); rm(twint)
length(unique(full_data$user_id))

full_data <- full_data[!duplicated(full_data$id, ), ]

# Manter só colunas de interesse
colnames(full_data)
cols_keep <- colnames(full_data)[c(1,4:8,10,13,14,17,18,22:27,31:35,40:52)]
full_data <- full_data %>%
  select(cols_keep); rm(cols_keep)
full_data$user_id <- as.character(full_data$user_id)

# Remover duplicadas
#full_data_cleaned <- full_data[order(all$id.x, -abs(as.numeric(all$nretweets))), ]
#full_data_cleaned <- full_data_cleaned[ !duplicated(full_data_cleaned$id.x),]
#full_data <- full_data_cleaned; rm(full_data_cleaned)
length(unique(full_data$id))
length(unique(full_data$user_id))

# Analises ----------------------------------------------------------------

## 0. Filtrar bots e erros #####

# Porcentagem de 70%+ na lista da hashtag
full_data <- full_data %>% # Separando a base 
  mutate(Resultado = cut(x = as.numeric(`Análise Total`), 
                         breaks = c(0, 70, Inf),
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
  distinct(user_id, username_original)

erros_twint <- full_data %>%
  filter(user_id %in% erro$user_id)

## 1. Stats #####

# Números gerais
ntweets_analisados <- length(unique(full_data$id)); ntweets_analisados
nperfis_total <- length(unique(full_data$user_id)); nperfis_total
nperfis_analisados <- full_data %>%
  select('username') %>%
  drop_na() %>%
  distinct() %>% 
  count(); nperfis_analisados
nerros <- nrow(erro); nerros
percent_erros <- percent(nerros/nperfis_analisados$n[1], 0.1); percent_erros
nposts_erro <- nrow(erros_twint); nposts_erro
percent_posts_erro <- percent(nposts_erro/ntweets_analisados); percent_posts_erro
primeiro_dia <- min(full_data$date); primeiro_dia
ultimo_dia <- max(full_data$date); ultimo_dia

nbots <- length(unique(bots$user_id)); nbots
nbots_twint <- length(unique(bots_twint$user_id)); nbots_twint
percent_bots <- percent(nbots/nperfis_analisados$n, 0.1); percent_bots
nposts_bots <- nrow(bots_twint); nposts_bots
ntweets_bots_twint <- bots_twint %>% filter(retweet == 'False') %>% nrow(); ntweets_bots_twint
nretweets_bots_twint <- bots_twint %>% filter(retweet == 'True') %>% nrow(); nretweets_bots_twint

percent_posts_bots <- percent(nposts_bots/ntweets_analisados, 0.1); percent_posts_bots
percent_tweets_bots <- percent(ntweets_bots_twint/nposts_bots); percent_tweets_bots
percent_retweets_bots <- percent(nretweets_bots_twint/nposts_bots); percent_retweets_bots

total_horas <- round(as.numeric(difftime(ultimo_dia, primeiro_dia, units = "h")),); total_horas
total_dias <- round(as.numeric(difftime(ultimo_dia, primeiro_dia, units = "d")),); total_dias
total_semanas <- round(as.numeric(difftime(ultimo_dia, primeiro_dia, units = "w")),); total_semanas

stats_df <- t(data.frame('Total de tweets' = ntweets_analisados,
                         'Total_perfis analisados' = nperfis_analisados,
                         'Total perfis não analisados' = nerros,
                         'Porc. perfis com erro' = percent_erros,
                         'Inicio' = primeiro_dia,
                         'Fim' = ultimo_dia,
                         'Total de horas' = total_horas,
                         'Total de dias' = total_dias,
                         'Total de semanas' = total_semanas,
                         'Total perfis automatizados' = nbots,
                         'Porc. Total perfis automatizados' = percent_bots,
                         'Posts de perfis automatizados' = nposts_bots,
                         'Porc. Posts de perfis automatizados' = percent_posts_bots))

#rm(ntweets_analisados, nperfis_analisados, primeiro_dia, total_horas,
#   ultimo_dia, nbots, percent_bots, nposts_bots, percent_posts_bots, msg_erro)

output_file <- "resumo.xlsx"

# Salvar arquivo
write.xlsx2(stats_df, output_file, sheetName = "Resumo",
            col.names = F, row.names = TRUE, append = TRUE)

## 2. O que tem na base (tweets e RTs) #####

nretweets <- nrow(full_data[full_data$retweet == 'True',]); nretweets
ntweets <- nrow(full_data[full_data$retweet == 'False',]); ntweets
percent_rts <- percent(nretweets/ntweets_analisados, 0.1); percent_rts
percent_tweets <- percent(ntweets/ntweets_analisados, 0.1); percent_tweets

# paleta_its <- c("#A3248B","#54318C","#255C8E","#00A6B7","#E1F1FD", "#8a8a8a", "#6c6c6c")


# Plot
tweets_rts <- c(`RT` = nretweets, `Tweets` = ntweets)
waffle(title = 'Proporção de RTs e Tweets compartilhados pelos usuários',
       tweets_rts / 1000, rows = 6,
       colors = c(paleta_its[2], paleta_its[6]),
       xlab = 'cada unidade = 10000 registros')

plot_tweets_rts <- waffle(
  tweets_rts / 1000, rows = 6,
  colors = c(paleta_its[2], paleta_its[6]),
  #use_glyph = 'twitter',
  #glyph_size = 7, 
  #glyph_font = "Font Awesome 5 Brands Regular",
  #glyph_font_family = "FontAwesome5Brands-Regular",
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

## 3. Analise Temporal #####
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

# Por dia
ft_temporal <- plyr::count(full_data, 'interval_3hour')
ft_temporal$interval <- format(strptime(ft_temporal$interval_3hour,
                                        "%Y-%m-%d %H:%M"),
                               format = "%d/%m (%Hh)") 
ft_temporal_outline <- boxplot.stats(ft_temporal$freq)$out

peaks <- ft_temporal %>%
  filter(freq %in% ft_temporal_outline); peaks; str(peaks) #%>% droplevels()

# Plot temporal
plot_temporal <- ggplot(ft_temporal, aes(x = fct_reorder(interval, as.Date(interval_3hour)), y = freq)) +
  geom_line(group = 1, color = paleta_its[2], size = 1) +
  #geom_point(data = peaks, aes(x = interval, y = freq), 
  #           color = paleta_its[7], size = 7, alpha = .4) +
  labs(title = "Evolução do compartilhamento dos tweets",
       subtitle = "publicados entre os dias 23/08 e 02/09 de 2022",
       x = "",
       y = "") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   breaks = ft_temporal$interval[seq(1, nrow(ft_temporal), by = 4)]) +
  theme_minimal() +
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
        axis.title.y = element_text(size = 12, 
                                    colour = paleta_its[7],
                                    vjust = 2, 
                                    hjust = 0.98),
        plot.margin = unit(c(.5,2,.5,1),"cm"),
        panel.grid.minor.y = element_blank()); plot_temporal

ggsave('output/plot1_temporal.png', width = 18, height = 10, units = 'in', dpi = 100)

write.xlsx2(ft_temporal, output_file, sheetName = "EvolucaoTemporal",
            col.names = F, row.names = TRUE, append = TRUE)

# Análise dos picos de compartilhamento
filter_tweets_peaks <- full_data %>%
  filter(retweet == 'False') %>%
  filter(interval_1day == '2022-09-01') %>%
  arrange(desc(as.numeric(nretweets)))

percent_peaks_tweets <- percent(length(filter_tweets_peaks$id)/ntweets_analisados); percent_peaks_tweets
#percent_peaks_tweets <- percent(sum(peaks$freq)/ntweets_analisados); percent_peaks_tweets
percent_peaks_days <- percent(nrow(peaks)/total_dias); percent_peaks_days

# Separando o que é só tweet
filter_tweets_peaks_perday <- filter_tweets_peaks %>%
  filter(retweet == 'False') %>%
  arrange(-as.numeric(nretweets))

# Plot dias de pico ###
peaks$interval_1day <- levels(droplevels(peaks$interval_1day))
peaks$interval <- levels(droplevels(peaks$interval))
str(peaks)
levels(peaks) <- peaks$interval_1day

plot_peaks <- ggplot(peaks, aes(x = fct_reorder(interval, as.Date(interval_1day)), y = freq)) +
  geom_bar(stat = "identity", fill = paleta_its[2]) +
  geom_text(aes(label = freq), vjust = 0.5,
            hjust = 1.2,
            color = paleta_its[5],
            size = 5) +
  labs(title = "Dias de pico de compartilhamentos",
       y = "Total de publicações",
       x = "") +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(peaks$interval))) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 13, colour = paleta_its[8]),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 14,  
                                colour = paleta_its[8],
                                hjust = 0.05),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, 
                              colour = paleta_its[8],
                              hjust = -0.09)) +
  gghighlight(freq > 30000, unhighlighted_params = aes(colour = NULL)); plot_peaks

# Quantos bots participaram dos dias de pico
filter_tweets_peaks_bots <- filter_tweets_peaks %>%
  filter(Resultado == 'Alta')

ntweets_peaks_bots <- nrow(filter_tweets_peaks_bots) # qntd de tweets
percent_tweets_peaks_bots <- percent(ntweets_peaks_bots/length(filter_tweets_peaks$id)); percent_tweets_peaks_bots

nbots_peaks <- length(unique(filter_tweets_peaks_bots$user_id)); nbots_peaks
percent_bots_peaks <- percent(nbots_peaks/nbots); percent_bots_peaks

# O que aconteceu até o pico de compartilhamento - AJUSTANDO NA MÂO (TODO: melhorar aqui)
aux_twint <- base_twint
max_intervalo_full <- as.character(max_compartilhamento$intervalo_full)
filter_timeline_atemax <- subset(aux_twint, date <= max_intervalo_full); nrow(filter_timeline_atemax)

# Porcentagem de registros até o intervalo do pico (incluindo o pico)
# Com a porcentagem do que é tweet e RT até esse pico
porc_total_atemax <- (nrow(filter_timeline_atemax)/ntweets_analisados)*100; porc_total_atemax
retweets_atemax <- nrow(filter_timeline_atemax[filter_timeline_atemax$retweet == 'True',]); retweets_atemax
tweets_atemax <- nrow(filter_timeline_atemax[filter_timeline_atemax$retweet == 'False',]); tweets_atemax
percent_rts_atemax <- (retweets_atemax/ntweets_analisados)*100; round(percent_rts_atemax,1);
percent_tweets_atemax <- (tweets_atemax/ntweets_analisados)*100; round(percent_tweets_atemax,1); 

# Dentre os perfis com mais de 70%+, quantos % mencionaram a # até o pico
usuarios_atemax_bots <- bots[tolower(bots$`Perfil Twitter`) %in% tolower(
  paste0('@',filter_timeline_atemax$username)),]
nrow(usuarios_atemax_bots)
(nrow(usuarios_atemax_bots)/nrow(bots))*100

rm(max_compartilhamento, filter_timeline, rt_max_compartilhamento, tweets_max_compartilhamento,
   usuarios_atemax_bots, filter_timeline_atemax, percent_rts_atemax, percent_tweets_atemax,
   aux_twint, retweets_atemax, tweets_atemax, porc_total_atemax)

# Usuarios que interagiram por meio de tweets
nusers_interacted <- length(unique(all$`ID do Usuário`))
percent_users_interacoes <- percent(nusers_interacted/nperfis_analisados, 0.1); percent_users_interacoes
# Quantos são bots
nbot_users_interacted <- all %>%
  filter(Resultado == "Probabilidade de existência de comportamento automatizado") %>%
  distinct(username)
percent(nrow(nbot_users_interacted)/nusers_interacted, 0.1)

## 4. Análises sobre os usuários #####

# Outras métricas de pegabot

bots_temporal <- full_data %>%
  filter(`Resultado Temporal` == "Alta") %>% 
  distinct(user_id, `Análise Total`, `Análise Temporal`)
percent_bots_temporal <- percent(nrow(bots_temporal)/nperfis_analisados); percent_bots_temporal

# Quantos tweets feitos por bots temporal 
bots_temporal_twint <- full_data %>%
  filter(`Resultado Temporal` == "Alta") %>%
  nrow()
percent(bots_temporal_twint/ntweets_analisados)

bots_network <- full_data %>%
  filter(`Resultado Rede` == "Alta") %>% 
  distinct(user_id, `Perfil Twitter`, `Análise Total`, `Análise Rede`)
percent_bots_network <- percent(nrow(bots_network)/nperfis_analisados); percent_bots_network

bots_sentiment <- full_data %>%
  filter(`Resultado Sentimento` == "Alta") %>% 
  distinct(user_id, `Perfil Twitter`, `Análise Total`, `Análise Sentimento`)
percent_bots_sentiment <- percent(nrow(bots_sentiment)/nperfis_analisados); percent_bots_sentiment

# Quantidade de usuários que foram criados nos últimos 3 meses e que são bots pelo
# critério temporal
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
sum(created_bots_year$n[12:16])/nbots
# Para agilizar 3:
write_csv(full_data, 'Dados/Consolidado/full_data_details_2.csv')
full_data <- read_csv('Dados/Consolidado/full_data_details_2.csv', col_types = cols(.default = "c"))

## 5. Análises sobre participação dos usuários por dia/semana #####

# Por semana
per_period <- full_data %>%
  count(interval_1day, Resultado)

# Ajustar o factor
per_period$Resultado <- `levels<-`(addNA(per_period$Resultado), 
                                   c(levels(per_period$Resultado), 'Não analisado'))
levels(per_period$Resultado)

# Ajustar formatação da semana e porcentagem em relação ao total de tweets na semana
per_period <- per_period %>%
  group_by(interval_1day) %>%
  mutate(total = sum(n)) %>% 
  mutate(percent_period = round(n/total*100,1)) %>%
  mutate(begin_period = format(strptime(interval_1day,
                                      "%Y-%m-%d"), # Ajustar aqui para incluir hora, se for o caso
                             format = "%d/%m")) %>%
  mutate(week = factor(paste0('Dia ',  begin_period)))

# Plot ###
plot_vol_user <- ggplot(per_period, aes(fill = factor(Resultado, 
                                                   levels = c('Não analisado', 'Baixa', 'Alta')), 
                                     y = percent_period, 
                                     x = week)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(round(percent_period) > 1, 
                               paste0(round(percent_period), '%'), '')),
            position = position_stack(vjust = .5),
            colour = ifelse(per_period$Resultado == 'Baixa', 
                            paleta_its[5], paleta_its[10]), size = 8) +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(per_period$week))) +
  labs(title = "Volume de publicações por tipo de usuário a cada dia",
       y = "% do total de publicações no dia",
       x = "") +
  theme_minimal() +
  scale_fill_manual(values = paleta_its[c(7,9,2)]) +
  theme(
    axis.text.x = element_text(size = 18),
    #axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 18),
    legend.position = 'top', 
    legend.justification = c(0,0),
    legend.direction = 'horizontal',
    axis.text.y = element_text(size = 20, 
                               colour = paleta_its[8],
                               hjust = 6),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 20,  
                                colour = paleta_its[8],
                                hjust = .06,
                                vjust = -1),
    panel.grid = element_blank(),
    plot.title = element_text(size = 22, 
                              colour = paleta_its[8]
    )); plot_vol_user

ggsave('output/plot4_perperiod.png', width = 12, height = 7, units = 'in', dpi = 100)

per_period_total <- per_period %>%
  select(week, total) %>%
  distinct() %>%
  mutate(percent_ntweetsanalisados = total/ntweets_analisados*100)

# Plot ###
plot_vol_week <- ggplot(per_week_total, aes(y = percent_ntweetsanalisados, 
                                            x = week)) + 
  geom_bar(stat = 'identity') +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(perw$week))) +
  scale_y_continuous(limits = c(0,100)) + 
  labs(title = "Volume semanal de publicações",
       y = "Total de publicações na semana",
       x = "") +
  theme_minimal() +
  scale_fill_manual(values = paleta_its[c(7,9,2)]) +
  theme(
    axis.text.x = element_text(size = 12),
    #axis.ticks.x = element_blank(),
    legend.title = element_blank(),
    legend.position = 'top', 
    legend.justification = c(-.14, 0),
    legend.direction = 'horizontal',
    axis.text.y = element_text(size = 13, 
                               colour = paleta_its[8],
                               hjust = 6),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 14,  
                                colour = paleta_its[8],
                                hjust = .06,
                                vjust = -1),
    panel.grid = element_blank(),
    plot.title = element_text(size = 16, 
                              colour = paleta_its[8],
                              hjust = -.18)); plot_vol_week

per_week_nbots <- full_data %>%
  select(user_id, Resultado, interval_1day) %>%
  distinct() %>%
  filter(Resultado == "Alta") %>%
  group_by(interval_1day) %>%
  count(); per_week_nbots

# Tabela com volume total de tweets por semana e qnt de bots únicos que participaram
per_week_summary <- merge(per_period_total, per_week_nbots) %>%
  select(week, total, percent_ntweetsanalisados, n) %>%
  mutate(percent_ntweetsanalisados = round(percent_ntweetsanalisados,1)) %>%
  mutate(total = paste0(total, ' (', percent_ntweetsanalisados, '%)')) %>%
  select(week, total, n); per_week_summary
colnames(per_week_summary) <- c('Dia', 'Total de publicações', 'Qtd. usuários com alta probabilidade')

week_peaks <- per_week_summary[c(1,3,8,9,11,12), 1]

row_change <- which(per_week_summary$Semana %in% week_peaks)

formatter_week_summary <- formatter("span",
                                    style = x ~ style(
                                      display = "block",
                                      `background-color` = ifelse(x > 0, paleta_its[9], "white"),
                                      color = 'black'))

tb_per_week_summary <- formattable(per_week_summary[2:3], 
                                   align = c('c', 'c')); tb_per_week_summary

.export_formattable(tb_per_week_summary,"output/plot5_tableperiod.png")

# Quantidade de tweets e RTs semanalmente
per_week_type <- full_data %>%
  select(retweet, interval_7day) %>%
  group_by(retweet, interval_7day) %>%
  summarize(n = n()) %>%
  dcast(interval_7day~retweet) %>%
  mutate(total = False + True) %>%
  mutate(percent_tweets_week = percent(False/total),
         percent_rs_week = percent(True/total)); per_week_type

## 6. Análise da distribuição dos tweets por usuário ####

# Verificar o desvio padrão, média e mediana dos registros 
# Agrupar qntd de tweets por usuário
total_tweets_user <- full_data %>% 
  group_by(user_id) %>% 
  summarise(n = n()) 
sum(total_tweets_user$n)

# Estatísticas
sd_tweets <- round(sd(total_tweets_user$n)); sd_tweets
mean_tweets <- round(mean(total_tweets_user$n)); mean_tweets
median_tweets <- round(median(total_tweets_user$n)); median_tweets

min(boxplot.stats(total_tweets_user$n)$out)

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
total_tweets_altafreq <- full_data %>% 
  group_by(user_id) %>% 
  summarise(n = n()) %>% 
  filter(n >= sd_tweets) %>%
  arrange(desc(n))

top10_altafreq <- merge(total_tweets_altafreq, full_data, by = 'user_id') %>%
  select(username, Seguindo, Seguidores, n) %>%
  distinct() %>%
  arrange(desc(n)) %>%
  head(10); top10_altafreq
colnames(top10_altafreq)[1] <- 'Perfil'
colnames(top10_altafreq)[4] <- '# tweets'

write.xlsx2(top10_altafreq, output_file, sheetName = "QtdTweets-QtdUsuarios",
            col.names = F, row.names = TRUE, append = TRUE)

tb_top10_altafreq <- formattable(top10_altafreq, 
                                 align = c('c', 'c')); tb_top10_altafreq

.export_formattable(tb_top10_altafreq,"Imagens/tb_top10_altafreq.png", width = 500)

# Verificando mais sobre esses perfis
top_altafreq <- merge(total_tweets_altafreq, full_data, by = 'user_id') %>%
  select(username, Seguindo, Seguidores, n) %>%
  distinct() %>%
  arrange(desc(n)) %>%
  mutate(Seguindo = as.numeric(Seguindo),
         Seguidores = as.numeric(Seguidores))
str(top_altafreq)

# Ver seguidores/seguindo
ggplot(top_altafreq, aes(x = as.numeric(Seguindo), y = as.numeric(Seguidores))) +
  geom_point() +
  stat_smooth()

boxplot.stats(as.numeric(top_altafreq$Seguindo))$out

library(corrplot)
top_altafreq_check <- top_altafreq %>% drop_na()
x <- cor(top_altafreq_check[3:5])
corrplot(x, type="upper", order="hclust")

# Salvar arquivo
write.xlsx2(total_tweets_altafreq, output_file, sheetName = "PerfisAltaFreqPublicacoes",
            col.names = TRUE, row.names = TRUE, append = TRUE)

# Porcentagem dos usuários com alta frequência de tweets em relação ao total de usuários únicos 
# presentes na base do twint
nusuarios_alta_freq <- nrow(total_tweets_altafreq)
usuarios_alta_freq <- scales::percent(nusuarios_alta_freq/nperfis_total, accuracy = 0.1); usuarios_alta_freq

# Quantidade total de tweets postados por usuários com alta frequência -- sum(total_tweets_freq80$n)
# em relação à quantidade total de tweets coletados -- ntweets_analisados
# Isso vai dar uma ideia do volume de tweets que poucos usuarios publicam em relação ao total de tweets.
ntotal_tweets_altafreq <- sum(total_tweets_altafreq$n)
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
subset_alta_freq <- full_data[full_data$user_id %in% total_tweets_altafreq$user_id,]
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
usuarios_altafreq_bots <- bots[bots$user_id %in% total_tweets_altafreq$user_id,]
nrow(usuarios_altafreq_bots)
(nrow(usuarios_altafreq_bots)/nrow(total_tweets_altafreq))*100

# Qntd de tweets compartilhados por bots com alta frequência de postagem
usuarios_altafreq_bots <- total_tweets_altafreq %>%
  filter(username %in% bots$`Perfil Twitter`)
sum(usuarios_altafreq_bots$n)
# percent em relaação ao total de postagens de usuários com alta frequência
percent(sum(usuarios_altafreq_bots$n)/sum(total_tweets_altafreq$n), accuracy = 0.1);
# percent em relação ao total de tweets analisados
percent(sum(usuarios_altafreq_bots$n)/ntweets_analisados, accuracy = 0.1);

# O que é tweet e retweet de DE BOTS 
nretweets_bots <- nrow(bots_twint[bots_twint$retweet == 'True',]); nretweets_bots
ntweets_bots <- nrow(bots_twint[bots_twint$retweet == 'False',]); ntweets_bots

# Porcentagem em relação ao total
percent_rtsbots <- percent(nretweets_bots/ntweets_analisados, 0.1); percent_rtsbots
percent_tweetsbots <- percent(ntweets_bots/ntweets_analisados, 0.1); percent_tweetsbots

# Porcentagem relação os bots apenas
percent_rtsbots <- (nretweets_bots/nrow(bots_twint))*100; round(percent_rtsbots,1)
percent_tweetsbots <- (ntweets_bots/nrow(bots_twint))*100; round(percent_tweetsbots,1)

rm(percent_rtsbots, percent_tweetsbots, percent_rtsbots, percent_tweetsbots,
   percent_rts, percent_tweets, percent_tweetsbots, percent_rtsbots,
   percent_tweets_alta, percent_rts_alta, retweets_altafreq, tweets_altafreq, 
   tweets_bots, retweets_bots,
   tweets, retweets, parts, mean_tweets, median_tweets, sd_tweets,
   total_tweets_1tweet, total_tweets_altafreq, total_tweets_user,
   usuarios_altafreq_bots, subset_alta_freq, usuarios_alta_freq)

## 6. Explorando mais os usuários com alta atividade

subset_altafreq_pegabot <- base_pegabot %>%
  filter(`Perfil Twitter` %in% paste0('@', total_tweets_altafreq$username))

## 7. Perfis com mais seguidores ####
top_nseguidores <- base_pegabot %>% 
  arrange(desc(Seguidores)) %>% 
  select(`Perfil Twitter`, Seguidores) %>% 
  distinct(`Perfil Twitter`, Seguidores)

# Plot
top_nseguidores$`Perfil Twitter` <- factor(top_nseguidores$`Perfil Twitter`,
                                           levels = top_nseguidores$`Perfil Twitter`[order(top_nseguidores$Seguidores)])

ggplot(top_nseguidores[c(1:10),], aes(x = `Perfil Twitter`, y = Seguidores)) +
  geom_bar(stat = "identity", fill = paleta_its[1]) +
  geom_text(aes(label = Seguidores), vjust = 0.5,
            hjust = 1.2,
            color = paleta_its[5],
            size = 4)  +
  labs(title = "Top 10 usuários com mais seguidores",
       subtitle = expression(paste("considerando ", bold("todos os perfis"))),
       y = "",
       x = "") +
  coord_flip() + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  theme_minimal()

# Salvar arquivo
write.xlsx2(top_nseguidores, output_file, sheetName = "PerfisTopSeguidores",
            col.names = TRUE, row.names = TRUE, append = TRUE)
rm(top_nseguidores)

## 8. Bots Influenciadores #####

# Dentre os perfis com mais de 70%+ aqueles que são mais influenciadores
# (maior número de seguidores)

top_nseguidores_bots <- base_pegabot %>% 
  filter(Resultado == "Probabilidade de existência de comportamento automatizado") %>%
  filter(`Perfil Twitter` != '@BotSentinel') %>%
  arrange(desc(Seguidores)) %>% 
  select(`Perfil Twitter`, Seguidores) %>% 
  distinct(`Perfil Twitter`, Seguidores)

# Plot
top_nseguidores_bots$`Perfil Twitter` <- factor(top_nseguidores_bots$`Perfil Twitter`,
                                                levels = top_nseguidores_bots$`Perfil Twitter`[order(top_nseguidores_bots$Seguidores)])

ggplot(top_nseguidores_bots[c(1:10),], aes(x = `Perfil Twitter`, y = Seguidores)) +
  geom_bar(stat = "identity", fill = paleta_its[1]) +
  geom_text(aes(label = Seguidores), vjust = 0.5,
            hjust = 1.2,
            color = paleta_its[5],
            size = 4)  +
  labs(title = "Top 10 usuários com mais seguidores",
       subtitle = expression(paste("considerando os perfis com ", bold("alta"), " probabilidade de automação")),
       y = "",
       x = "") +
  coord_flip() + 
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  theme_minimal()

# Salvar arquivo
write.xlsx2(top_nseguidores_bots, output_file, sheetName = "BotsTopSeguidores",
            col.names = TRUE, row.names = TRUE, append = TRUE)
rm(top_nseguidores_bots)

## 9. Bots com mais RTs #####

# Filtrar RTs
populares_retweets_bots <- bots_twint %>%
  filter(retweet == 'True') %>%
  group_by(username) %>%
  summarise(RTs = n())  %>%
  arrange(desc(RTs)) 

populares_retweets_bots$username <- paste0('@', populares_retweets_bots$username)
populares_retweets_bots$username <- factor(populares_retweets_bots$username,
                                           levels = populares_retweets_bots$username[order(populares_retweets_bots$RTs)])

# Filtrar tweets
populares_tweets_bots <- bots_twint %>%
  filter(retweet == 'False') %>%
  group_by(username) %>%
  summarise(Tweets = n())  %>%
  arrange(desc(Tweets)) 

populares_tweets_bots$username = paste0('@', populares_tweets_bots$username)
populares_tweets_bots$username <- factor(populares_tweets_bots$username,
                                         levels = populares_tweets_bots$username[order(populares_tweets_bots$Tweets)])

# Juntar RTs e Tweets
join <- merge(populares_retweets_bots, populares_tweets_bots, by = 'username', all.x = TRUE) %>%
  arrange(desc(RTs)) %>% head(10)

# Aplicar função melt para ajustar o formato do dataframe
# para o plot de 2 barras em uma
dfm <- melt(join, id.vars=c("username"), measure.vars=c("RTs","Tweets"),
            variable.name="Conteúdo", value.name="total")
dfm$username = factor(dfm$username, levels = join$username[order(join$RTs)])

# Plot
ggplot(dfm, aes(x = username, y = total, group = -Conteúdo)) + 
  geom_col(aes(fill=Conteúdo)) +
  #geom_text(aes(label = sum(total)), vjust = 0.5,
  #          hjust = 1.2,
  #          color = paleta_its[5],
  #          size = 4)  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25)) +
  labs(title = "Top 10 usuários que mais retweetaram",
       subtitle = "e a quantidade de tweets",
       x = "", y = "") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = paleta_its[c(2,6)])

# Salvar arquivo
write.xlsx2(join, output_file, sheetName = "Usuários que mais retuitaram (automatizado)",
            col.names = TRUE, row.names = TRUE, append = TRUE)

rm(join, dfm, populares_tweets_bots)

## 10. Análise sobre qntd de perfis que pontuaram alto na análise temporal #####

users_hightemporal <- full_data %>%
  filter(`Resultado Temporal` == 'Alta') %>%
  distinct(user_id)

nposts_users_hightemporal <- full_data %>%
  filter(`Resultado Temporal` == 'Alta')

nusers_hightemporal <- users_hightemporal %>% count(); nusers_hightemporal
percent(nusers_hightemporal$n/nperfis_analisados, 0.1)
percent(nrow(nposts_users_hightemporal)/ntweets_analisados)

nusers_hightemporal_bots <- base_pegabot %>%
  filter(Resultado == "Alta") %>% 
  filter(Temporal == 'Alta') %>%
  filter(`Perfil Twitter` %in% bots_twint$username)

percent(nrow(nusers_hightemporal_bots)/nbots_twint, 0.1)

## 9. Análise temporal dos tweets dos usuários mais centrais na rede

centrais <- c('@taoquei1', '@jouberth19', '@carlazambelli38', 
              '@bolsonarosp', '@bolsomito_2', '@allusapore')

# Filtrar tweets dessas pessoas

tweets_usercentrais <- full_data %>%
  filter(retweet == 'False') %>%
  filter(username %in% centrais) 

when_tweets_usercentrais <- tweets_usercentrais %>%
  select(username, interval_1day) %>%
  distinct() %>%
  group_by(interval_1day) %>%
  count(); when_tweets_usercentrais

# Filtrar os RTs desses tweets

rts_tweets_centrais <- full_data %>%
  filter(retweet == 'True') %>%
  mutate(tweet = tolower(tweet))

rts <- c()

for (i in seq(1:nrow(tweets_usercentrais))){
  aux <- rts_tweets_centrais[which(str_detect(rts_tweets_centrais$tweet, substr(tweets_usercentrais$tweet[i], 1, 25)) == TRUE),]
  rts <- rbind(rts, aux)
}

# outra tentativa
centrais_edit <- paste0('rt ', centrais,':')
for (i in seq(1:length(centrais_edit))){
  rts_centrais <- rts_tweets_centrais[which(str_detect(rts_tweets_centrais$tweet, centrais_edit[i]) == TRUE),]
  rts_centrais$user_font <- centrais[i] 
  rts <- rbind(rts, rts_centrais)
}

# O quanto esses tweets representam do total
percent(nrow(rts)/nretweets)
# Qntd de usuários únicos que publicaram os RTs
length(unique(rts$user_id))

freq_table <- count(rts, user_font, interval_1day)
freq_table <- freq_table %>%
  group_by(user_font) %>%
  mutate(cum_n = cumsum(n))
freq_table <- left_join(freq_table, when_tweets_usercentrais, by = 'interval_1day')

freq_table$interval <- format(strptime(freq_table$interval_1day, "%Y-%m-%d"),
                              format = "%d/%m")
breaks_dates <- levels(fct_reorder(freq_table$interval, as.Date(freq_table$interval_1day)))[seq(1,65, by = 5)]

scale_color_discrete(name="Cylinders",
                     breaks=c(8,6,4),
                     labels=c("8 Cylinder","6 Cylinder","4 Cylinder"))

ggplot(data = freq_table, 
       aes(x = fct_reorder(interval, as.Date(interval_1day)), y = cum_n)) +
  geom_line(aes(color = freq_table$user_font, group = freq_table$user_font)) +
  geom_point(aes(x = fct_reorder(interval, as.Date(interval_1day)), y = n.y), 
             color = paleta_its[7], size = 5, alpha = .4) +
  labs(title = "Evolução do compartilhamento de RTs",
       subtitle = "dos usuários mais centrais (acumulado)",
       x = "",
       y = "", colour = 'Perfil') +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   breaks = breaks_dates) +
  scale_y_continuous(breaks = seq(0, max(freq_table$cum_n) + 200, 500),
                     name = "Nº de tweets acumulado") +
  scale_color_manual(values = paleta_its[1:8], 
                     labels = c(unique(freq_table$user_font), 'Dias de publicação')) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, 
                                  colour = paleta_its[8],
                                  hjust = -0.08),
        plot.subtitle = element_text(size = 16, 
                                     hjust = -0.08,
                                     colour = paleta_its[8]),
        axis.title.y = element_text(size = 16, 
                                    colour = paleta_its[7],
                                    vjust = 2, 
                                    hjust = 0.98),
        axis.text.x = element_text(size = 16,
                                   colour = paleta_its[7]),
        axis.text.y = element_text(size = 16,
                                   colour = paleta_its[7]),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.margin = unit(c(.5,2,.5,1),"cm"),
        panel.grid.minor.y = element_blank());

ggsave('output/plot6_evolucaorts_centrais.png', width = 18, height = 10, units = 'in', dpi = 100)

## 11. Tweets originais de RTs mais comuns feitos por bots ####

poprts_bots <- bots_twint %>%
  filter(retweet == 'True') %>%
  group_by(tweet, retweet_id) %>%
  summarise(n = n())  %>%
  arrange(desc(n)) %>%
  head(10)

poprts_bots_font <- full_data %>%
  filter(id %in% poprts_bots$retweet_id) %>%
  select(username, tweet, link, id) %>%
  left_join(y = poprts_bots, by = c('id' = 'retweet_id')) %>%
  arrange(desc(n))

# Salvar arquivo
write.xlsx2(populares_retweets_bots_conteudo, output_file, 
            sheetName = "Tweets com mais RTs (por usuários automat.)",
            col.names = TRUE, row.names = TRUE, append = TRUE)

## 12. Outras hashtags #####

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


# Exemplo de tweet com hashtag incomum

hashtag_incomum <- base_twint[which(grepl('bbb21', base_twint$hashtags)), ]
hashtag_incomum <- hashtag_incomum %>% 
  arrange(date) %>%
  head(1) %>%
  select(link,date,tweet)

# Quantas vezes ele apareceu

n_tweentincomum <- base_twint[which(grepl(hashtag_incomum$tweet,base_twint$tweet)), ]

# Salvar arquivo
write.xlsx2(hashtags, output_file, 
            sheetName = "Hashtags mencionadas (por usuários automat.)",
            col.names = TRUE, row.names = TRUE, append = TRUE)
rm(hashtag_incomum, hashtags)

## 13. RTs mais comuns em toda a base ####

ids_populares_retweets <- full_data %>%
  filter(retweet == 'True', ) %>%
  group_by(retweet_id) %>%
  summarise(n = n())  %>%
  arrange(desc(n)) %>% 
  head(10)

populares_retweets <- full_data %>%
  filter(id %in% ids_populares_retweets$retweet_id) %>%
  select(tweet, username, link, date, id) 

populares_retweets <- merge(ids_populares_retweets, populares_retweets,
                            by.x = 'retweet_id', by.y = 'id', all.y = TRUE) %>% arrange(desc(n))

# Total
sum(populares_retweets$n)
# Porcentagem em relação ao total de registros
(sum(populares_retweets$n)/ntweets_analisados)*100

RTsPopulares <- full_data[full_data$retweet_id %in% populares_retweets$retweet_id,]

bots_RTsPopulares <- RTsPopulares %>%
  filter(Resultado == "Alta") %>% 
  distinct(user_id)

nrow(bots_RTsPopulares) # Bots que participaram dos RTs
porc_bots_RTpopulares <- (nrow(bots_RTsPopulares)/nperfis_analisados)*100; porc_bots_RTpopulares
porc_bots_RTpopulares_bots <- (nrow(bots_RTsPopulares)/nbots)*100; porc_bots_RTpopulares_bots

# Salvar arquivo
write.xlsx2(populares_retweets, output_file, 
            sheetName = "Tweets com mais RTs (todos os usuários)",
            col.names = TRUE, row.names = TRUE, append = TRUE)

## 14. Analise temporal dos top 10 RTs #####

all_topRT <- all[all$tweet %in% populares_retweets$tweet, ]
all_topRT <- merge(all_topRT, populares_retweets, by.x = 'tweet', by.y = 'tweet')

freq_table <- count(all_topRT, username_fonte, intervalo_10min)

top3RTfonte <- freq_table %>%
  group_by(username_fonte) %>%
  summarise(n = sum(n))  %>%
  arrange(desc(n)) %>% 
  head(3)

freq_table <- freq_table[which(freq_table$username_fonte %in% top3RTfonte$username_fonte),]

freq_table$intervalo <- format(strptime(freq_table$intervalo_10min,
                                        "%Y-%m-%d %H:%M:%S"),
                               format = "%d/%m %H:%M")

ggplot(freq_table, aes(x = intervalo,
                       y = n, color = username_fonte, group = username_fonte)) +
  geom_line() +
  labs(title = "Evolução do compartilhamento dos RTs mais populares",
       x = "",
       y = "", colour = 'Perfil com mais RTs') +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(freq_table$n) + 100, 20)) +
  scale_color_manual(values = paleta_its[1:3]) +
  theme(axis.text.x = element_text(angle = 45, size = 6))

## 15. Analise temporal dos top 10 RTs - Acumulado #####

all_topRT <- all[all$tweet %in% populares_retweets$tweet, ]
all_topRT <- merge(all_topRT, populares_retweets, by.x = 'tweet', by.y = 'tweet')

freq_table <- count(all_topRT, username_fonte, intervalo_full)
freq_table <- freq_table %>%
  group_by(username_fonte) %>%
  mutate(cum_n = cumsum(n))

freq_table <- freq_table[which(freq_table$username_fonte %in% top3RTfonte$username_fonte),]

freq_table$intervalo <- format(strptime(freq_table$intervalo_full,
                                        "%Y-%m-%d %H:%M:%S"),
                               format = "%d/%m %H:%M")

ggplot(freq_table, aes(x = fct_inorder(intervalo, intervalo_10min),
                       y = cum_n, color = username_fonte, group = username_fonte)) +
  geom_line() +
  labs(title = "Evolução do compartilhamento de RTs dos tweets mais populares (acumulado)",
       x = "",
       y = "", colour = 'Perfil com mais RTs') +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE),
                   breaks = freq_table$intervalo[seq(1, 2000, by = 100)]) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(freq_table$cum_n) + 200, 500),
                     name = "Nº de tweets") +
  scale_color_manual(values = paleta_its[1:3]) +
  theme(axis.text.x = element_text(angle = 20, size = 13),
        axis.text.y = element_text(size = 14),
        plot.margin = unit(c(.5,2,.5,1),"cm"),
        #panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


## 16. Analise de menções dos bots #####

bots <- base_pegabot %>%
  filter(Resultado == "Probabilidade de existência de comportamento automatizado") %>%
  ungroup

colnames(bots)[16] <- 'hashtags'
hashtags_bots <- unnest_tokens(bots, h, hashtags) %>%
  #filter(h != 'ninguemmexecomernesto') %>%
  count(h, sort = T) %>%
  select(h, n)

ggplot(hashtags_bots[1:50,], aes(label = h, size = n,  color = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  scale_color_gradient(low = paleta_its[3], high = paleta_its[1])


## 17. Qntd posts por min, agrupado por usuário #####
freq_usuarios <- base_twint %>%
  count(username, intervalo_5min) %>%
  arrange(username, intervalo_5min) %>%
  arrange(-n)

volume_5min <- base_twint %>%
  filter(username == freq_usuarios$username[2]) %>%
  filter(intervalo_5min == freq_usuarios$intervalo_5min[2])


freq_usuarios_bots <- freq_usuarios[freq_usuarios$username %in% gsub('@', '', bots$`Perfil Twitter`),]

# Usuarios que passam o limite de 1 post por minuto

tweets_botuser <- bots_twint %>% 
  group_by(username) %>% 
  summarise(n = n()) %>%
  arrange(desc(n))

# Linha do recomendado
# Usuarios bots que mais publicam por hora (total tweets/qtd de horas total da analise) - quant
# oque publicam - qualitativo

tweets_botuser$rate <- tweets_botuser$n/total_horas
ideal_rate <- 5/24

## 13. Quantos tweets por dia

base_twint$intervalo_dia <- cut(as.POSIXct(base_twint$date),
                                breaks = "1 day")

freq_dia <- base_twint %>%
  count(intervalo_dia)
freq_dia$intervalo_dia <- strftime(freq_dia$intervalo_dia,"%d/%m")

ggplot(freq_dia, aes(x = intervalo_dia, y = n)) +
  geom_bar(stat = "identity", fill = paleta_its[1]) +
  geom_text(aes(label = paste0(n, '\n', intervalo_dia)), vjust = -0.2,
            color = paleta_its[1],
            size = 4)  +
  labs(title = "Quantidade de compartilhamentos por dia",
       subtitle = expression(paste("considerando os tweets nativos e RTs que mencionam as #s")),
       y = "",
       x = "") +
  expand_limits(y = c(1, max(freq_dia$n + 2000))) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()) 

## 18. Qntd de tweets que tem alguma url #####
# Juntar funções para extrair urls normais e encurtadas
base_twint$url <- ex_url(base_twint$tweet)
base_twint$url <- rm_twitter_url(base_twint$url)

str(base_twint$url)
# Qnts tweets tem pelo menos um url
ntweets_urls <- base_twint %>%
  filter(!is.na(url))

## Combine removing Twitter URLs and standard URLs
rm_twitter_n_url(x)
rm_twitter_n_url(x, extract=TRUE)
#TODO
## 16. Qntd de menções nos tweets ##### #TODO
## 19. Sobre que DallasGin21 e @rgacowboy_rga estão falando #####

specific_users_twint <- base_twint %>%
  filter(username == '@dallasgin21' | username == '@rgacowboy_rga')

# Percent em relação ao total de tweets
percent(nrow(specific_users_twint)/nrow(base_twint))
# Quantos tweets e RTs
rts_specific <- specific_users_twint %>% filter(retweet == 'True')
percent(nrow(rts_specific)/nrow(specific_users_twint))
tweets_specific <- specific_users_twint %>% filter(retweet == 'False')
percent(nrow(tweets_specific)/nrow(specific_users_twint))

specific_users_pegabot <- base_pegabot %>%
  filter(`Perfil Twitter` == '@dallasgin21' | `Perfil Twitter` == '@rgacowboy_rga')

specific_users_twint$nlikes <- as.integer(specific_users_twint$nlikes); min(specific_users_twint$nlikes)
specific_users_twint$nretweets <- as.integer(specific_users_twint$nretweets); min(specific_users_twint$nretweets)
specific_users_twint$nreplies <- as.integer(specific_users_twint$nreplies); min(specific_users_twint$nreplies)

tweets_dup <- specific_users_twint %>%
  group_by(tweet) %>%
  count() %>%
  arrange(-n) %>% 
  filter(n == 2)

percent(nrow(tweets_dup)/nrow(specific_users_twint))


tokens_specific_user <- unnest_tokens(specific_users_pegabot, h, 'Hashtags Recentes') %>%
  #filter(h != 'ninguemmexecomernesto') %>%
  count(h, sort = T)


ggplot(hashtags[2:20,], aes(label = h, size = n,  color = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE, shape = 'circle') +
  scale_size_area(max_size = 18) +
  theme_minimal() +
  scale_color_gradient(low = paleta_its[3], high = paleta_its[1])


# Pegar o texto dos tweets e o texto de rts, separados
# já que o RT normalmente fica cortado
# pra contar a quantidade de palavras e ver a quantidade de 
# tweets que só tem menções

tweets <- base_twint %>%
  filter(retweet == 'False') %>%
  select(username, id, text = tweet) %>%
  mutate(type = 'tweet')
rts <- base_twint %>%
  filter(retweet == 'True') %>%
  select(username, id, text = user_rt) %>%
  mutate(type = 'rt')


## 20. Linha do tempo de engajamento #####

full_data <- full_data %>%
  mutate(engajamento = as.numeric(nreplies) + as.numeric(nlikes) + as.numeric(nretweets))

top_engajamento <- full_data %>%
  filter(retweet == 'False') %>%
  group_by(interval_1day) %>%
  arrange(desc(engajamento)) %>%
  slice(1:5) %>%
  select(username_original, engajamento, interval_1day, date, conversation_id, id)
  
write.csv(top_engajamento,"File Name.csv", row.names = FALSE)

# Pegabot
write.xlsx2(base_pegabot, output_file, sheetName = "Resultados Pegabot",
            col.names = TRUE, row.names = TRUE, append = TRUE)
# Base twint
write.xlsx2(base_twint, output_file, sheetName = "Twint",
            col.names = TRUE, row.names = TRUE, append = TRUE)
# All
write.xlsx2(all, output_file, sheetName = "Twint e Pegabot",
            col.names = TRUE, row.names = TRUE, append = TRUE)

## 21. Interaçções de bots com usuários com mais engajamento #####

bots_topengajamento <- bots_twint %>%
  filter(conversation_id %in% top_engajamento$id | retweet_id %in% top_engajamento$id) %>%
  group_by(retweet_id) %>%
  count() %>%
  arrange(desc(n))

bots_topengajamento_rts <- merge(bots_topengajamento, top_engajamento, 
      by.x = 'retweet_id', by.y = 'id') %>%
  arrange(desc(n)) %>%
  head() %>% 
  select(username_original, link, n, engajamento)
  

## 22. Interações de bots por termo de busca #####

all_termos <- full_data %>% 
  group_by(search) %>%
  count() %>%
  arrange(desc(n))

bots_termos <- bots_twint %>%
  group_by(search) %>%
  count() %>% 
  mutate(percent_botstwint = percent(n/nposts_bots),
         percent_twint = percent(n/ntweets_analisados)) %>%
  merge(all_termos, by.x = 'search', by.y = 'search') %>%
  arrange(desc(n.x))

bots_termos$search <- gsub(' lang:pt', '', bots_termos$search)
colnames(bots_termos) <- c('Termo', '', '% de tweets de u. automatizados', '% de todos os tweets', '# tweets coletados')

tb_per_term_bot <- formattable(bots_termos[,-2], 
                                   align = c('c', 'c')); tb_per_term_bot

.export_formattable(tb_per_term_bot,"output/plot6_tabletermbot.png", width = 700)

