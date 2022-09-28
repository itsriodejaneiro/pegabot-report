# Exemplos de arquivos


Aqui estão exemplos de arquivos resultantes da execução do script getData.py (coleta pontual).

- tweets_\<termo de busca e timestamp>.csv: arquivo contendo apenas os tweets coletados
- retweets\<termo de busca e timestamp>.csv: arquivo contendo apenas os retweets coletados
- all_\<termo de busca e timestamp>.csv: junção dos arquivos anteriores
- handles_\<termo de busca e timestamp>.csv: seleção apenas dos nomes dos usuários do arquivo anterior
**Nota**: esse arquivo precisa ser editado para retirar a numeração da primeira coluna e renomear a coluna como 'Perfil' antes de subir para o Pegabot lote.
- alltweets\<timestamp>.csv: arquivo que reúne todos tweets e retweets de todos termos de busca. Esse arquivo pode conter registros duplicados de tweets ou retweets a depender dos termos de busca utilizados. A limpeza pode ser feita durante o processo de análsie e cruzamento dos resultados.
