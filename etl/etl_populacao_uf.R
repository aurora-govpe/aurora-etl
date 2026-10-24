library(dplyr)
library(ibger)

ibge_aggregates() %>% View()

dados_populacao_uf <- ibge_variables(6579, localities = "N3")

dados_populacao_uf %>% distinct(locality_id) %>% count()

dados_populacao_uf %>% glimpse()

dados_populacao_uf %>% 
  saveRDS(
    'app_aurora_fiscal/data/dados_populacao_uf.rds'
    )

