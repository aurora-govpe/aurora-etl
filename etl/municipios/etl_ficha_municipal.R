library(data.table)
library(janitor)

ficha_municipal <- fread(
  'data/municipios_pernambuco_populacao.csv'
) %>% 
  clean_names()

ficha_municipal %>% glimpse()

ficha_municipal %>%
  saveRDS(
    'app_aurora_novo/data/ficha_municipal.rds'
  )
