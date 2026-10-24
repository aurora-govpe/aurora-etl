# pacotes ----
library(dplyr)
library(lubridate)
library(stringr)

# extrair ----
anatel_ibc_municipios_2024 <- data.table::fread(
  'data/anatel/IBC/IBC_municipios_indicadores_originais.csv'
) %>%
  janitor::clean_names()

anatel_ibc_municipios_2024 %>% saveRDS('data/anatel/anatel_ibc_municipios_2024.rds')
