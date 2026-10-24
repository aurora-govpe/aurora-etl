library(data.table)
library(googledrive)
library(janitor)
library(tidyverse)

# 1. Autenticação com a Conta de Serviço (Service Account JSON)
drive_auth(
  path = "data/r-sheets-hugo-0788cc358cb5.json"
)

# 2. ID da Pasta extraído da URL do Google Drive
folder_id <- "1tEyjYwBCqBqhfpkSQwDNVr5mx86d9Oe4"

# 3. Listar os arquivos contidos na pasta do Drive
arquivos_pasta <- drive_ls(as_id(folder_id))
print(arquivos_pasta)

# 4. Filtrar o arquivo CSV desejado
# Opção A: Pegar o primeiro arquivo .csv encontrado
arquivo_alvo <- arquivos_pasta %>%
  filter(str_detect(name, "(?i)\\.csv$")) %>%
  slice(1)

# 5. Criar arquivo temporário local e fazer o download
caminho_temp <- tempfile(fileext = ".csv")

drive_download(
  file = as_id(arquivo_alvo$id),
  path = caminho_temp,
  overwrite = TRUE
)

# 6. Leitura do arquivo para um Data Frame no R
df_ibc <- fread(
  caminho_temp) %>%
  clean_names()

# Checagem da estrutura lida
df_ibc %>% glimpse()

df_ibc_pe <- df_ibc %>%
  # Filtrar apenas municípios de Pernambuco (UF == "PE" ou código IBGE iniciando com 26)
  filter(uf == "PE" | str_starts(as.character(codigo_municipio), "^26")) %>%
  
  # Mapear chaves territoriais/temporais
  mutate(
    cod_municipio  = as.character(codigo_municipio),
    nm_municipio   = municipio,
    ano            = as.numeric(ano),
    mes_referencia = NA_character_
  ) %>%
  
  # Renomear as colunas originais para as siglas oficiais do dicionário
  rename(
    ibc_anatel      = ibc,
    cob_pop_4g5g    = cobertura_pop_4g5g,
    dens_smp        = densidade_smp,
    hhi_smp         = hhi_smp,
    dens_scm        = densidade_scm,
    hhi_scm         = hhi_scm,
    adens_estacoes  = adensamento_estacoes,
    cob_fibra       = fibra,
    cob_area_agric  = cobertura_area_agricultavel
  ) %>%
  
  # Converter todas as métricas para <chr> e tratar vazios ("") como NA
  # Evita o erro em pivot_longer ao misturar <dbl>, <int> e <chr> (ex: cobertura_area_agricultavel)
  mutate(across(
    c(ibc_anatel, cob_pop_4g5g, dens_smp, hhi_smp, dens_scm, 
      hhi_scm, adens_estacoes, cob_fibra, cob_area_agric),
    ~ na_if(as.character(.x), "")
  ))

# 2. Pivotagem para o Formato Vertical (Tidy Data)
df_ibc_long <- df_ibc_pe %>%
  pivot_longer(
    cols = c(ibc_anatel, cob_pop_4g5g, dens_smp, hhi_smp, dens_scm, 
             hhi_scm, adens_estacoes, cob_fibra, cob_area_agric),
    names_to  = "sigla_indicador",
    values_to = "indicador"
  ) %>%
  filter(!is.na(indicador)) %>%
  select(cod_municipio, nm_municipio, ano, sigla_indicador, indicador, mes_referencia)

# 3. Enriquecimento com o Dicionário do Projeto
lista_de_indicadores <- readxl::read_excel("data/indicadores/lista de indicadores.xlsx")

df_anatel_final <- df_ibc_long %>%
  left_join(
    lista_de_indicadores %>% select(sigla_indicador, nm_indicador, grupo, tipo),
    by = "sigla_indicador"
  )

# 4. Salvamento em Parquet (Staging)
write_parquet(df_anatel_final, "data/anatel/df_anatel_ibc_long.parquet")