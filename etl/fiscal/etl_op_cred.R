# bibliotecas ----
library(arrow)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(janitor)
library(lubridate)
# library(readxl) # Não será mais necessário, mas mantido para referência
library(stringr)
library(stringi)

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

# Autenticação ----
auth_path <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json") else "data/r-sheets-hugo-0788cc358cb5.json"
if (file.exists(auth_path)) {
  gs4_auth(path = auth_path)
  drive_auth(path = auth_path)
} 

# extrair ----
# URL da sua nova planilha do Google Sheets
url_planilha <- "https://docs.google.com/spreadsheets/d/1Kd6oY6spOymnr7F7F72D4EQLe8kBFGtQODN5zAwJQAs/edit?gid=1916106476#gid=1916106476"

# Lê diretamente do Google Sheets
dados_op_cred <- read_sheet(
  ss = url_planilha,
  sheet = 'Execução por Operação',
  col_types = "c"
) %>%
  clean_names() %>%
  filter(!is.na(contratado_x), !is.na(operacao))

dados_op_cred %>% glimpse()

# Exportação unificada
saveRDS(dados_op_cred, file.path(DIR_OUT_AURORA, 'dados_op_cred.rds'))
write_parquet(dados_op_cred, file.path(DIR_OUT_AURORA, 'dados_op_cred.parquet'))
if (dir.exists(file.path(DIR_RAW_AURORA, 'efisco'))) {
  saveRDS(dados_op_cred, file.path(DIR_RAW_AURORA, 'efisco/dados_op_cred.rds'))
}