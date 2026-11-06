# ==============================================================================
# SCRIPT: EXTRAÇÃO E PROCESSAMENTO DE DADOS - PAC
# Objetivo: Leitura da base do PAC via Google Sheets, tratamento de valores,
#           exportação para o Aurora e upload no Google Drive.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(BigDataPE)
library(bslib)
library(cli)
library(fs)
library(glue)
library(googledrive)
library(googlesheets4)
library(janitor)
library(purrr)
library(readxl)
library(rmapshaper)
library(sf)
library(stringi)
library(stringr)
library(tidyverse)
library(vialactea)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
options(scipen = 999)

# Busca hierárquica pelo paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_DADOS"))      DIR_DADOS      <- DIR_OUT_AURORA

# Autenticação Google Drive / Sheets via DIR_RAW_AURORA
google_token_hugo <- file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")

if (file.exists(google_token_hugo)) {
  drive_auth(cache = FALSE, path = google_token_hugo)
  gs4_auth(path = google_token_hugo, scopes = "https://www.googleapis.com/auth/spreadsheets")
  cli_alert_success("Autenticação Google realizada com sucesso via {.path {google_token_hugo}}.")
} else {
  cli_alert_warning("Credencial do Hugo não encontrada em: {.path {google_token_hugo}}")
}

# ------------------------------------------------------------------------------
# 3. EXTRAÇÃO E PROCESSAMENTO DOS DADOS (PAC)
# ------------------------------------------------------------------------------
url_pac <- "https://docs.google.com/spreadsheets/d/17JWmV_f5fE3y-mZArg6LRos__FCzTdQEqHTAflJx-a8/edit?gid=801356181#gid=801356181"

dados_pac_raw <- read_sheet(
  ss    = url_pac, 
  sheet = "base"
)

dados_pac <- dados_pac_raw %>%
  janitor::clean_names() %>%
  dplyr::rename(
    estimativa_de_valor_total = estimativa_de_valor_total_do_empreendimento_2023_2030_r
  ) %>%
  dplyr::mutate(
    # Extrai o valor numérico diretamente da lista com segurança
    estimativa_de_valor_total = purrr::map_dbl(
      estimativa_de_valor_total, 
      ~ {
        if (is.null(.x) || length(.x) == 0 || is.na(.x[1])) {
          NA_real_
        } else {
          as.numeric(.x[1])
        }
      }
    ),
    
    # Converte colunas numéricas
    execucao_fisica_percent      = as.numeric(execucao_fisica_percent),
    ano_de_previsao_de_conclusao = as.numeric(ano_de_previsao_de_conclusao),
    
    # Trata NAs em colunas de texto
    dplyr::across(where(is.character), ~tidyr::replace_na(., "Não Informado"))
  )

dados_pac %>% glimpse()

# ------------------------------------------------------------------------------
# 4. EXPORTAÇÃO LOCAL E UPLOAD NO GOOGLE DRIVE
# ------------------------------------------------------------------------------
# ID da pasta de destino no Google Drive
gdrive_folder_id <- googledrive::as_id("1IU7unR4LOscOCXB92xGufCxX4z2kKEjf")

# --- Exportação RDS ---
path_out_rds <- file.path(DIR_OUT_AURORA, "dados_pac.rds")
write_rds(dados_pac, path_out_rds)
cli_alert_success("Arquivo local RDS gravado em: {.path {path_out_rds}}")

if (file.exists(path_out_rds)) {
  drive_upload(
    media     = path_out_rds,
    path      = gdrive_folder_id,
    name      = "dados_pac.rds",
    overwrite = TRUE
  )
  cli_alert_success("Arquivo {.file dados_pac.rds} enviado ao Google Drive com sucesso!")
} else {
  cli_alert_danger("Arquivo RDS para upload não localizado em: {.path {path_out_rds}}")
}

# --- Exportação Parquet ---
dir_parquet <- file.path(DIR_OUT_AURORA, "parquet")
if (!dir.exists(dir_parquet)) {
  dir.create(dir_parquet, recursive = TRUE)
}

path_out_parquet <- file.path(dir_parquet, "dados_pac.parquet")
arrow::write_parquet(dados_pac, path_out_parquet)
cli_alert_success("Arquivo local Parquet gravado em: {.path {path_out_parquet}}")

if (file.exists(path_out_parquet)) {
  drive_upload(
    media     = path_out_parquet,
    path      = gdrive_folder_id,
    name      = "dados_pac.parquet",
    overwrite = TRUE
  )
  cli_alert_success("Arquivo {.file dados_pac.parquet} enviado ao Google Drive com sucesso!")
} else {
  cli_alert_danger("Arquivo Parquet para upload não localizado em: {.path {path_out_parquet}}")
}