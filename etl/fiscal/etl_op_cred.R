# ==============================================================================
# SCRIPT: etl_op_cred.R
# Objetivo: Extração de dados de operações de crédito via Google Sheets,
#           limpeza inicial e exportação unificada (RDS e Parquet) para o Aurora.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE BIBLIOTECAS
# ------------------------------------------------------------------------------
library(arrow)
library(dplyr)
library(googledrive)
library(googlesheets4)
library(janitor)
library(lubridate)
library(stringr)
library(stringi)
library(cli)
library(readr)

is_linux <- Sys.info()["sysname"] == "Linux"
if (is_linux) {
  Sys.setlocale(locale = "pt_BR.utf8")
}

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
# Busca hierárquica pelo centralizador paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

# Estrutura de diretórios de saída
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir_efisco_raw  <- file.path(DIR_RAW_AURORA, "efisco")

dir.create(DIR_OUT_AURORA, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_efisco_raw, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. AUTENTICAÇÃO GOOGLE
# ------------------------------------------------------------------------------
auth_path <- if (file.exists(file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json"))) {
  file.path(DIR_RAW_AURORA, "r-sheets-hugo-0788cc358cb5.json")
} else {
  "data/r-sheets-hugo-0788cc358cb5.json"
}

if (file.exists(auth_path)) {
  gs4_auth(path = auth_path)
  drive_auth(path = auth_path)
  cli_alert_success("Autenticação Google realizada via {.path {auth_path}}.")
} else {
  cli_alert_warning("Credencial do Google não encontrada em: {.path {auth_path}}")
}

# ------------------------------------------------------------------------------
# 4. EXTRAÇÃO E TRATAMENTO DOS DADOS
# ------------------------------------------------------------------------------
url_planilha <- "https://docs.google.com/spreadsheets/d/1Kd6oY6spOymnr7F7F72D4EQLe8kBFGtQODN5zAwJQAs/edit?gid=1916106476#gid=1916106476"

dados_op_cred <- tryCatch({
  read_sheet(
    ss        = url_planilha,
    sheet     = "Execução por Operação",
    col_types = "c"
  ) %>%
    clean_names() %>%
    filter(!is.na(contratado_x), !is.na(operacao))
}, error = function(e) {
  cli_abort("Falha ao ler dados da planilha do Google Sheets: {.val {e$message}}")
})

cli_alert_success("Dados de operações de crédito carregados com sucesso ({nrow(dados_op_cred)} registros).")

# ------------------------------------------------------------------------------
# 5. EXPORTAÇÃO LOCAL UNIFICADA (RDS & PARQUET)
# ------------------------------------------------------------------------------
# Saídas em DIR_OUT_AURORA
path_out_rds     <- file.path(DIR_OUT_AURORA, "dados_op_cred.rds")
path_out_parquet <- file.path(DIR_OUT_AURORA, "dados_op_cred.parquet")
path_sub_parquet <- file.path(dir_parquet_out, "dados_op_cred.parquet")

readr::write_rds(dados_op_cred, path_out_rds)
arrow::write_parquet(dados_op_cred, path_out_parquet, compression = "snappy")
arrow::write_parquet(dados_op_cred, path_sub_parquet, compression = "snappy")

# Espelhamento em DIR_RAW_AURORA/efisco
readr::write_rds(dados_op_cred, file.path(dir_efisco_raw, "dados_op_cred.rds"))
arrow::write_parquet(dados_op_cred, file.path(dir_efisco_raw, "dados_op_cred.parquet"), compression = "snappy")

cli_alert_success("Exportação concluída em RDS e Parquet em {.path {DIR_OUT_AURORA}} e backup em {.path {dir_efisco_raw}}.")