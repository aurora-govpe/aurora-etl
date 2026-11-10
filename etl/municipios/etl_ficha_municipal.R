# ==============================================================================
# SCRIPT: etl_ficha_municipal.R
# Objetivo: Leitura da base de população dos municípios de PE, padronização
#           cadastral e exportação (RDS e Parquet) para o Aurora.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARREGAMENTO DE PACOTES
# ------------------------------------------------------------------------------
library(data.table)
library(dplyr)
library(janitor)
library(arrow)
library(readr)
library(cli)

# ------------------------------------------------------------------------------
# 2. RESOLUÇÃO DE DIRETÓRIOS E AMBIENTE (FALLBACK HIERÁRQUICO)
# ------------------------------------------------------------------------------
is_linux <- Sys.info()["sysname"] == "Linux"
if (is_linux) Sys.setlocale(locale = "pt_BR.utf8")

# Busca hierárquica pelo centralizador paths.R
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../R/paths.R")) {
  source("../R/paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

# Fallbacks defensivos para execuções avulsas
if (!exists("DIR_ETL"))        DIR_ETL        <- getwd()
if (!exists("DIR_RAW_AURORA")) DIR_RAW_AURORA <- file.path(DIR_ETL, "data")
if (!exists("DIR_OUT_AURORA")) DIR_OUT_AURORA <- file.path(DIR_ETL, "data")

# Estrutura de saída
dir_parquet_out <- file.path(DIR_OUT_AURORA, "parquet")
dir.create(DIR_OUT_AURORA,  recursive = TRUE, showWarnings = FALSE)
dir.create(dir_parquet_out, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 3. EXTRAÇÃO E TRATAMENTO
# ------------------------------------------------------------------------------
caminho_csv <- if (file.exists(file.path(DIR_RAW_AURORA, "municipios_pernambuco_populacao.csv"))) {
  file.path(DIR_RAW_AURORA, "municipios_pernambuco_populacao.csv")
} else if (file.exists("data/municipios_pernambuco_populacao.csv")) {
  "data/municipios_pernambuco_populacao.csv"
} else {
  file.path(DIR_ETL, "data/municipios_pernambuco_populacao.csv")
}

if (!file.exists(caminho_csv)) {
  cli_abort("Arquivo de população municipal não encontrado em: {.path {caminho_csv}}")
}

# Se a origem for UTF-8:
ficha_municipal <- fread(caminho_csv, encoding = "UTF-8") %>% 
  clean_names()

# Garantir que todas as colunas de texto fiquem marcadas explicitamente como UTF-8:
ficha_municipal <- ficha_municipal %>%
  mutate(across(where(is.character), enc2utf8))

cli_alert_success("Ficha municipal carregada com sucesso ({nrow(ficha_municipal)} municípios).")

# ------------------------------------------------------------------------------
# 4. EXPORTAÇÃO (RDS E PARQUET)
# ------------------------------------------------------------------------------
# 4.1 RDS
caminho_rds_out <- file.path(DIR_OUT_AURORA, "ficha_municipal.rds")
readr::write_rds(ficha_municipal, caminho_rds_out)

# 4.2 Parquet (Raiz e subpasta parquet/)
caminho_parquet_out <- file.path(DIR_OUT_AURORA, "ficha_municipal.parquet")
caminho_parquet_sub <- file.path(dir_parquet_out, "ficha_municipal.parquet")

arrow::write_parquet(ficha_municipal, caminho_parquet_out, compression = "snappy")
arrow::write_parquet(ficha_municipal, caminho_parquet_sub, compression = "snappy")

# Backup defensivo na pasta raw se ela existir
if (dir.exists(DIR_RAW_AURORA)) {
  readr::write_rds(ficha_municipal, file.path(DIR_RAW_AURORA, "ficha_municipal.rds"))
  arrow::write_parquet(ficha_municipal, file.path(DIR_RAW_AURORA, "ficha_municipal.parquet"), compression = "snappy")
}

cli_alert_success("Arquivos gravados em RDS e Parquet em {.path {DIR_OUT_AURORA}}.")