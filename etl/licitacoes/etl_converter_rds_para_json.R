# ==============================================================================
# ETL: Conversão de Artefatos RDS para JSON (Backend Go - Aurora Licitações)
# Localização: etl/licitacoes/etl_converter_rds_para_json.R
# Objetivo: Ler os arquivos .rds consolidados de licitações e exportar versões
#           .json otimizadas para o backend de alta performance em Go.
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(fs)
})

# Carregamento centralizado de caminhos
if (file.exists("R/paths.R")) {
  source("R/paths.R")
} else if (file.exists("paths.R")) {
  source("paths.R")
} else if (file.exists("../paths.R")) {
  source("../paths.R")
} else if (file.exists("etl/paths.R")) {
  source("etl/paths.R")
}

log_info <- function(msg) {
  cat(sprintf("[%s] [INFO] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_success <- function(msg) {
  cat(sprintf("[%s] [SUCCESS] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_error <- function(msg) {
  cat(sprintf("[%s] [ERROR] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

dir_out <- if (exists("DIR_OUT_AURORA") && dir.exists(DIR_OUT_AURORA)) {
  DIR_OUT_AURORA
} else if (dir.exists("C:/dados/aurora")) {
  "C:/dados/aurora"
} else {
  "data"
}

dir_out <- normalizePath(dir_out, winslash = "/", mustWork = TRUE)
log_info(paste("Diretório de saída identificado:", dir_out))

# 1. dados_licitacoes.rds -> dados_licitacoes.json
src_lic <- file.path(dir_out, "dados_licitacoes.rds")
dst_lic <- file.path(dir_out, "dados_licitacoes.json")
if (file.exists(src_lic)) {
  log_info("Lendo dados_licitacoes.rds...")
  d <- readRDS(src_lic)
  log_info("Exportando dados_licitacoes.json...")
  jsonlite::write_json(d, dst_lic, auto_unbox = TRUE)
  log_success(paste("Criado:", dst_lic))
} else {
  log_error(paste("Arquivo não encontrado:", src_lic))
}

# 2. dados_item_pncp.rds -> dados_item_pncp.json
src_item <- file.path(dir_out, "dados_item_pncp.rds")
dst_item <- file.path(dir_out, "dados_item_pncp.json")
if (file.exists(src_item)) {
  log_info("Lendo dados_item_pncp.rds...")
  item <- readRDS(src_item)
  log_info("Exportando dados_item_pncp.json...")
  jsonlite::write_json(item, dst_item, auto_unbox = TRUE)
  log_success(paste("Criado:", dst_item))
} else {
  log_error(paste("Arquivo não encontrado:", src_item))
}

# 3. dados_carga.rds -> dados_carga.json
src_carga <- file.path(dir_out, "dados_carga.rds")
dst_carga <- file.path(dir_out, "dados_carga.json")
if (file.exists(src_carga)) {
  log_info("Lendo dados_carga.rds...")
  carga <- readRDS(src_carga)
  log_info("Exportando dados_carga.json...")
  jsonlite::write_json(carga, dst_carga, auto_unbox = TRUE)
  log_success(paste("Criado:", dst_carga))
} else {
  log_error(paste("Arquivo não encontrado:", src_carga))
}

log_success("Conversão RDS para JSON concluída!")
