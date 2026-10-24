# ==============================================================================
# ETL: Conversão de Artefatos RDS para JSON (Backend Go - Aurora Fiscal)
# Localização: etl/fiscal/etl_converter_rds_para_json.R
# Objetivo: Ler os arquivos .rds do app_aurora_fiscal e exportar versões .json
#           otimizadas para o backend de alta performance em Go.
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
  library(geojsonsf)
  library(fs)
})

log_info <- function(msg) {
  cat(sprintf("[%s] [INFO] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_success <- function(msg) {
  cat(sprintf("[%s] [SUCCESS] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

log_error <- function(msg) {
  cat(sprintf("[%s] [ERROR] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}

# 1. Determina dinamicamente o caminho da pasta data do app_aurora_fiscal
candidatos_pasta_data <- c(
  "app_aurora_fiscal/data",
  "../../app_aurora_fiscal/data",
  file.path(getwd(), "app_aurora_fiscal/data")
)

pasta_data <- NULL
for (caminho in candidatos_pasta_data) {
  if (dir.exists(caminho)) {
    pasta_data <- normalizePath(caminho, winslash = "/", mustWork = TRUE)
    break
  }
}

if (is.null(pasta_data)) {
  stop("Não foi possível localizar o diretório 'app_aurora_fiscal/data'. Execute a partir da raiz do projeto.")
}

log_info(paste("Diretório de dados identificado:", pasta_data))

# ------------------------------------------------------------------------------
# 2. Conversão: base_painel.rds -> base_painel.json
# ------------------------------------------------------------------------------
caminho_base_painel_rds <- file.path(pasta_data, "base_painel.rds")
caminho_base_painel_json <- file.path(pasta_data, "base_painel.json")

if (file.exists(caminho_base_painel_rds)) {
  log_info("Lendo base_painel.rds...")
  base_painel <- readRDS(caminho_base_painel_rds)
  
  log_info("Exportando base_painel.json...")
  jsonlite::write_json(base_painel, caminho_base_painel_json, auto_unbox = TRUE)
  log_success(paste("Criado:", caminho_base_painel_json))
} else {
  log_error(paste("Arquivo não encontrado:", caminho_base_painel_rds))
}

# ------------------------------------------------------------------------------
# 3. Conversão: base_painel_mun.rds -> base_painel_mun.json
# ------------------------------------------------------------------------------
caminho_base_painel_mun_rds <- file.path(pasta_data, "base_painel_mun.rds")
caminho_base_painel_mun_json <- file.path(pasta_data, "base_painel_mun.json")

if (file.exists(caminho_base_painel_mun_rds)) {
  log_info("Lendo base_painel_mun.rds...")
  base_painel_mun <- readRDS(caminho_base_painel_mun_rds)
  
  log_info("Exportando base_painel_mun.json...")
  jsonlite::write_json(base_painel_mun, caminho_base_painel_mun_json, auto_unbox = TRUE)
  log_success(paste("Criado:", caminho_base_painel_mun_json))
} else {
  log_error(paste("Arquivo não encontrado:", caminho_base_painel_mun_rds))
}

# ------------------------------------------------------------------------------
# 4. Conversão: populacao_mun.rds -> populacao_mun.json
# ------------------------------------------------------------------------------
caminho_populacao_rds <- file.path(pasta_data, "populacao_mun.rds")
caminho_populacao_json <- file.path(pasta_data, "populacao_mun.json")

if (file.exists(caminho_populacao_rds)) {
  log_info("Lendo populacao_mun.rds...")
  populacao_mun <- readRDS(caminho_populacao_rds)
  
  log_info("Exportando populacao_mun.json...")
  jsonlite::write_json(populacao_mun, caminho_populacao_json, auto_unbox = TRUE)
  log_success(paste("Criado:", caminho_populacao_json))
} else {
  log_error(paste("Arquivo não encontrado:", caminho_populacao_rds))
}

# ------------------------------------------------------------------------------
# 5. Conversão: pernambuco_sf.rds -> mapa_pe.json (GeoJSON)
# ------------------------------------------------------------------------------
caminho_pe_sf_rds <- file.path(pasta_data, "pernambuco_sf.rds")
caminho_mapa_pe_json <- file.path(pasta_data, "mapa_pe.json")

if (file.exists(caminho_pe_sf_rds)) {
  log_info("Lendo pernambuco_sf.rds e gerando GeoJSON...")
  pe_sf <- readRDS(caminho_pe_sf_rds)
  geo_pe <- geojsonsf::sf_geojson(pe_sf)
  
  writeLines(geo_pe, caminho_mapa_pe_json)
  log_success(paste("Criado:", caminho_mapa_pe_json))
} else {
  log_error(paste("Arquivo não encontrado:", caminho_pe_sf_rds))
}

log_success("ETL de conversão concluído com sucesso!")
