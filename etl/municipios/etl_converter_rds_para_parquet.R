# etl/municipios/etl_converter_rds_para_parquet.R
# Script de conversão das bases legadas RDS para Parquet otimizado
# Aurora Municípios

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(jsonlite)
})

# paths ----
if (file.exists("R/paths.R")) source("R/paths.R") else if (file.exists("paths.R")) source("paths.R") else if (file.exists("../paths.R")) source("../paths.R") else if (file.exists("etl/paths.R")) source("etl/paths.R")

cat("Iniciando conversao e sincronizacao de dados para Parquet (Aurora Municipios)...\n")
dir_origem <- DIR_OUT_AURORA
dir_dest   <- file.path(DIR_OUT_AURORA, "parquet")
if (!dir.exists(dir_dest)) dir.create(dir_dest, recursive = TRUE)

# Helper para buscar arquivo em DIR_OUT ou DIR_RAW
find_rds <- function(nome) {
  p_out <- file.path(DIR_OUT_AURORA, nome)
  if (file.exists(p_out)) return(p_out)
  p_raw <- file.path(DIR_RAW_AURORA, nome)
  if (file.exists(p_raw)) return(p_raw)
  return(p_out)
}

# 1. Demografia
cat("1. Convertendo df_demografia_pe...\n")
p_demo <- find_rds("df_demografia_pe.rds")
if (file.exists(p_demo)) {
  demo <- readRDS(p_demo)
  if (inherits(demo, "sf")) demo <- sf::st_drop_geometry(demo)
  if ("ibge6" %in% names(demo)) demo$ibge6 <- as.character(demo$ibge6)
  if ("ibge7" %in% names(demo)) demo$ibge7 <- as.character(demo$ibge7)
  arrow::write_parquet(demo, file.path(dir_dest, "df_demografia_pe.parquet"))
  cat("   -> df_demografia_pe.parquet salvo com", nrow(demo), "linhas.\n")
}

# 2. Emendas
cat("2. Convertendo dados_emendas_atual...\n")
p_eme <- find_rds("dados_emendas_atual.rds")
if (file.exists(p_eme)) {
  eme <- readRDS(p_eme)
  if (inherits(eme, "sf")) eme <- sf::st_drop_geometry(eme)
  if ("ibge6" %in% names(eme)) eme$ibge6 <- as.character(eme$ibge6)
  if ("ibge7" %in% names(eme)) eme$ibge7 <- as.character(eme$ibge7)
  arrow::write_parquet(eme, file.path(dir_dest, "dados_emendas.parquet"))
  cat("   -> dados_emendas.parquet salvo com", nrow(eme), "linhas.\n")
}

# 3. PCP (Chapéu de Palha)
cat("3. Convertendo df_pcp...\n")
p_pcp <- find_rds("df_pcp.rds")
if (file.exists(p_pcp)) {
  pcp <- readRDS(p_pcp)
  if (inherits(pcp, "sf")) pcp <- sf::st_drop_geometry(pcp)
  if ("ibge6" %in% names(pcp)) pcp$ibge6 <- as.character(pcp$ibge6)
  if ("ibge7" %in% names(pcp)) pcp$ibge7 <- as.character(pcp$ibge7)
  if ("cod_ibge" %in% names(pcp)) pcp$cod_ibge <- as.character(pcp$cod_ibge)
  arrow::write_parquet(pcp, file.path(dir_dest, "df_pcp.parquet"))
  cat("   -> df_pcp.parquet salvo com", nrow(pcp), "linhas.\n")
}

# 4. Dados Fiscais com calculo de rank_pe
cat("4. Convertendo base_painel_mun com rank_pe...\n")
p_fisc <- find_rds("base_painel_mun.rds")
if (file.exists(p_fisc)) {
  fisc <- readRDS(p_fisc)
  if (is.list(fisc) && "dados" %in% names(fisc)) fisc <- fisc$dados
  if (inherits(fisc, "sf")) fisc <- sf::st_drop_geometry(fisc)

  fisc <- fisc %>%
    group_by(grupo, indicador, exercicio) %>%
    mutate(
      rank_pe = ifelse(is.na(valor), NA_integer_, min_rank(desc(valor)))
    ) %>%
    ungroup()

  if ("ibge6" %in% names(fisc)) fisc$ibge6 <- as.character(fisc$ibge6)
  if ("cod_ibge" %in% names(fisc)) fisc$cod_ibge <- as.character(fisc$cod_ibge)
  arrow::write_parquet(fisc, file.path(dir_dest, "dados_fiscais.parquet"))
  cat("   -> dados_fiscais.parquet salvo com", nrow(fisc), "linhas.\n")
}

# 5. RAIS Vocações
cat("5. Convertendo df_rais_vocacoes...\n")
p_voc <- find_rds("df_rais_vocacoes.rds")
if (file.exists(p_voc)) {
  voc <- readRDS(p_voc)
  if (inherits(voc, "sf")) voc <- sf::st_drop_geometry(voc)
  if ("id_municipio" %in% names(voc)) voc$id_municipio <- as.character(voc$id_municipio)
  arrow::write_parquet(voc, file.path(dir_dest, "df_rais_vocacoes.parquet"))
  cat("   -> df_rais_vocacoes.parquet salvo com", nrow(voc), "linhas.\n")
}

cat("ETL concluido com sucesso!\n")
